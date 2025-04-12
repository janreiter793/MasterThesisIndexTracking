################################################################################
#                                                                              #
# 11-04-2025: mrcCholCov* estimator                                            #
# - Author: Jan Reiter Sørensen                                                #
#                                                                              #
# An implementation of the mrcCholCov* estimator propsed by Boudt et al (2016) #
# Positive semidefinite integrated covariance estimation, factorizations and   #
# asynchronicity. Utilizes the bias-adjusted mrc-estimator with projection     #
# into the positive semidefinite space proposed by K. Christensen et al (2010) #
# and Fan et al (2012).                                                        #
#                                                                              #
################################################################################
library(magrittr)
source("Pre-averaged_Bias-adjusted_MRC(Christensen et al 2010).R")
source("refresh-time_sampler.R")

##### Functions #####
# Takes a matrix, where each column is a log-price process for a stock. Returns
# the differences for the stocks
returns <- function(V) {
  diff(V)
}

# Takes a matrix with l columns, and l coefficient estimates for the entries in
# H, say h. Calculates the factors and h-coefficients. Returns a list with the
# factors-matrix and the vector of h-coefficients
calc_factors <- function(V_returns, h = c()) {
  l <- ncol(V_returns)
  h_coefs <- numeric(l - 1)
  if(length(h)) { h_coefs[1:length(h)] <- h }
  factors <- matrix(nrow = nrow(V_returns), ncol = l)
  
  for(m in (length(h) + 1):l) {
    if(m == 1) {
      factors[, m] <- V_returns[, m] 
    } else {
      factors[, m] <- V_returns[, m] - h_coefs[m - 1] * 
        rowSums(matrix(V_returns[, 1:(m - 1)], ncol = (m - 1)))
    }
    
    if(m < l) {
      h_coefs[m] <- estimate_h(V_return = V_returns[, m + 1],
                               V_factor = factors[, m])
    }
  }
  
  return(list(factors = factors, h_coefs = h_coefs))
}

# Takes a vector of factors and estimates the g-coefficient as the integrated
# variance of V_factor using the biased-adjusted MRC-estimator
estimate_g <- function(V_factor) {
   temp_g <- MRC(matrix(V_factor, ncol = 1), project_psd = FALSE)
   diag(temp_g)[diag(temp_g) < 0] <- (1e-8) # If the estimate is negative, set
                                            # to 10^-8 for stability
   return(temp_g)
}

# Takes a vector of returns and a vector of factors, and then estimates the
# realized beta as the covariance divided by the variance of V_factor. Uses
# the biased-adjusted MRC-estimator
estimate_h <- function(V_return, V_factor) {
  sigma <- MRC(matrix(c(V_return, V_factor), ncol = 2),
               project_psd = TRUE)
  res <- sigma[1,2] / sigma[2, 2]
  if(is.na(res) | abs(sigma[2, 2]) < 1e-10) { return(0) }
  return(res)
}

# Takes a dataframe of refresh-sampled log-prices and returns a matrix with the
# log-price processes as columns. Dataframe should have a column, specifying the
# order of the price-processes
df_logpricedata_to_matrix <- function(data) {
  liquidities <- data$liquidity %>% unique %>% sort
  length_liquidities <- length(liquidities)
  
  # Make an empty matrix to have the price processes
  res <- matrix(ncol = length_liquidities,
                nrow = nrow(data) / length_liquidities)
  
  # Fill in the rows of res
  for(l in 1:length_liquidities) {
    res[, l] <- 
      data %>%
      filter(liquidity == liquidities[l]) %$%
      vw
  }
  
  return(res)
}

# Takes a dataframe with cleaned log-price processes and unit times for a spe-
# cific day, then adds a column with numbers 1,2,... specifying the order of
# liquidity, with 1 being most liquid, 2 next most liquid and so on.
squaredDurationOrder <- function(data) {
  symbols <- data$symb %>% unique
  squared_durations <- symbols %>% length %>% numeric

  # Calculate the sum of squared durations for each process
  for(i in 1:length(symbols)) {
    squared_durations[i] <- sum((data %>% 
                                filter(symb == symbols[i]) %$% 
                                t_unit %>% 
                                diff)^2)
  }
  
  # Rank the durations
  ranked_sq_durations <- squared_durations %>% rank
  
  # Put in the order of liquidity in a clumn in a new dataframe
  new_data <- data.frame()
  for(i in 1:length(symbols)) {
    new_data %<>% rbind(  
      data %>% 
      filter(symb == symbols[i]) %>% 
      mutate(liquidity = ranked_sq_durations[i]))
  }
  
  return(new_data)
}

# The mrcCholCov estimator. Takes the cleaned data frame of log-prices and unit
# times. Data should be filtered to only be for one specific day.
mrcCholCov <- function(data) {
  # First add an ordering by liquidity. Calculate number of assets
  data %<>% squaredDurationOrder
  d <- data$symb %>% unique %>% length
  
  # Define the matrices we will estimate
  H <- diag(rep(1, d))
  G <- diag(rep(1, d))
  
  # Estimate the entries according the algorithm 1. First estimate g_1,1
  ret <-
    data %>% 
    filter(liquidity == 1) %$%
    vw %>% 
    matrix(ncol = 1) %>% 
    returns
  factors <- ret %>% matrix(ncol = 1)
  G[1,1] <- factors[,1] %>% estimate_g
  
  for(iter in 2:d) {
    for(iter_2 in 1:(iter - 1)) {
      cat("At row ", iter, ", and column ", iter_2, "\n")
      # First synchronize data for assets (1,2,...,iter_2,iter) with refresh-
      # time sampling
      sync_data <-
        data %>% 
        filter(liquidity %in% c(1:iter_2, iter)) %>% 
        refreshTimes
      
      # Calculate the matrix of returns
      ret <- 
        sync_data %>% 
        df_logpricedata_to_matrix %>%
        returns
      
      # Estimate the H_(iter, iter_2) entry in H
      if(iter_2 == 1) {
        factors_h <- calc_factors(V_returns = ret)
      } else {
        factors_h <- calc_factors(V_returns = ret,
                                  h = H[iter, 1:(iter_2 - 1)])
      }
      
      H[iter, iter_2] <- factors_h$h_coefs[iter_2]
    }
    cat("At row ", iter, ", and column ", iter, "\n")
    G[iter, iter] <- estimate_g(factors_h$factors[, iter])
  }
  
  # First calculate HGH^T
  res <- H %*% G %*% t(H)
  
  # Then calculate the adjustment matrix D
  diag_res <- diag(res)
  diag_adj <- numeric(d)
  for (iter in 1:d) {
    diag_adj[iter] <- 
      data %>%
      filter(liquidity == iter) %$%
      vw %>% 
      estimate_g
  }
  adjustment <- sqrt(diag_adj / diag_res)
  adjustment[diag_res < 1e-10] <- 1
  
  # Multiply with the adjustment matrix, Sigma = D^1/2 * H * G * H^t * D^1/2
  res <- diag(adjustment) %*% res %*% diag(adjustment)
  
  return(res)
}