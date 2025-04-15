################################################################################
#                                                                              #
# 14-04-2025: BAC-estimator                                                    #
# Author: Jan Reiter Sørensen                                                  #
#                                                                              #
# An implementation of the BAC-estimator using mrcCholCov* as the pre-esti-    #
# mator. Relies on the highfrequency package for quick and efficient estima-   #
# tion the the mrcCholCov*. The BAC-estimator is implemented in the highfre-   #
# quency package, however, not with the mrcCholCov* as the pre-estimator.      #
#                                                                              #
################################################################################
library(magrittr)
library(highfrequency)
library(zoo)

##### Functions #####
# Some unbounded increasing sequence. Used as avering window for estimating the
# weights
l_n <- function(n, theta = 1) {
  round(sqrt(n) * theta, digits = 0)
}

# Takes the dataframe of data and adds a weights-column. Etf should not be in-
# cluded.
weights <- function(data, a) {
  symbols <- data$symb %>% unique
  
  n <- nrow(data)
  d <- length(symbols)
  window_size <- l_n(n / d, theta = 1 / 3)
  new_data <- data.frame()
  for(i in 1:length(symbols)) {
    data_temp <- data %>% filter(symb == symbols[i])
    
    # Find the number of observations for i'th stock
    nk <- data_temp %>% nrow
    weights_temp <- numeric(nk)
    
    # Use roll-apply. Last l_n entries does not calculate weight with all l_n
    # observations
    data_temp %<>% cbind(weight = 
      rollapply(data %>% filter(symb == symbols[i]) %$% vw,
               width = window_size,
               FUN = function(x) {
                 a[i] * exp(mean(x)) 
               },
               partial = TRUE,
               align = "left"))

    new_data %<>% rbind(data_temp) 
  }

  return(new_data)
}

# Estimates the W-matrix. Takes the dataframe with stock-data and should include
# the column og weights. Dataframe should not include the ETF
W <- function(data) {
  symbols <- data$symb %>% unique
  d <- length(symbols)
  W_mat <- matrix(nrow = d, ncol = d^2)
  
  # Estimate the vector of length d with the average weights
  mean_weights <- numeric(d)
  for(i in 1:d) {
    mean_weights[i] <- 
      data %>% 
      filter(symb == symbols[i]) %$% 
      weight %>% 
      mean 
  }
  
  # W is generated row-wise
  for(i in 1:d) {
    W_mat[i,] <- c(rep(0, (i - 1) * d),
                   mean_weights,
                   rep(0, (d - i) * d))
    
  }
  
  return(W_mat)
}

# Calculates the Q-matrix for restricting adjustment-matrix to be symmetric.
# Takes the number of stocks in the dataset excluding the etf
Q <- function(d) {
  Q_mat <- matrix(nrow = d^2, ncol = d^2)
  
  # Insert all rows. This does not take into account when i = j
  for(i in 1:d) {
    for(j in 1:d) {
      Q_mat[(i - 1) * d + j,] <- c(rep(0, (i - 1) * d + j - 1), 1, 
                                   rep(0, (d - i + 1) * d - j)) +
                                 c(rep(0, (j - 1) * d + i - 1), -1,
                                   rep(0, (d - j + 1) * d - i)) 
    }
  }
  
  # Now correct rows, where i = j
  for(i in 1:d) {
    Q_mat[(i - 1) * d + i,] <- rep(0, d^2) 
  }
  
  return(Q_mat)
}

# Calculates the L-matrix for estimating the adjustment matrix. Takes the data-
# frame with weights, the W-matrix, and the Q-matrix
L <- function(data, W_mat, Q_mat) {
  symbols <- data$symb %>% unique
  d <- length(symbols)
  
  # Calculate the sum of the product of inverted number of observations multi-
  # plied by sum of squared weights for each asset
  n_inv <- numeric(d)
  sum_sq_weight <- numeric(d)
  P <- numeric(d)
  for(i in 1:length(symbols)) {
    n_inv[i] <- nrow(data[data$symb == symbols[i],])^(-1)
    sum_sq_weight[i] <- sum(data[data$symb == symbols[i],]$weight^2)
  }
  P2 <- diag(nrow = d) * sum(n_inv * sum_sq_weight) # <- BAC-article and the
                                                    # documentation for the
                                                    # highfrequency-packages
                                                    # states that the size of
                                                    # the identity-matrix should
                                                    # be of size d^2, but this
                                                    # size, does not conform.
  P1 <- (diag(nrow = d^2) - 0.5 * Q_mat) %*% t(W_mat)
  P3 <- W_mat %*% Q_mat %*% t(W_mat) * 0.5
  
  return(P1 %*% solve(P2 - P3))
}

# Calculates the spot covariation between two assets. Takes a dataframe with the
# price-data for two stocks
spotCov <- function(data, t, k_name, kn) {
  symbols <- unique(data$symb)
  
  # If there is only one asset
  if(length(symbols) == 1) {
    nk <- nrow(data)
    
    # Generate the lists with xts-objects. One for ICov up to time t and one for
    # ICov up to time t + kn/nk
    pData_t1 <- list()
    pData_t2 <- list()
    subset_data_t1 <- data[data$t_unit <= t,]
    subset_data_t2 <- data[data$t_unit <= (t + kn / nk),]
    pData_t1[[k_name]] <- xts(x = subset_data_t1$vw,
                              order.by = subset_data_t1$t_POSIXct)
    pData_t2[[k_name]] <- xts(x = subset_data_t2$vw,
                              order.by = subset_data_t2$t_POSIXct)
    
    if(nrow(subset_data_t1) == 1) {
      return((nk / kn) * rCholCov(pData = pData_t2)$CholCov[1, 1])
    }
    return((nk / kn) * (rCholCov(pData = pData_t2)$CholCov[1, 1] - 
                          rCholCov(pData = pData_t1)$CholCov[1, 1]))
  }
  
  nk <- nrow(data[data$symb != k_name,]) - 1

  # If 1 - kn/nk < t <= 1 then set Sigma^(kl)_t = Sigma^(kl)_(1 - kn / nk)
  if(t > (1 - kn / nk)) {
    return(spotCov(data = data, t = (1 - kn/nk)))
  }
    
  # Generate the lists with xts-objects. One for ICov up to time t and one for
  # ICov up to time t + kn/nk
  pData_t1 <- list()
  pData_t2 <- list()
  subset_data_t1 <- data[data$t_unit <= t,]
  subset_data_t2 <- data[data$t_unit <= (t + kn / nk),]
  for(symbol in symbols) {
    pData_t1[[symbol]] <- xts(x = subset_data_t1[subset_data_t1$symb == symbol,]$vw,
                              order.by = subset_data_t1[subset_data_t1$symb == symbol,]$t_POSIXct)
    pData_t2[[symbol]] <- xts(x = subset_data_t2[subset_data_t2$symb == symbol,]$vw,
                              order.by = subset_data_t2[subset_data_t2$symb == symbol,]$t_POSIXct)
  }
  
  if(nrow(subset_data_t1) == length(symbols)) {
    return((nk / kn) * rCholCov(pData = pData_t2)$CholCov[1, 1])
  }
  return((nk / kn) * (rCholCov(pData = pData_t2)$CholCov[1, 2] - 
    rCholCov(pData = pData_t1)$CholCov[1, 2]))
}

# Estimates the stock-ETF covariation between a stock and an ETF based on the
# component stocks in the ETF. Hence, data should be a dataframe of the com-
# ponents and should exclude the ETF-data
beta_l <- function(data, l_name, kn) {
  symbols <- unique(data$symb)
  d <- length(symbols)
  temp_vec <- numeric(d)
  for(k in 1:d) {
    nk <- nrow(data[data$symb == symbols[k],]) - 1
    indices1 <- (0:(floor(nk / kn) - 1)) * kn + 1
    indices2 <- (1:floor(nk / kn)) * kn + 1
    temp_subset <- data[data$symb == symbols[k],]
    
    # Calculate the spot-covarianecs
    spotCovariances <- numeric(length(indices1))
    for(j in 1:length(indices1)) {
      spotCovariances[j] <- spotCov(data[data$symb == symbols[k] | 
                                           data$symb == l_name,],
                                    t = temp_subset$t_unit[indices1[j]],
                                    k_name = l_name,
                                    kn = kn) 
    }
    
    temp_vec[k] <-
      sum(temp_subset$weight[indices1] * spotCovariances * 
      (temp_subset$t_unit[indices2] - temp_subset$t_unit[indices1]))
  }
  
  return(sum(temp_vec))
}

# The BAC-estimator implemented with the mrcCholCov as the pre-estimator. Takes
# a cleaned dataset in data.frame format, and a numeric vector, a, correspon-
# ding to the amounts invested into each. Data should be filtered to be for
# one trading day.
BAC_mrcCholCov <- function(data, a, ETF_name = "ETF") {
  # First obtain the L-matrix
  weighted_data <- weights(data %>% filter(symb != ETF_name), a = a)
  symbols <- unique(data$symb)
  d <- length(symbols) - 1
  n <- nrow(weighted_data) - d
  kn <- floor(sqrt(n))
  W_matrix <- W(weighted_data)
  Q_matrix <- Q(W_matrix %>% nrow)
  L_matrix <- L(weighted_data, W_mat = W_matrix, Q_mat = Q_matrix)
  
  # Second, obtain the target beta. That is, the stock-ETF covariances according
  # to the pre-estimator (mrcCholCov)
  beta_target <- numeric(d)
  pData <- list()
  for(symbol in symbols) {
    pData[[symbol]] <- xts(x = data %>% filter(symb == symbol) %$% vw,
                           order.by = data %>% filter(symb == symbol) %$% 
                             t_POSIXct)
  }
  nonETF_symbols <- symbols[-which(symbols == ETF_name)]
  for(i in 1:d) {
    temp_pData <- list()
    temp_pData[[nonETF_symbols[i]]] <- pData[[nonETF_symbols[i]]]
    temp_pData$ETF <- pData[[ETF_name]]
    CholCov_mat <- rCholCov(pData = temp_pData)$CholCov
    beta_target[i] <- CholCov_mat[1, 2]
  }
  
  # Calculate the estimated betas
  beta_estimated <- numeric(d)
  for(i in 1:d) {
    beta_estimated[i] <- beta_l(weighted_data, l_name = nonETF_symbols[i], 
                                kn = kn) 
  }
  
  # Calculate the adjustment-matrix
  adj_mat <- matrix(L_matrix %*% matrix(beta_estimated - beta_target, ncol = 1), 
                    ncol = d)
  preEstimate <- rCholCov(pData = pData[nonETF_symbols])$CholCov
  
  # According to footnote 4, in the BAC-article, when the result is not psd, we
  # perform a projection onto the psd-space by factorizing according to a spec-
  # tral decomposition, and setting eigen-values to zero
  Theta_k <- preEstimate[nonETF_symbols, nonETF_symbols] - adj_mat
  
  omega_E <- rCholCov(pData = pData[[ETF_name]])$CholCov
  omega_EK <- beta_target
  
  Omega_p <- matrix(nrow = nrow(Theta_k) + 1, ncol = ncol(Theta_k) + 1)
  Omega_p[1, 1] <- omega_E
  Omega_p[1:length(omega_EK) + 1, 1] <- omega_EK
  Omega_p[1, 1:length(omega_EK) + 1] <- omega_EK
  Omega_p[2:nrow(Omega_p), 2:ncol(Omega_p)] <- Theta_k
  
  eigen_decomp <- eigen(Omega_p)
  eigen_decomp$values[which(eigen_decomp$values < 0)] <- 1e-10
  Omega_p <- eigen_decomp$vectors %*% diag(eigen_decomp$values) %*%
    t(eigen_decomp$vectors)
  
  return(Omega_p)
}