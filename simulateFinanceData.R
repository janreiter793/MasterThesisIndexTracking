################################################################################
#                                                                              #
# 15-04-2025: Simulate finance data                                            #
# Author: Jan Reiter Sørensen                                                  #
#                                                                              #
# Functionality for generating finance data to use for MC-simulation of the    #
# covariance estimators.                                                       #
#                                                                              #
################################################################################
library(magrittr)
library(gtools)
library(highfrequency)
library(zoo)
library(xts)
library(tidyverse)
set.seed(100)

##### Functions #####
# Generate unit times according to a poisson distribution. Takes a parameter
# lambda which specifies the expected number of observations
generateUnitTimes <- function(lambda) {
  num <- rpois(n = 1, lambda = lambda)
  times <- cumsum(rexp(lambda, lambda))
  times[times <= 1]
}

# Takes a vector of diff-times and returns a vector of BM increments 
simulateBMIncrements <- function(Delta_t) {
  rnorm(n = length(Delta_t), sd = Delta_t)
}

# Takes a vector observation times, the parameters, theta, mu, delta, and the
# initial value y0. Returns a vector, same length as times, with observations
# of an OU process
simulateOUProcess <- function(times, theta, mu, sigma, y0 = 1) {
  # Obtain Delta t and Delta W
  Delta_t <- diff(times)
  Delta_W <- simulateBMIncrements(Delta_t = Delta_t)
  
  # Fill in the Y-vector using for-loop. Use of cumsum is not possible due to
  # the dependence of Y_n-1 for y_n.
  n <- length(times)
  Y <- numeric(n)
  Y[1] <- y0
  for(i in 2:n) {
    Y[i] <- theta * (mu - Y[i - 1]) * Delta_t[i - 1] + sigma * Delta_W[i - 1]
  }
  
  return(Y)
}

# Simulate the semimartingale processes for finance data according to the
# Barndorff-Nielsen article Multivariate realised kernels. Takes a vector
# liquidity specifying the individual level of liquidity for each stock, that
# is, the expected number of observation times for a day. The vectors with
# parameters, mu, beta0, beta1, alpha, and rho. Returns a dataframe with unit
# times, log-prices, symbols, POSITXct. Also adds etf-prices.
simulateSemiMart <- function(liquidities, mu, beta0, beta1, alpha, rho,
                             day, symb = NULL, add.noise = T, 
                             noise.to.signal.ratio = 0.01,
                             ETF_name = "ETF", etf_liquidity, a = NULL) {
  d <-length(liquidities)
  
  # If weights are not specified then assume equal weight for stocks
  if(is.null(a)) {
    a <- rdirichlet(n = 1, alpha = rep(1 / d, d))
  }
  a <- matrix(a, ncol = 1)
  
  # First obtain the simulation times
  observation_times <- list()
  for(i in 1:d) {
    observation_times[[i]] <- generateUnitTimes(lambda = liquidities[i])
  }
  
  # Now generate the increments of the common Brownian motion
  times <- sort(unique(unlist(observation_times)))
  increments <- simulateBMIncrements(diff(times))
  common_BM <- data.frame(
    times = times[-length(times)],
    increments = increments
    )
  
  # Simulate each asset and store in a dataframe.
  # First allocate memory
  temp_matrix <- matrix(nrow = length(unlist(observation_times)),
                        ncol = 5)
  colnames(temp_matrix) <- c("vw", "day", "symb", "t_unit", "t_POSIXct")
  new_data <- as.data.frame(temp_matrix)
  
  # We only simulate for one day. So set all day entries to specified day
  new_data$day <- day
  new_data$t_unit <- unlist(observation_times) # Put in observation times
  
  # If a specific set of symbol names are given, put them in. Otherwise set
  # symbol names to "1", "2", ..., "d"
  if(is.null(symb)) {
    new_data$symb <- as.character(rep(1:d, 
                                      sapply(observation_times, FUN = length)))
  } else {
    new_data$symb <- as.character(rep(symb,
                                      sapply(observation_times, FUN = length))) 
  }
  
  # Calculate the vw, that is, simulate prices. We also want to store the pure
  # log-prices to estimate etf-prices
  y0_OU <- rnorm(d, (-2 * alpha))
  etf_obs_times <- generateUnitTimes(etf_liquidity)
  etf_stockprice_matrix <- matrix(ncol = d, nrow = length(etf_obs_times))
  for(i in 1:d) {
    Delta_t <- diff(observation_times[[i]])
    
    # First obtain the BM movements
    BM_increments <- 
      simulateBMIncrements(Delta_t = Delta_t)
    
    # Simulate the OU-process for obtaining sigma
    OU_process <- simulateOUProcess(observation_times[[i]], 
                                    theta = -alpha[i],
                                    mu = 0,
                                    sigma = 1,
                                    y0 = y0_OU[i])
    sigma <- exp(beta0[i] + beta1[i] * OU_process) # Calculate sigma
    
    # Calculate the prices using mu as the initial value
    vw <- cumsum(c(rnorm(n = 1), 
                   mu[i] * Delta_t + 
                     rho[i] * sigma[-length(sigma)] * BM_increments +
                     sqrt(1 - rho[i]^2) * sigma[-length(sigma)] * 
                      common_BM[common_BM$times %in% 
                                  observation_times[[i]][-length(observation_times[[i]])], 
                                "increments"]))
    
    for(j in 1:length(etf_obs_times)) {
      # Take the last observed price before observation time
      last_observed_price <- rev(
        vw[which(observation_times[[i]] <= etf_obs_times[j])])[1]
      
      # If there was no observation time, then use first observation
      if(is.na(last_observed_price)) {
        last_observed_price <- vw[1]
      }
      
      etf_stockprice_matrix[j, i] <- last_observed_price
    }
    
    # Finally add noise to the price-observations
    if(add.noise) {
      N <- length(vw)
      U <- rnorm(n = N,
                 sd = sqrt(
                   noise.to.signal.ratio^2 * 
                     sqrt(mean(sigma^4 * (1:N) / N))
                 ))
      vw <- vw + U
    }
    
    # Insert into the dataset
    new_data[new_data$symb == as.character(i), "vw"] <- vw
  }
  
  # Insert the POSIXct-time
  timestamp <- as.POSIXct("2024-04-01 09:30:00", 
                          format = "%Y-%m-%d %H:%M:%S", 
                          tz = "EST")
  new_data$t_POSIXct <- timestamp + new_data$t_unit * 390 * 60 +
    day * 60 * 60 * 24
  
  # Obtain the log-etf prices
  etf_prices <- log(exp(etf_stockprice_matrix) %*% a)
  etf_POSIXct_times <- as.POSIXct("2024-04-01 09:30:00",
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "EST")
  etf_POSIXct_times <- etf_POSIXct_times + etf_obs_times * 390 * 60 +
    day * 60 * 60 * 24
  pData <- xts(x = exp(etf_prices),
               order.by = etf_POSIXct_times)
  etf_IV <- rCholCov(pData = pData)$CholCov
  etf_noise <- rnorm(n = length(etf_obs_times),
                     sd = noise.to.signal.ratio * sqrt(etf_IV))
  
  # Create a dataframe with the etf-data in it
  etf_data <- matrix(nrow = length(etf_obs_times),
                     ncol = 5)
  colnames(etf_data) <- c("vw", "day", "symb", "t_unit", "t_POSIXct")
  etf_data <- as.data.frame(etf_data)
  etf_data$vw <- etf_prices + etf_noise
  etf_data$day <- day
  etf_data$symb <- ETF_name
  etf_data$t_unit <- etf_obs_times
  etf_data$t_POSIXct <- etf_POSIXct_times
  
  new_data <- rbind(etf_data, new_data)
  
  # Now stock data is complete
  return(list(data = new_data, a = a))
}
