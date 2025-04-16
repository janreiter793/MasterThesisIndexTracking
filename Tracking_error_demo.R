################################################################################
#                                                                              #
# 15-04-2025: Tracking error estimations of the demo data                      #
# Author: Jan Reiter Sørensen                                                  #
#                                                                              #
# A small program that assesses the tracking error for the demo-dataset.       #
#                                                                              #
################################################################################
library(magrittr)
library(highfrequency)
source("BACestimator.R")
source("refresh-time_sampler.R")
source("mrcCholCov.R")

##### Functions #####
# Takes an alpha and an Omega, and calculates the tracking error
tracking_error <- function(alpha, Omega) {
  t(matrix(1, nrow = 5) - alpha) %*% Omega %*% (matrix(1, nrow = 5) - alpha)
}

# Takes a dataframe of stockprices and generetes the block-matrix Omega
CholCov_omega <- function(data) {
  symbols <- unique(data$symb)
  pData <- list()
  for(symbol in symbols) {
    pData[[symbol]] <- xts(x = data %>% filter(symb == symbol) %$% vw,
                           order.by = data %>% filter(symb == symbol) %$% 
                             t_POSIXct)
  }
  temp_CholCov <- rCholCov(pData = pData)$CholCov
  temp_CholCov[symbols, symbols]
}

# Estimate alpha
estimate_alpha <- function(Omega) {
  matrix(c(0, solve(Omega[2:nrow(Omega), 2:ncol(Omega)]) %*% 
             Omega[2:nrow(Omega), 1]), ncol = 1)
}

##### Program execution #####
market_caps <- c(573021105812.12,
                 563682458609.2799,
                 446295293600,
                 296056770627.52)

# Obtain the tracking error
days <- unique(cleaned_data$day)
Tracking_errors_BAC <- numeric(length(days) - 1)
Tracking_errors_CholCov <- numeric(length(days) - 1)
for(i in 0:(length(days) - 2)) {
  BAC_estimate <- BAC_mrcCholCov(cleaned_data %>% filter(day == days[i + 1]), 
                                 market_caps / sum(market_caps), 
                                 ETF_name = "XLF")
  BAC_estimatep <- BAC_mrcCholCov(cleaned_data %>% filter(day == days[i + 2]), 
                                  market_caps / sum(market_caps), 
                                  ETF_name = "XLF")
  CholCov_estimate <- CholCov_omega(cleaned_data %>% filter(day == days[i + 1]))
  CholCov_estimatep <- CholCov_omega(cleaned_data %>% filter(day == days[i + 2]))
  
  alpha_BAC <- estimate_alpha(BAC_estimate)
  alpha_CholCov <- estimate_alpha(CholCov_estimate)
  
  Tracking_errors_BAC[i + 1] <- tracking_error(alpha_BAC, BAC_estimatep)
  Tracking_errors_CholCov[i + 1] <- tracking_error(alpha_CholCov, CholCov_estimatep)
}

mean(Tracking_errors_BAC < Tracking_errors_CholCov)

old_par <- par(no.readonly = TRUE)

# Set larger bottom margin (bottom, left, top, right)
par(mar = c(7, 4, 4, 2))  # default is c(5, 4, 4, 2)
Tracking_errors_CholCov %>% plot(type = "l", ylim = c(0, max(Tracking_errors_CholCov)), panel.first = grid(),
                                 xaxt = "n", xlab = "", ylab = "Tracking error")
lines(Tracking_errors_BAC, col = "red")
legend("topright", legend = c("CholCov", "BAC"), col = c("black", "red"), lty = 1)
dates <- (as.Date("2024-04-01") + days)[-1]
axis(1, at = seq_along(dates), labels = dates, las = 2)
par(old_par)

png(filename = "XLF_movements_demo.png", width = 1500, height = 700)
  par(mfrow = c(1, 2))
  # How does it look it I replicate etf with just the four stocks an market-cap
  # weights
  vw_mat <- cleaned_data %>% filter(day == 0, symb != "XLF") %>% squaredDurationOrder %>%
    refreshTimes %>% df_logpricedata_to_matrix
  weighted <- market_caps / sum(market_caps) %>% matrix(ncol = 1)
  estimated_etf_prices <- vw_mat %*% weighted
  ETF_prices <- cleaned_data[cleaned_data$day == 0 & 
                               cleaned_data$symb == "XLF", "vw"]
  plot(ETF_prices %>% scale, type = "l", panel.first = grid(),
       ylim = c(min(c(estimated_etf_prices %>% scale, ETF_prices %>% scale)), 
                max(c(estimated_etf_prices %>% scale, ETF_prices %>% scale))),
       xlab = "Minutes from market open (9:30:00 EST)",
       ylab = "Centralized log-prices",
       main = "XLF movements for 2024-04-01")
  lines(estimated_etf_prices %>% scale, col = "red")
  legend("topright", 
         legend = c("XLF log-prices", 
                    "Market-cap weighted XLF log-prices", 
                    "Index log-prices"),
         col = c("black", "red", "green"),
         lty = 1)
  
  # Replicated using the estimated alpha for that day
  Omega_BAC <- BAC_mrcCholCov(cleaned_data %>% filter(day == 0),
                              market_caps / sum(market_caps),
                              ETF_name = "XLF")
  alpha <- estimate_alpha(Omega_BAC)[-1] %>% matrix(ncol = 1)
  estimated_etf_prices <- vw_mat %*% alpha
  ETF_prices <- cleaned_data[cleaned_data$day == 0 & 
                               cleaned_data$symb == "XLF", "vw"]
  lines(estimated_etf_prices %>% scale, col = "green")
  
  # Using that alpha for tracking the day after
  vw_mat <- cleaned_data %>% filter(day == 1, symb != "XLF") %>% squaredDurationOrder %>%
    refreshTimes %>% df_logpricedata_to_matrix
  estimated_etf_prices <- vw_mat %*% alpha
  ETF_prices <- cleaned_data[cleaned_data$day == 1 & 
                               cleaned_data$symb == "XLF", "vw"]
  plot(ETF_prices %>% scale, type = "l", panel.first = grid(),
       ylim = c(min(c(estimated_etf_prices %>% scale, ETF_prices %>% scale)), 
                max(c(estimated_etf_prices %>% scale, ETF_prices %>% scale))),
       xlab = "Minutes from market open (9:30:00 EST)",
       ylab = "Centralized log-prices",
       main = "XLF movements for 2024-04-02")
  lines(estimated_etf_prices %>% scale, col = "green")
  
  # How about the market-cap weighted price estimate
  estimated_etf_prices <- vw_mat %*% weighted
  lines(estimated_etf_prices %>% scale, col = "red")
  legend("topright", 
         legend = c("XLF log-prices", 
                    "Market-cap weighted XLF log-prices", 
                    "Index log-prices with previous day alpha"),
         col = c("black", "red", "green"),
         lty = 1)
dev.off()
par(mfrow = c(1, 1))

# Try to calculate excess returns for next-days tracking error
mean_excess_returns <- numeric(length(days) - 1)
for(i in 1:(length(days) - 1)) {
  vw_mat <- cleaned_data %>% filter(day == days[i + 1], symb != "XLF") %>% 
    squaredDurationOrder %>% refreshTimes %>% df_logpricedata_to_matrix
  Omega_BAC <- BAC_mrcCholCov(cleaned_data %>% filter(day == days[i]),
                              market_caps / sum(market_caps),
                              ETF_name = "XLF")
  alpha <- estimate_alpha(Omega_BAC)[-1] %>% matrix(ncol = 1)
  estimated_etf_prices <- vw_mat %*% alpha
  ETF_prices <- cleaned_data[cleaned_data$day == days[i + 1] &
                              cleaned_data$symb == "XLF",
                             "vw"]
  mean_excess_returns[i] <- mean(abs(ETF_prices[1:length(estimated_etf_prices)] - estimated_etf_prices))
  
}
par(mar = c(7, 4, 4, 2))  # default is c(5, 4, 4, 2)
(100 * Tracking_errors_BAC / mean_excess_returns) %>%
  abs %>% 
  plot(type = "l", panel.first = grid(),
       ylab = "Tracking error [%]", xlab = "",
       xaxt = "n")
dates <- (as.Date("2024-04-01") + days)[-1]
axis(1, at = seq_along(dates), labels = dates, las = 2)
par(old_par)

