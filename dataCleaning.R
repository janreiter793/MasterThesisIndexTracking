################################################################################
#                                                                              #
# 09-04-2025: Data cleaning                                                    #
# Author: Jan Reiter Sørensen                                                  #
#                                                                              #
# Cleans trade data according to the procedure of Realized kernels in practice #
# by O. E. Barndorff-Nielsen, P. Reinhard Hansen, A. Lunde, and N. Shephard.   #
# (2009)                                                                       #
#                                                                              #
# Data from polygon.io is relatively clean, and I have therefore only implemen-#
# ted the parts of the cleaning procedure that seem relevant.                  #
#                                                                              #
################################################################################
library(magrittr)
library(dplyr)
library(lubridate)
library(zoo)

##### Funcitons #####
# Takes the whole dataframe of stock prices, and turns time column into unit
# time for each day. That is, for each day, the time points fall in the interval
# [0, 1], where 0 is market open (09:30:00 EST) and 1 is market close (16:00:00 
# EST). This is P1 of the cleaning process
msToUnitTime <- function(data) {
  market_duration_ms <- 6 * 60 * 60 * 1000 + 30 * 60 * 1000
  
  data %>%
    mutate(
      # Convert milisecond time stamp to date and time
      t_POSIXct = as.POSIXct(t / 1000, origin = "1970-01-01", tz = "EST"),
      
      # Find the date
      date = as.Date(t_POSIXct, tz = "EST"),
      
      # Generate POSIXct objects specifying exact open and close times
      market_open = as.POSIXct(paste0(date, " 09:30:00"), tz = "EST"),
      market_close = as.POSIXct(paste0(date, " 16:00:00"), tz = "EST"),
      
      # Specify rows that are within trading hours
      in_market_hours = t_POSIXct > market_open & t_POSIXct < market_close,
      
      # Calculate unit time by subtract market-open milisecond time from trade
      # time in ms. Then divide by market open duration in ms
      t_unit = (as.numeric(t_POSIXct) * 1000 - as.numeric(market_open) * 1000) / market_duration_ms
    ) %>%
    # Filter out trades that are not within trading hours
    filter(in_market_hours) %>%
    
    # Remove unnesecary columns
    select(-market_open, -market_close, -in_market_hours) %>% 
    return
}

# Remove zero-trades (P2)
rmZeroTrades <- function(data) {
  data %>% 
    filter(vw != 0)
}

# Rolling MAD function. Centers a vector of numeric values, and calculate
rolling_mad <- function(vec) {
  mean(abs(vec[-26] - mean(vec[-26])))
}

# Modified mean function. Excludes center value of vector in the calculation of
# the mean
mean_mod <- function(vec) {
  mean(vec[-26])
}

# Delete entries where the volume weighted price is more than 10 absolute 
# deviations away from rolling mean of 25 observations before and after the
# observation in consideration (Q4)
rmOutliers <- function(data) {
  window_size <- 51
  
  # Go through each stock for each day
  symbols <- data$symb %>% unique
  new_data <- data.frame()
  for(symbol in symbols) {
    days <- data %>% filter(symb == symbol) %$% day %>% unique
    for(d in days) {
      temp <- data %>% filter(symb == symbol, day == d)
      
      # Find the absolute devations for each entry in the vw vector
      absolute_deviations <- rollapply(temp$vw,
                                       width = window_size,
                                       FUN = rolling_mad,
                                       align = "center",
                                       partial = TRUE)
      
      # Find the rolling mean for the vw
      rolling_mean <- rollapply(temp$vw,
                                width = window_size,
                                FUN = mean_mod,
                                align = "center",
                                partial = TRUE)
      
      # Calculate the number of absolute devations for each observation
      temp$absDeviations <- abs((temp$vw - rolling_mean) / absolute_deviations)
      
      # Omit the entries where vw is at least 10 absolute devations from the
      # rolling mean
      new_data %<>% 
        rbind(temp %>% 
                filter(absDeviations < 10) %>% 
                select(-absDeviations))
    }
  }
  
  return(new_data)
}

# Takes the raw polygon.io-dataset and applies the cleaning procedure P1, P2,
# and Q4. In addition unit times are included
cleanRawData <- function(data) {
  data %>% msToUnitTime %>% rmZeroTrades %>% rmOutliers
}
