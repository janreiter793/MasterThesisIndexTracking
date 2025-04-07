################################################################################
#                                                                              #
# 07-04-2025: Refresh-time sampler                                             #
# - Author: Jan Reiter Sørensen                                                #
#                                                                              #
# An implementation of the refresh-time sampling mechanism. Functionality      #
# takes a dataframe of stock prices and unit times, and then returns a data-   #
# frame with stock data and refresh-times. To fill in observations on refresh- #
# times with no observation, the last known observation is used.               #
#                                                                              #
################################################################################
library(magrittr)
library(dplyr)
library(lubridate)

##### Functions #####
# Takes the whole dataframe of stock prices, and turns time column into unit
# time for each day. That is, for each day, the time points fall in the interval
# [0, 1], where 0 is market open (09:30:00 EST) and 1 is market close (16:00:00 EST).
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

# Takes a list of vectors and returns the length of the shortest vector
min_length <- function(l) {
  l %>% 
    lapply(FUN = length) %>% 
    unlist %>% 
    unname %>% 
    min %>% 
    return
}

# Takes a dataset of stock data with unit times, and then returns a new 
# dataset, where observations are refresh-time sampled
refreshTimes <- function(data) {
  # Find the unique trading days
  days <- data$day %>% unique
  for(d in days) {
    temp <- 
      data %>% 
      filter(day == d)
  }
}