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

##### Functions #####
# Takes the whole dataframe of stock prices, and turns time column into unit
# time for each day. That is, for each day, the time points fall in the interval
# [0, 1], where 0 is market open and 1 is market close.
msToUnitTime <- function(data) {
  # Find each ticker symbol
  symbols <- data$symb %>% unique
  
  new_data <- data.frame()
  for(symbol in symbols) { # Loop through each symbol
    temp <- data %>% filter(symb == symbol) # Find subset of symbol
    days <- temp$day %>% unique # Find all the unqiue days
    for(d in days) { # For each day the time column is turned to unit time
      temp_2 <- temp %>% filter(day == d)
      temp_2$t <- temp_2$t - min(temp_2$t)
      temp_2$t <- temp_2$t / max(temp_2$t)
      new_data %<>% rbind(temp_2) # Append to new data set
    }
  }
  
  return(new_data)
}