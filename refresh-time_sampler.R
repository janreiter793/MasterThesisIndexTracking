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
  new_data <- data.frame()
  
  # Find the unique trading days
  days <- data$day %>% unique
  for(d in days) {
    # Subset data for the same day
    temp <- 
      data %>% 
      filter(day == d)
    
    # Extract the unit times for each stock, put together in a list
    symbols <- temp$symb %>% unique
    unit_times <- list()
    for(symbol in symbols) {
       unit_times[[symbol]] <- temp %>% filter(symb == symbol) %$% t_unit
    }
    
    # Find the refresh times. First find the initial refresh time
    refresh_times <- unit_times %>% min_length %>% numeric
    num_stocks <- symbols %>% length
    tau_cand <- numeric(num_stocks)
    refresh_times[1] <- 
      unit_times %>% 
      lapply(FUN = function(x) {x[1]}) %>% 
      unlist %>% 
      max
    for(i in 2:min_length(unit_times)) {
      times <-  
        unit_times %>% 
        lapply(FUN = function(x) { 
          x[-which(x <= refresh_times[i - 1])][1] 
          }) %>% 
        unlist
      
      if(any(is.na(times))) {
        break 
      }
      
      refresh_times[i] <- times %>% max
    }
    
    # Omit zero-entries
    refresh_times <- refresh_times[1:tail(which(refresh_times != 0), 1)]
    
    # Now that refresh-times are found for day d across all stock, we need to
    # fill observations at each time with most recent observation to each re-
    # fresh-time
    for(symbol in symbols) {
      temp_2 <- temp %>% filter(symb == symbol)
      for(refresh_time in refresh_times) {
        new_data %<>% 
          rbind(   
            temp_2 %>% 
            filter(t_unit <= refresh_time) %>% 
            tail(n = 1) %>%
            mutate(t_unit = refresh_time)
          )
      }
    }
  }
  
  return(new_data)
}
