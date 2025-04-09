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
    
    # Create a matrix where each column is the unit times for each stock
    # number of rows is the length of the shortest list of unit times
    l_min <- min_length(unit_times)
    adjusted_vectors <- lapply(unit_times, function(x) {
          x[1:l_min]
        })
    unit_times_matrix <- do.call(cbind, adjusted_vectors)
    
    # Find the refresh-times. This approach is wrong, see Multivariate Realized
    # Kernels... by Barndorf-Nielsen (2008)
    refresh_times <- numeric(l_min)
    refresh_times[1] <- max(unit_times_matrix[1,])
    unit_times_matrix <- 
      unit_times_matrix[apply(unit_times_matrix, 1, 
                        function(row) all(row > refresh_times[1])), ]
    i <- 2
    while(!is.null(nrow(unit_times_matrix))) {
      refresh_times[i] <- max(unit_times_matrix[1,])
      unit_times_matrix <- 
        unit_times_matrix[apply(unit_times_matrix, 1, 
                                function(row) all(row > refresh_times[i])), ]
      i <- i + 1
    }
    
    if(length(unit_times_matrix) > 0) {
      refresh_times[i] <- max(unit_times_matrix) 
    }
    
    # Omit zero-entries
    refresh_times <- refresh_times[1:tail(which(refresh_times != 0), 1)]
  }
}
