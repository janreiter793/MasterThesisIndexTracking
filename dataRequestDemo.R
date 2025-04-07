################################################################################
#                                                                              #
# 07-04-2025: Data fetch demo                                                  #
# - Author: Jan Reiter Sørensen                                                #
#                                                                              #
# Downloads stock data with minute resolution for XLF, BRK-B, JPM, V, MA, and  #
# BAC in the time interval from 2024-04-01 to 2025-04-15. Obtains the market   #
# cap for stocks. Works for the free subscription to polygon.io                #
#                                                                              #
################################################################################
library(jsonlite)
library(magrittr)

###### Constants #####
TEST_RUN <- FALSE # If true, then program will execute when running script. When
                  # false, functions will just be loaded.
API_KEY <- "" # API key for polygon.io, for demo it does not require subscription.
WD <- "" # Directory to store demo data

##### Functions #####
# Takes key, from-date, to-date, resolution, multiplier, and stock-ticker, and 
# then returns a string with the api-request for polygon.io
generateAPIRequest <- function(key = NULL, from = NULL, to = NULL, 
                               resolution = "minute", 
                               multiplier = 1, ticker = NULL) {
  # Check parameters
  if(any(is.null(key), is.null(from), is.null(to), is.null(ticker))) {
    print("Missing parameter(s):")
    if(is.null(key)) {
      print(" - key")
    }
    if(is.null(from)) {
      print(" - from") 
    }
    if(is.null(to)) {
      print(" - to")
    }
    if(is.null(ticker)) {
      print(" - ticker") 
    }
    return("")
  }
  
  # Assemble the request string
  request <- paste0("https://api.polygon.io/v2/aggs/ticker/",
                    ticker,
                    "/range/",
                    multiplier,
                    "/",
                    resolution,
                    "/",
                    from,
                    "/",
                    to,
                    "?sort=asc&limit=50000&apiKey=",
                    key)
  
  return(request)
}

# Takes an http-request for polygon.io. Returns a standard R-list containing
# the data obtained from the call
requestData <- function(request) {
  raw <- jsonlite::fromJSON(request)
  return(raw)
}

# Extracts the stock price data from the polygon.io result
extractResults <- function(Data) {
  return(Data$results) 
}

# Extracts the return status of the http-request
extractStatus <- function(Data) {
  return(Data$status) 
}

# Extract number of returned entries
extractResultsCount <- function(Data) {
  return(Data$resultsCount) 
}

# Asses whether the stock data is available or not based on the returned http-
# request.
wasTraded <- function(Data) {
  if(extractStatus(Data) == "OK" & extractResultsCount(Data) > 0) {
    return(TRUE) 
  }
  return(FALSE)
}

# Takes a key, from date, to date, resolution, multiplier, and ticker symbol,
# then downloads stock data for the ticker for each day spanning in the time 
# interval specified by from and to. Function will return a dataframe with the
# stock data, including a column with the ticker symbol and a column with num-
# ber of days from "from".
downloadFromTo <- function(key = NULL, from = NULL, to = NULL, 
                           resolution = "minute", 
                           multiplier = 1, ticker = NULL,
                           print.status = FALSE) {
  # Check parameters
  if(any(is.null(key), is.null(from), is.null(to), is.null(ticker))) {
    print("Missing parameter(s):")
    if(is.null(key)) {
      print(" - key")
    }
    if(is.null(from)) {
      print(" - from") 
    }
    if(is.null(to)) {
      print(" - to")
    }
    if(is.null(ticker)) {
      print(" - ticker") 
    }
    return("")
  }
  
  # Calculate the number of days that we obtain data over
  days <- as.numeric(as.Date(to) - as.Date(from))
  if(print.status) {
    print(paste("Receiving data in time interval from", from, 
                "to", to, ":", days, "days."))
  }
  
  # Retrieve data for each day
  Data <- data.frame()
  for(num in 0:days) {
    # Generate http-request string
    request <- generateAPIRequest(key = key, 
                                  from = as.Date(from) + num, 
                                  to = as.Date(from) + num, 
                                  resolution = resolution, 
                                  multiplier = multiplier,
                                  ticker = ticker)

    if(print.status) {
      print(paste("Requesting data on", ticker, "with resolution", resolution,
          "and time multiplier", multiplier, "for date:", as.Date(from) + num))
    }
    
    # Send the request to polygon.io    
    data_raw <- requestData(request)
    
    # If there is trading data then append the data
    if(wasTraded(data_raw)) {
      if(print.status) {
        print("Data was retreived. Saving to dataframe.") 
      }
      temp <- extractResults(data_raw)
      temp$day <- num
      temp$symb <- ticker
      Data %<>% rbind(temp)
    } else if(print.status) {
      print(paste("No data was retreived for", as.Date(from) + num))
    }
    
    if(print.status) {
      print("Waiting 12 seconds.")  
    }
    Sys.sleep(12) # Only 5 api calls allowed per minute
  }
  
  return(Data)
}

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

##### Program execution #####
main <- function() {
  tickers <- c("XLF", "BRK-B", "JPM", "V", "MA", "BAC")
  data <- data.frame()
  for(ticker in tickers) {
    temp <- downloadFromTo(key = API_KEY, from = "2024-04-01", to = "2024-04-15", 
                           ticker = ticker, print.status = T)
    data %<>% rbind(temp)
  }
  write.csv(file = paste0(WD, "/DemoData_2024-04-01-2024-04-15.csv"), x = data)
}

if(TEST_RUN) {
  main() 
}
