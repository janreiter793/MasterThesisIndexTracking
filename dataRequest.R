################################################################################
#                                                                              #
# 01-05-2025: Data fetch demo                                                  #
# - Author: Jan Reiter Sørensen                                                #
#                                                                              #
# Downloads stock data with minute resolution for a given set of stock symbols #
# in a given time interval. Obtains the market cap for stocks. Works for the   #
# free subscription to polygon.io.                                             #
#                                                                              #
################################################################################
library(jsonlite)
library(magrittr)

###### Constants #####
API_KEY <- "" # API key for polygon.io, does not require subscription.
WD <- "" # Directory to store data

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

# Retrieves the market cap of a ticker
retrieveMarketCap <- function(ticker, date) {
  data_raw <- requestData(paste0(
    "https://api.polygon.io/v3/reference/tickers/",
    ticker,
    "?date=",
    date,
    "&apiKey=JGWTBya3mqdbU16w43ImnqjPMP7AjrOt"
  ))
  temp <- extractResults(data_raw)
  return(temp$market_cap)
}

##### Program execution #####
main <- function() {
  selected_stocks <- 
    read.csv(file = paste0(WD, "stock_symbols.csv"))
  tickers <- selected_stocks$Ticker
  
  # First obtain the market caps and save in a file
  cat("Retrieving market caps.\n")
  market_caps <- numeric(length(tickers))
  for(iter_main in 1:length(tickers)) {
    cat("Fetching market cap for ", tickers[iter_main], ". (", iter_main, 
    "/", length(tickers), ")\n")
    market_caps[iter_main] <- retrieveMarketCap(tickers[iter_main],
                                                "2024-04-01")
    cat("Got ", market_caps[iter_main], ".\n")
    Sys.sleep(12)
  }
  df <- data.frame(Tickers = tickers,
                   Market_cap = market_caps)
  write.csv(file = paste0(WD, "/MarketCapsSP500_Sectors_2024-04-01.csv"), 
            x = df)
  
  # Now fetch the stock price data
  data <- data.frame()
  for(ticker in tickers) {
    temp <- downloadFromTo(key = API_KEY, from = "2024-04-01", to = "2024-06-30", 
                           ticker = ticker, print.status = T)
    data %<>% rbind(temp)
  }
  write.csv(file = paste0(WD, "/DataSP500_Sectors_2024-04-01-2024-06-20.csv"), 
            x = data)
}

main() 

