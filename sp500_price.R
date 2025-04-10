################################################################################
#                                                                              #
# Lasted edited: 19-03-2025                                                    #
# Author: Jan Reiter Sørensen                                                  #
#                                                                              #
# Calculates the price of buying all the stocks in the SP500 in the correct    #
# amount of proportions.                                                       #
#                                                                              #
################################################################################
library(rvest)
library(tidyverse)
library(magrittr)

#### Execution ####
url <- "https://www.slickcharts.com/sp500"

# Read the webpage content, and extract the table of S&P 500 components
webpage <- read_html(url)
table <- webpage %>%
  html_table(fill = TRUE)
df <- table[[1]]

# Turn the numbers from character to numeric, and remove observations with
# weight <= 0. Hence, the estimate in the end, may be lower than the actual price
df %<>% 
  mutate(Weight = substr(Weight, 1, 4),
         Weight = as.numeric(Weight),
         Price = gsub(",", "", Price),
         Price = as.numeric(Price)) %>% 
  filter(Weight > 0)

# Find the position with smallest weight and largest unit price, assume that
# we buy just one of these
df_subset <- df[which(df$Weight == min(df$Weight)),]
dp <-  
  df_subset %>% 
  filter(Price == max(Price)) %>% 
  select(Price, Weight)

# Estimate the price of the SP500
cat(
  paste0("The price of buying the SP500 by buying each stock with the correct weight is around: ",
         dp$Price / dp$Weight * 100,
         "$\n")
  )
