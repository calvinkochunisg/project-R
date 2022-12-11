#install all required r-packages
#install.packages("httr")
#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("tidyverse")
#install.packages("tidyquant")

library('httr')
library('jsonlite')
library('dplyr')
library('lubridate')
library('tidyverse')
library('tidyquant')
# Configuring settings as per tidyquant tutorial
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

#Configuring candlestick stlye Chart

geom_candlestick(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  colour_up = "darkblue",
  colour_down = "red",
  fill_up = "darkblue",
  fill_down = "red"
)

#get the information to look up historical price date in API
token <- "drj71x0ytu" #enter the personalized token from Stock Shark

dataType <- "getHistoricalPrice" #specify the type of data to retrieve (show a list of available data types)
ticker <- "SPY" #Stock Ticker Symbol (maybe also show a list of available Tickers)
startDate <- "2020-01-01" #Set a Start-Date for the date range of the data
endDate <- "2022-01-01" #Set an End-Date for the date range of the data
periodType <- "daily" #set the resolution of the time axis (show a list of available periods)

#Get the Data for the selected ticker time frame and time intervalls
api_url <- paste(                                                   #get the information and create the respective api url to retrieve the data
                  "https://stock-shark.com/api/v1/", 
                  dataType, "?",
                  "startDate=",  startDate,
                  "&endDate=", endDate,
                  "&ticker=", ticker,
                  "&token=", token, 
                  "&periodType=", periodType, 
                  sep=""
                )
raw_data <- httr :: GET(api_url)                                    #retrieve the data with the created url and store it using the httr package
content <- httr::content(raw_data, as = 'text')                     #convert the stored content to text
json_content <- jsonlite::fromJSON(content)                         #convert the stored content to json
data <- json_content$data$candles                                   #convert the stored content to a data frame
date <- as.Date(data$datetime)


# ggplot function for candelstick frame
candlechart <- function(ticker, startDate, endDate, data) {
  
  low_min <- min(data$low)
  high_max <- max(data$high)
  
  ggplot(data = data, aes(x = date, y = close, title = "ticker")) + 
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  geom_ma(color = "darkgreen") +
  coord_x_date(xlim = c(startDate, endDate),
               ylim = c(low_min, high_max))
}
candlechart(ticker, startDate, endDate, data)
