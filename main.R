#install all required r-packages
install <- function(){
  
  install_y_n <- "n"
  {
  install_y_n = readline("Do you want to install/ update the needed packages for this script (The Session might crash when installing the packages)? y/n ")
  }
  install_y_n <- tolower(install_y_n)
  if (install_y_n == "y"){
    
    install.packages("httr")
    install.packages("jsonlite")
    install.packages("dplyr")
    install.packages("lubridate")
    install.packages("tidyverse")
    install.packages("tidyquant")
    
    install_y_n <- "n"
    print("INSTALLING PROCESS COMPLETED")
  }
  else{
    print("INSTALLING PROCESS ABORTED")
  }
  
  
  
}

#Initially Set Up the R environment
initialize <- function(){
library('httr')
library('jsonlite')
library('dplyr')
library('lubridate')
library('tidyverse')
library('tidyquant')

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

#Set the Standard Values for the Chart
std_token <<- "drj71x0ytu" #enter the personalized token from Stock Shark

std_dataType <<- "getHistoricalPrice" #specify the type of data to retrieve (show a list of available data types)
std_ticker <<- "SPY" #Stock Ticker Symbol (maybe also show a list of available Tickers)
std_startDate <<- as.Date("2020-01-01") #Set a Start-Date for the date range of the data
std_endDate <<- today() #Set an End-Date for the date range of the data
std_periodType <<- "daily" #set the resolution of the time axis (show a list of available periods)

#Initially define Variables with standard values
token <<- std_token
dataType <<- std_dataType

ticker <<- std_ticker
startDate <<- std_startDate
endDate <<- std_endDate
periodType <<- std_periodType
}

#Get User Input
user_input <- function(){

  input_change <- "y"
  

  while( input_change == "y"){
    
    print("Enter the requested values to retrieve data for your stock ticker. Faulty inputs or the lack thereof will be replaced with the standard values for that variable.")
    {
      ticker <<- readline("Enter a Stock Ticker (Example: SPY): ");
      startDate <<- readline("Enter the Start Date of the Time Series (Format: YYYY-MM-DD): ");
      endDate <<- readline("Enter the End Date of the Time Series (Format: YYYY-MM-DD): ");
      periodType <<- readline("Enter the time resolution of your data (Options: daily, hourly, 5min, 10min, 15min, 30min): ");
    }
    
    #Clean up Ticker Case
    ticker <<- toupper(ticker)
    
    #Check if Ticker input is Empty
    if(ticker == ""){    #If ticker input is empty replace it with the standard value for ticker
      ticker <<- std_ticker
    }
    
    #Check if Date is valid else use the standard value
    test_startDate <<- tryCatch(
      {
        #Clean up / Convert Dates to the needed Format (with - instead of other seperators)
        startDate <<- as.Date(startDate)
        
        
        #Check if Date inputs are valid
        if(!is.na(parse_date_time(startDate,orders="ymd")) == FALSE){    #If startDate input is not in Year-Month-Day format replace it with the standard value for startDate
          print("The selected start date is invalid. The standard value for the start date is used.")
          startDate <<- std_startDate}
      },
      error = function(e){
        print("The selected start date is invalid. The standard value for the start date is used.")
        startDate <<- std_startDate
      }
      )
    test_startDate
    
    #Check if Date is valid else use the standard value
    test_endDate <<- tryCatch(
      {
        #Clean up / Convert Dates to the needed Format (with - instead of other seperators)
        endDate <<- as.Date(endDate)
        
        #Check if Date inputs are valid
        if(!is.na(parse_date_time(endDate,orders="ymd")) == FALSE){      #If endDate input is not in Year-Month-Day format replace it with the standard value for endDate
          print("The selected end date is invalid. The standard value for the end date is used.")
          endDate <<- std_endDate
        }
      },
      error = function(e){
        print("The selected end date is invalid. The standard value for the end date is used.")
        endDate <<- std_endDate
      }
    ) 
    test_endDate
    
    
    #Check if periodType is one of the valid ones
    #If periodType is not one of the options (daily, hourly, 5min, 10min, 15min, 30min) it will be replaced with the standard value for periodType
    if((periodType == "daily" | periodType == "hourly" | periodType == "5min" | periodType == "10min" | periodType == "15min" |periodType == "30min") == FALSE){
      print("The selected time period is invalid. The standard value for the time period is used.")
      periodType <<- std_periodType
    }
    
    cat(paste("\nYou Selected the Ticker", ticker, "from", startDate, "to",endDate, "on a", periodType, "interval."))
    input_change = readline("Do you want to change these inputs? y/n ");
    input_change <- tolower(input_change)
    
  }
print(paste("You confirmed Ticker", ticker, "from", startDate, "to",endDate, "on a", periodType, "interval."))
}

#Get the Data for the selected ticker time frame and time intervals
get_selected_data <- function(){
  print(paste("Downloading data for ticker", ticker, "..."))
  
  #Set Global Variables    
  api_url <<- ""
  raw_data <<- ""
  dl_content <<- ""
  json_content <<- ""
  ticker_data <<- ""
      
      api_url <<- paste(                                                   #get the information and create the respective api url to retrieve the data
                        "https://stock-shark.com/api/v1/", 
                        std_dataType, "?",
                        "startDate=",  startDate,
                        "&endDate=", endDate,
                        "&ticker=", ticker,
                        "&token=", std_token, 
                        "&periodType=", periodType, 
                        sep=""
                      )
      raw_data <<- httr :: GET(api_url)                                    #retrieve the data with the created url and store it using the httr package
      dl_content <<- httr::content(raw_data, as = 'text')                     #convert the stored content to text
      json_content <<- jsonlite::fromJSON(dl_content)                         #convert the stored content to json
      ticker_data <<- as.data.frame(json_content$data$candles)                    #convert the stored content to a data frame
      ticker_data$datetime <<- as_datetime(ticker_data$datetime)                         #convert the datetime column to the standard datetime format used in this script
      ticker_data$date <<- as_date(ticker_data$datetime)                                 #convert the datetime column to the standard datetime format used in this script
      #data$datetime_converted <- as.POSIXct(format(data$datetime, "%Y-%m-%d %H"), format="%Y-%m-%d %H")
      
      
      print(paste("Data dowloaded for ticker", ticker))
}

# ggplot function for candlestick chart
candlechart <- function() {
  
  low_min <<- min(ticker_data$low)
  high_max <<- max(ticker_data$high)
  
  date_min <<- min(ticker_data$date)
  date_max <<- max(ticker_data$date)
  
  #datetime_min <<- as.POSIXct(format(min(data$datetime_converted), format="%Y-%m-%d %H"))
  #datetime_max <<- max(data$datetime_converted)
  
  print(ggplot(data = ticker_data, aes(x = date, y = close)) + 
                  geom_candlestick(aes(open = open, high = high, low = low, close = close))  +
                  coord_x_date(xlim = c(date_min, date_max),ylim = c(low_min, high_max)) +
                  ggtitle(ticker)+
                  xlab("Date")+
                  ylab("Value in USD"))
                  #+scale_x_datetime(labels = date_format("%Y-%m-%d %H"), date_breaks = "8 hours")
  
  
  print(paste("Data plotted for ticker", ticker))
}

#combine all functions into a single program
run <- function(){

  run_again <- "y"
  
  initialize()
  
    while(run_again == "y"){
      user_input()
      get_selected_data()
      candlechart()
      
      run_again = readline("Do you want to plot another chart? y/n ")
      run_again <- tolower(run_again)
    }
}

start_program <- function(){
  install()
  run()
}

#run the program
start_program()
