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
    install.packages("zoo")
    
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
  library("zoo")
  
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
    
    #Get User specified Ticker and Date Range (period was removed)
    print("Enter the requested values to retrieve data for your stock ticker. Faulty inputs or the lack thereof will be replaced with the standard values for that variable.")
    {
      ticker <<- readline("Enter a Stock Ticker (Example: SPY): ");
      startDate <<- readline("Enter the Start Date of the Time Series (Format: YYYY-MM-DD): ");
      endDate <<- readline("Enter the End Date of the Time Series (Format: YYYY-MM-DD): ");
      #periodType <<- readline("Enter the time resolution of your data (Options: daily, hourly, 5min, 10min, 15min, 30min): ");           <- Feature removed
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

#Calculating different financial indicators
indicators <- function(){
  
  #In the next few lines, the difference of the two variable endDate and StartDate will be calculated. Dependant on the outcome, the code
  #will (if condition is true) calculate the moving average (7 days) and generate a new column in the df named MA7 or (if condition is false)
  #create a new column MA7 and fill it blank.
  
  if (difftime(endDate,startDate) < 365) {
    
    MA_7 <<- rollmean(ticker_data$close, k = 7)
    ma7 <<- c(NA [1:6], MA_7)
    ticker_data["MA7"] <<- round(ma7, digits = 2)
    
  } else {ticker_data["MA7"] <<- ""
  
  }
  
  #if the condition was true above, MA_7 variable has been created and therefore the formula below will be used to calculate the difference between
  #the last closing price and the last moving average value. This value will be applied to the variable distance MA7 (in percent)
  
  if (n_distinct(ticker_data["MA7"])>1) {
    
    distance_MA7 <<- round((ticker_data$close[nrow(ticker_data)]*100/ticker_data$MA7[nrow(ticker_data)])-100, digits = 4)
    
  } else {print("Difference from close price to moving average (7 days) is not included")
    
  }
  
  #Moving average (50 days) will be included without any condition. Same for distance between ma and last close price.
  
  MA_50 <<- rollmean(ticker_data$close, k = 50, digits = 2)
  ma50 <<- c(NA [1:49], MA_50)
  ticker_data["MA50"] <<- round(ma50, digits = 2)
  
  distance_MA50 <<- round((ticker_data$close[nrow(ticker_data)]*100/ticker_data$MA50[nrow(ticker_data)])-100, digits = 4)
  
  #Exactly the same applies for moving average (200 days), which will only be included if the difference in the time span is greater than 4 years.
  #Else, a column in the df with the name MA200 will be filled blank
  
  if (difftime(endDate,startDate) > 1460) {
    
    MA_200 <<- rollmean(ticker_data$close, k = 200, digits = 2)
    ma200 <<- c(NA [1:199],MA_200)
    ticker_data["MA200"] <<- round(ma200, digits = 2)
    
  } else {ticker_data["MA200"] <<- ""
  
  }
  
  #Check if MA_200 is existent, if so, formula is applied to calculate differnce between ma and last close price. If not the sentce below will be printed.
  
  if (n_distinct(ticker_data["MA200"])>1) {
    
    distance_MA200 <<- round((ticker_data$close[nrow(ticker_data)]*100/ticker_data$MA200[nrow(ticker_data)])-100, digits = 4)
    
  } else {print("Difference from close price to moving average (200 days) is not included")
    
  }
  
  #The following lines of code are about daily returns. with the function options() the values wont be treated in a scientific notification.
  #A new column with the name "Daily return" has been created filled with the values in the variable Returns.
  
  close_price <<- c(ticker_data$close)
  options(scipen = 999)
  Returns <<- diff(close_price)/close_price[-length(close_price)]
  ticker_data["Daily Returns"] <<- c(0,Returns)
  
  #The maximum and minimum value of the daily return in the created column is saved into variables
  
  search_max_daily_return <<- which.max(ticker_data$`Daily Returns`)
  search_min_daily_return <<- which.min(ticker_data$`Daily Returns`)
  
  #the maximum daily return will be formed into a percentage value and also the date will be searched out
  
  max_daily_return_per <<- round(ticker_data$`Daily Returns`[search_max_daily_return]*100, digits = 4)
  max_return_date <<- ticker_data$date[search_max_daily_return]
  
  #Same step for the minimum daily return and its date.  
  
  min_daily_return_per <<- round(ticker_data$`Daily Returns`[search_min_daily_return]*100, digits = 4)
  min_return_date <<- ticker_data$date[search_min_daily_return]
  
  #Generate the Plot Caption with the calculated financial Data
  
  plot_caption <<- c() #Reset the Plot Caption
  
  plot_caption <<- c(paste(
    "\n\n",
    "The selected ticker ", ticker," has shown the following technical properties from ", startDate, " to ", endDate,"\n\n", 
    "Maximum daily return of ", max_daily_return_per, " percent was observed on ", max_return_date, "\n\n",
    "Minimum daily return of ", min_daily_return_per, " percent was observed on ", min_return_date, "\n\n",
    sep=""
  )
  )
  
  if(n_distinct(ticker_data["MA7"])>1){
    plot_caption <<- c(plot_caption,
                       paste("The difference between ", endDate, " closing price (", ticker_data$close[nrow(ticker_data)], ") and 7 days moving average (", ticker_data$MA7[nrow(ticker_data)], ") is ", distance_MA7, " percent", "\n\n", sep="")
    )
  }
  
  if(n_distinct(ticker_data["MA50"])>1){
    plot_caption <<- c(plot_caption,
                       paste("The difference between ", endDate, " closing price (", ticker_data$close[nrow(ticker_data)], ") and 50 days moving average (", ticker_data$MA50[nrow(ticker_data)], ") is ", distance_MA50, " percent", "\n\n", sep="")
    )
  }
  
  if(n_distinct(ticker_data["MA200"])>1){
    plot_caption <<- c(plot_caption,
                       paste("The difference between ", endDate, " closing price (", ticker_data$close[nrow(ticker_data)], ") and 200 days moving average (", ticker_data$MA200[nrow(ticker_data)], ") is ", distance_MA200, " percent", "\n\n", sep="")
    )
  }
  
  plot_caption <<- paste(plot_caption,collapse="")
  
  #If everything is calculated/applied, the print function reflects the following sentence
  
  print(paste("Financial indicators calculated for", ticker))
}

#ggplot function for candlestick chart
candlechart <- function(){
  #Set data Range for plot
  low_min <<- min(ticker_data$low)
  high_max <<- max(ticker_data$high)
  
  date_min <<- min(ticker_data$date)
  date_max <<- max(ticker_data$date)
  
  #datetime_min <<- as.POSIXct(format(min(data$datetime_converted), format="%Y-%m-%d %H"))
  #datetime_max <<- max(data$datetime_converted)
  
  #Generate Plot with GGplot
  Index_chart <<- print(ggplot(data = ticker_data, aes(x = date, y = close)) + 
                          geom_candlestick(aes(open = open, high = high, low = low, close = close))  +
                          coord_x_date(xlim = c(date_min, date_max),ylim = c(low_min, high_max)) +
                          ggtitle(paste("Ticker: ", ticker))+
                          xlab("Date")+
                          ylab("Value in USD"))+
    #+scale_x_datetime(labels = date_format("%Y-%m-%d %H"), date_breaks = "8 hours")+
    {if(n_distinct(ticker_data["MA7"])>1)geom_line(aes(y=MA7, colour = "MA7"), size = 0.4)} +
    {if(n_distinct(ticker_data["MA50"])>1)geom_line(aes(y=MA50, colour = "MA50"), size = 0.4)} + 
    {if(n_distinct(ticker_data["MA200"])>1)geom_line(aes(y=MA200, colour = "MA200"), size = 0.4)} +
    scale_color_manual(name = "Moving Averages", values = c("MA7" = "green", "MA50" = "black", "MA200" = "brown"))+
    labs(caption = plot_caption)+                  
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))
  print(Index_chart)
  
  print(paste("Data plotted for ticker", ticker))
}

#Output of determined values/plots
output <- function(){
  #Generate a PDF file of the Plot
  pdf(file = paste(ticker, "_from_", startDate, "_to_", endDate, "_", periodType, "_Chart.pdf"),
      width = 12.8, 
      height = 9)
  print(Index_chart)
  dev.off()
  
  file.show(paste(ticker, "_from_", startDate, "_to_", endDate, "_", periodType, "_Chart.pdf"))
}

#combine all functions into a single program
run <- function(){
  
  run_again <- "y"
  
  initialize()
  
  while(run_again == "y"){
    user_input()
    get_selected_data()
    indicators()
    candlechart()
    output()
    
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
