library(tidyverse)
get_data <- function(tickers, last_date, first_date) {
  
  # convert last_date into accessible format
  c("last_year", "last_month", "last_day") %=% (last_date %>%
                                                  stringr::str_split("/") %>%
                                                  unlist() %>%
                                                  as.numeric())
  
  # convert first_date into accessible format
  c("first_year", "first_month", "first_day") %=% (first_date %>% 
                                                     stringr::str_split("/") %>%
                                                     unlist() %>%
                                                     as.numeric())
  
  t1 <- ISOdate(first_year, first_month, first_day, hour=0)
  t2 <- ISOdate(last_year, last_month, last_day, hour=0)
  
  for(ticker in tickers) {
    
    # draw the url query to scrape the data off of Yahoo finance
    url <- paste(
      "https://query1.finance.yahoo.com/v7/finance/download/",
      ticker,
      "?period1=",
      as.integer(t1),
      "&period2=",
      as.integer(t2),
      "&interval=1d&events=history",
      sep="")

    tryCatch(
      expr = {
        # download the data
        temp <- read.csv(url)

        #subset relevant columns
        temp <- temp[,c("Date", "Adj.Close")]
        
        # systematically rename columns (ticker_adj_close)
        new_close <- paste(ticker, "adj_close", sep = "_")
        names(temp) <- c("Date", new_close)
        
        # combine the data of different tickers
        if (ticker == tickers[1]) {
          #on the first download, the result data.frame is newly created
          result <- temp
        } else {
          # all other data.frames get joined onto the result frame via Date column
          result <- dplyr::left_join(result, temp, by="Date")
        }
      },
      error = function(e) {
        print(ticker)
      }
    )
  }
  return(result)
}







