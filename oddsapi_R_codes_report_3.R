# set the working directory -----------------------------------------------

setwd("~/oddsapi_assignment/")

# install and load the required package  ----------------------------------

install.packages('oddsapiR')
library(oddsapiR)
library(readr)
library(dplyr)

# REPORT 3 ----------------------------------------------------------------

# set the API keys here
api_key <- 'API KEY HERE'
Sys.setenv(ODDS_API_KEY = api_key)


# current odds with editable filters --------------------------------------

# function to get current odds with filters
get_current_odds <- function(sport_key = NULL,
                             regions = NULL,
                             markets = NULL,
                             bookmaker = NULL,
                             odds_format = 'decimal',
                             date_format = 'iso') {
  all_odds <- data.frame()
  for(region in regions){
    for(market in markets){
      # fetch the odds data for each region and market combination
      current_odds <- toa_sports_odds(
        sport_key = sport_key,
        regions = region,
        markets = market,
        odds_format = odds_format,
        date_format = date_format
      )
      print(head(current_odds))
      # apply additional filters if necessary
      if (!is.null(bookmaker)) {
        current_odds <- current_odds[current_odds$bookmaker %in% bookmaker, ]
      }
      if (!is.null(sport_key)) {
        current_odds <- current_odds[current_odds$sport_key %in% sport_key, ]
      }
      
      # combine the results into one data frame
      all_odds <- rbind(all_odds, current_odds)
      print(all_odds)
      return(all_odds)
    }
  }
}
# get the current odds
current_odds <- get_current_odds(
  sport_key = 'soccer_epl',
  regions = c('us', 'uk'),
  markets = c('spreads'),
  bookmaker = NULL
)

# write a csv of current odds
write_csv(current_odds, 'current_odds_soccer_epl_us_uk_regions_total_spreads_market.csv')
