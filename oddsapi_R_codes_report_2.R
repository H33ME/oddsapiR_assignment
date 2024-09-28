# set the working directory -----------------------------------------------

setwd("~/oddsapi_assignment/")

# install and load the required package  ----------------------------------

install.packages('oddsapiR')
library(oddsapiR)
library(readr)
library(dplyr)


# REPORT 2 ----------------------------------------------------------------

# set the API keys here
api_key <- 'API KEY HERE'
Sys.setenv(ODDS_API_KEY = api_key)

# list of all available bookmakers ---------------------------------------

get_toa_bookmakers <- function(sport_key = NULL, regions = NULL, markets = NULL){
  
  all_bookmaker <- data.frame()
  
  for (region in regions) {
    for (market in markets) {
      toa_sports_odds_to_access_bookmakers <- toa_sports_odds(
        sport_key = sport_key,
        regions = region,
        markets = market,
        odds_format = 'decimal',
        date_format = 'iso'
      )
      print(head(toa_sports_odds_to_access_bookmakers))
      
      # select the bookmakers, bookmaker_key, region
      toa_bookmakers <- toa_sports_odds_to_access_bookmakers %>%
        mutate(region = region) %>%
        select(bookmaker_key, bookmaker, region)
      all_bookmaker <- rbind(all_bookmaker, toa_bookmakers)
      
    }
  }
  return(all_bookmaker)
}

sport_key <- 'baseball_mlb'
regions <- c('us', 'uk')
markets <- c('h2h', 'totals')

# call the function 
toa_bookmarkers <- get_toa_bookmakers(sport_key = sport_key, regions = regions, markets = markets)

#write the csv with the bookmakers
write_csv(toa_bookmarkers, 'toa_bookmarkers.csv')
