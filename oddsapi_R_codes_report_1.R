
# set the working directory -----------------------------------------------

setwd("~/oddsapi_assignment/")

# install and load the required package  ----------------------------------

install.packages('oddsapiR')
library(oddsapiR)
library(readr)
library(dplyr)

# REPORT 1 ----------------------------------------------------------------

# set the API keys here
api_key <- 'API KEY HERE'
Sys.setenv(ODDS_API_KEY = api_key)


# list of everything in toa_sports_keys -----------------------------------

# fetch all available sports keys -----------------------------------------

sports_keys <- toa_sports_keys

# write the csv file for sports keys --------------------------------------

write_csv(sports_keys, 'sports_keys.csv')



