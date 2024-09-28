# set the working directory -----------------------------------------------

setwd("~/oddsapi_assignment/")


# install and load the required package  ----------------------------------

install.packages('oddsapiR')
library(oddsapiR)
library(readr)
library(dplyr)
library(lubridate)
library(httr)
library(glue)

# REPORT 4 ----------------------------------------------------------------

# set the API keys here
api_key <- 'API KEY HERE'
Sys.setenv(ODDS_API_KEY = api_key)


# Function to fetch historical events for a given sport
get_historical_events <- function(sport_key,
                                  apiKey,
                                  date,
                                  commenceTimeFrom = NULL,
                                  commenceTimeTo = NULL,
                                  dateFormat = "iso") {
  # Base URL for the endpoint
  base_url <- glue("https://api.the-odds-api.com/v4/historical/sports/{sport_key}/events")
  
  # Define the query parameters
  query_params <- list(
    apiKey = apiKey,
    date = date,
    dateFormat = dateFormat,
    commenceTimeFrom = commenceTimeFrom,
    commenceTimeTo = commenceTimeTo
  )
  
  # Make the GET request
  response <- httr::GET(base_url, query = query_params)
  
  # Check if the request was successful
  if (response$status_code == 200) {
    # Parse the response as JSON
    event_data <- httr::content(response, as = "parsed", type = "application/json")
    
    
    # Check if the event_data is a list and not empty
    if (is.list(event_data) && length(event_data) > 0) {
      # Extract the list of events from the `data` field in `events`
      event_data <- event_data$data
      
      # Use lapply or a loop to extract the `id` values
      event_ids <- lapply(event_data, function(x)
        x$id)
      
      # If you want a vector instead of a list, you can unlist it
      event_ids <- unlist(event_ids)
      
      
      # Convert to a data frame for easier handling
      event_df <- data.frame(
        id = event_ids,
        home_team = sapply(event_data, function(x)
          x$home_team),
        away_team = sapply(event_data, function(x)
          x$away_team),
        commence_time = sapply(event_data, function(x)
          x$commence_time),
        stringsAsFactors = FALSE
      )
      
      return(event_df)
    } else {
      message("No events found for the provided parameters.")
      return(NULL)
    }
  } else {
    # Handle errors
    stop(
      glue(
        "Failed to fetch historical events. Status code: {response$status_code}. Error: {httr::content(response, as = 'text')}"
      )
    )
  }
}

# Example usage:
events <- get_historical_events(sport_key = "soccer_epl",
                                apiKey = api_key,
                                date = "2024-09-01T12:00:00Z")
print(events)

# Define the function to fetch historical odds for multiple event_ids
get_historical_odds_for_event_ids <- function(sport_key,
                                              date,
                                              regions = NULL,
                                              markets = NULL,
                                              bookmakers = NULL,
                                              odds_format = "decimal",
                                              date_format = "iso",
                                              output_file = "historical_odds.csv") {
  # Initialize an empty list to store results for all event_ids
  result_list <- list()
  
  # Fetch all unique event_ids for the specified sport_key
  event_ids <- get_historical_events(sport_key = sport_key,
                                     apiKey = api_key,
                                     date = date)$id
  
  # Loop through each event_id
  for (event_id in event_ids) {
    cat("Fetching historical odds for event_id:", event_id, "\n")
    for (region in regions) {
      for (market in markets) {
      # Fetch historical odds for each event_id using toa_sports_odds_history
      odds_history_data <- tryCatch({
        toa_sports_odds_history(
          sport_key = sport_key,
          event_ids = event_id,
          date = date,
          regions = region,
          markets = market,
          bookmakers = bookmakers,
          odds_format = odds_format,
          date_format = date_format
        )
        
      }, error = function(e) {
        cat("Error fetching data for event_id:",
            event_id,
            "\n",
            e$message,
            "\n")
        return(NULL)  # Return NULL in case of error
      })
      # Check if the data is not NULL before processing
      if (!is.null(odds_history_data)) {
        # Apply filters if bookmakers are provided
        if (!is.null(bookmakers)) {
          odds_history_data <- odds_history_data[odds_history_data$bookmaker %in% bookmakers, ]
        }
        
        # Proceed only if there are valid rows after filtering
        if (nrow(odds_history_data) > 0) {
          # Select the earliest and latest odds for each sport_key and bookmaker
          filtered_data <- odds_history_data %>%
            group_by(sport_key, bookmaker) %>%
            summarize(
              earliest_odds = first(outcomes_price),
              latest_odds = last(outcomes_price),
              home_team = first(home_team),         
              away_team = first(away_team),         
              commence_time = first(commence_time),
              # include outcome_name and outcome_point for totals/spreads
              outcomes_name=outcomes_name,
              outcome_point = first(outcomes_point),
              outcomes_price = first(outcomes_price),
              .groups = "drop"
            )
          
          # Append result to the list
          result_list[[paste(region, market, event_id, sep = '_')]] <- filtered_data
        }
      }
    }
  }
  }
  # Combine the data from all event IDs
  if (length(result_list) > 0) {
    combined_data <- bind_rows(result_list, .id = 'region_market_event_id')
    combined_data <- combined_data %>%
      tidyr::separate(
        region_market_event_id,
        into = c('region', 'market key', 'event_id'),
        sep = '_'
      )
    # Write the combined data to a CSV file
    write.csv(combined_data, file = output_file, row.names = FALSE)
    cat("Historical odds data saved to:", output_file, "\n")
  } else {
    cat("No valid data to write to CSV.\n")
  }
}



# Example usage:
get_historical_odds_for_event_ids(
  sport_key = "soccer_epl",
  # Edit this as needed
  date = "2024-09-01T12:00:00Z",
  # Specify the date or date range
  regions = "us",
  # Specify region(s) (optional)
  markets = c("totals", 'spreads'),
  # Specify markets (optional)
  bookmakers = NULL,
  # Specify bookmakers (optional)
  output_file = "historical_odds_output.csv"  # Specify output file name
)
