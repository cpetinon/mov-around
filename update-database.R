library(lubridate)
library(purrr)
library(jsonlite)
library(readr)
library(httr)
library(dplyr)

library(telraamStats)

source('params.R')

########
# Utilisation of the lubridate package
########

options(lubridate.week.start = 1)  # To start the week on day 1 (package parameter)

########
# API key (see the Telraam site to generate one)
########

key <- Sys.getenv("TELRAAM_KEY")
key1 <- c(
  'X-Api-Key' = key
)

## Initialization of the update

date_filepath <- "data/date.txt"

# Function to check if an update is possible
checkUpdatePossible <- function(date) {
  return(as.Date(date) < as.Date(ending_date))
}

# Function to update the database
updateDatabase <- function(update) {
  date <- as.Date(update$date)
  
  # Check if an update is possible, otherwise nothing happens
  if (checkUpdatePossible(date)) {
    # Check the existence of the API's key
    if (is.null(key)) {
      update$state <- "The API key is missing."
    } else if (!api_state(key1)) {
      update$state <- "There seems to be a problem with the API. Please wait until tomorrow or contact support."
    } else {
      # Update the database
      for (id_sensor in sensor_ids) { # Iteration on all sensors
        yesterday <- ending_date - 1 # Today is excluded
        write_update_data(id_sensor, date1 = date, date2 = yesterday)
      }
      
      # Update the date of the next update
      writeLines(as.character(ending_date), con = date_filepath)
      update$date <- ending_date
      
      # Update the state of the database
      update$state <- paste0("The data ranges from ", starting_date, "to ", ending_date - 1, ". The database has been updated. Please restart the application.")
    }
  }
}

# Initialize the update object
if (file.exists(date_filepath)) { # When the database already exists
  date <- readLines(date_filepath)
  
  # Check if an update is possible
  if (checkUpdatePossible(date)) {
    action <- ", an update with the Telraam API is possible."
  } else {
    action <- ", the database is up to date with the Telraam API."
  }
  
  update <- list(
    state = paste0("The data stored in the application ranges from ", starting_date, "to ", date, action),
    date = date,
    key = NULL
  )
  
} else { # When the database is empty
  update <- list(
    state = "The database is empty, please update the data.",
    date = starting_date, # The database begins on 2021-01-01
    key = NULL
  )
}

# Update the database
updateDatabase(update)