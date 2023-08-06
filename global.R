########
# Package import
########
library(shiny) # for the app
library(tidyverse) # tibble import
library(dplyr) # tibble manipulation
library(ggplot2) # chart
library(httr) # for the import
library(jsonlite) # for the import
library(lubridate) # date format management
library(cowplot) # graphic overlay
library(CPAT) # Darling Erdos test
library(synchrony) # peak function
library(forecast) # moving average function
library(zoo) # na.trim function
library(readr) # csv manipulation
library(mgcv) # gam function (lisser les courbes)

library(telraamStats)

########
# File import
########
source('1-accueil.R')
source('2-import.R')
source('3-comparaison_periode.R')
source('4-heure_engorg.R')
source('5-seuil_engorg.R')
source('6-comparaison.R')
source('params.R')



########
# Utilisation of the lubridate package
########

options(lubridate.week.start = 1)  # To start the week on day 1 (package parameter)
today <- today()

########
# API key (see the Telraam site to generate one)
########

key <- Sys.getenv("MY_KEY")
key1 <- c(
  'X-Api-Key' = key
)