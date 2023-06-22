

############
# Package import
############

library(shiny) # Pour l'appli
library(tidyverse) # Import de tibble
library(dplyr) # Manipulation de tibble
library(ggplot2) # Graphique
library(shinythemes) # Pour le style "journal"
library(httr) # Pour l'import
library(jsonlite) # Pour l'import
library(lubridate) # Gestion du format de date
library(cowplot) # Superposition de graphique
library(CPAT) # test de Darling Erdos
library(synchrony) # fonction peaks (synchronicite des pics)
library(forecast) # fonction ma : moving average
library(zoo) # fonction na.trim
library(plotly) # Pour le graphique des heures d'engorgement
library(readr) # Pour l'export en csv compatible excel

############
# File import
############
source('1-accueil.R')
source('2-import.R')
source('3-comparaison_periode.R')
source('4-heure_engorg.R')
source('5-seuil_engorg.R')
source('6-comparaison.R')
source('7-methode.R')

########
# Parameters of the package
########

options(lubridate.week.start = 1) # Pour que la semaine commence jour 1


#######################################################################################

# Sensor list

#######################################################################################

# /!\ The order of the following lists is important, as it links sensor ids to their names /!\

listeSegments <- c(9000002156, 9000001906, 9000001618,9000003090,9000002453,9000001844,
                   9000001877,9000002666,9000002181,9000002707,9000003703,
                   9000003746,9000003775,9000003736)
listeNom <- c("Burel","Leclerc","ParisMarche","rueVignes","ParisArcEnCiel","RteVitre",
              "RueGdDomaine","StDidierNord","rueVallee","StDidierSud","RuePrieure",
              "RueCottage","RueVeronniere","RueDesEcoles")
listeNombis <- c("Burel-01","Leclerc-02","ParisMarche-03","rueVignes-04","ParisArcEnCiel-05","RteVitre-06",
                 "RueGdDomaine-07","StDidierNord-08","rueVallee-09","StDidierSud-10","RuePrieure-11",
                 "RueCottage-12","RueVeronniere-13","RueDesEcoles-14")


#######################################################################################

# Data import (constant)

#######################################################################################


##########################################################
### clef pour l'API Telraam (à generer su telraam.net) ###
##########################################################

# Read API key for updates
if (file.exists('clef.txt')){
  key <- readLines("clef.txt")
} else {
  key <- NULL
}

# Retrieve of today
today <- today()

#################################################
### Import public holiday and vacation dates ###
#################################################

# From a government API (this part must be re-adapted if there is a problem/change in the gouv rating)

### Public holiday ###

# (imported over 25 years: 285 days)
# Recovering days in "YYYY-MM-DD" format

public_holidays <- GET("https://calendrier.api.gouv.fr/jours-feries/metropole.json") %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON() %>% 
  names() %>% 
  ymd()

### Import vacation date ###

#Import sur les donnees en lignes: a la creation entre 2017 et 2023 (35 vacances)
#recuperation des donnees specification de l'academie dans location (a changer si changement d'academie).

url <- "https://data.education.gouv.fr/api/v2/catalog/datasets/fr-en-calendrier-scolaire/exports/json"

vacations <- GET(
  url = url,
  query = list(refine = "location:Rennes",
               exclude = "population:Enseignants")
) %>%
  .$content %>%
  rawToChar() %>%
  fromJSON() %>%
  select(description,
         start_date,
         end_date) %>%
  mutate(
    start_date = ymd_hms(start_date),
    end_date = ymd_hms(end_date)
  )
# Remarque: les dates des vacance finissent vers 22h ou 23h -> cela ne changent rien au vu des heures
# de fonctionnement des capteurs Telraam (ne fonctionne que s'il fait jour)

#######################################################################################

# Definition of global functions

#######################################################################################


########################
# Date selection function
########################


#' Filtering dataframe rows by date
#'
#' @param data a dataframe with a "date" column (lubridate format)
#' @param daterange Date or character. c("aaaa-mm-jj","aaaa-mm-jj")
#'
#' @return 
#' @export
filter_date <- function(data, daterange){
  # if dates are exchanged
  date1 <- ymd(daterange[1])
  date2 <- ymd(daterange[2])
  if (date1<date2){
    start <- date1
    end <- date2
  } else {
    start <- date2
    end <- date1
  }
  # Filter according to dates
  matching_dates <- (data$date>=start & data$date<=end)
  return(data[matching_dates,])
}

########################
# Vacation selection function
########################


#' Filtering dataframe rows by date
#'
#' @param data a dataframe with a "date" column (lubridate format)
#' @param vacance character. "Oui" vacations included
#'                           "Non" vacations not included
#'                           "Seulement les vacances" vacations only
#'
#' @return 
#' @export
filter_vacation <- function(data, vacation){
  if (vacation=='Oui'){
    return(data)
  }  
  
  # Building of the filter
  matching_dates <- rep(FALSE,nrow(data))
  for (i in 1:nrow(vacations)){ 
    temp <- (data$date>=vacations$start_date[i] & data$date<=vacations$end_date[i])
    matching_dates <- matching_dates + temp
  }
  matching_dates <- (matching_dates>0)
  
  if(vacation=="Non"){
    return(data[!matching_dates,])
  }
  if(vacation=="Seulement les vacances"){
    return(data[matching_dates,])
  }
}

########################
# Public holidays selection function
########################


#' Filtering dataframe rows by date
#'
#' @param data a dataframe with a "date" column (lubridate format)
#' @param vacance character. "Oui" public holidays included
#'                           "Non" public holidays not included
#'                           "Seulement les vacances" public holidays only
#'
#' @return 
#' @export
filter_public_holidays <- function(data, JF){
  if (JF=='Oui'){
    return(data)
  }  
  d <- substr(as.character(data$date),1,10)
  matching_dates <- rep(0,nrow(data))
  chara <- as.character(public_holidays)
  for (ferie in chara){
    matching_dates <- matching_dates + (ferie==d)
  }
  matching_dates <- (matching_dates>0)
  
  if(JF=="Non"){
    return(data[!matching_dates,])
  }
  if(JF=="Seulement les jours fériés"){
    return(data[matching_dates,])
  }
}

########################
# Filter function
########################


#' Filter by selected criteria. 
#' Not all criteria need to be filled in. Unfilled criteria are set by default so that no filtering is performed.
#'
#' @param data Raw data. See the "importation" function in the '2-import.R' file
#' @param capteur character. Name of desired sensor
#' @param sens character. Direction of the street: " " or "_lft" or "_rgt"
#' @param mobilite character. Type of mobility: c("car","heavy","pedestrian","bike")
#' @param daterange character. Date or character. c("aaaa-mm-jj","aaaa-mm-jj")
#' @param vacance character. With, without, or only with vacation: "Oui" or "Non" or "Seulement les vacances"
#' @param JF character. With, without, or only with public holiday: "Oui" or "Non" or "Seulement les jours fériés"
#' @param SM character. Selected days of the week: c("1","2","3","4","5","6","7") here all days are selected
#'
#' @return 
#' @export
filtrage <- function(data,
                     sensor    = data$segment_id[1],
                     direction = " ",
                     mobility  = c("car","heavy","pedestrian","bike"),
                     daterange = c('2021-01-01', as.character(Sys.Date())),
                     vacation  = "Oui",
                     p_holiday = "Oui",
                     weekdays  = as.character(1:7)
){
  # c("Toute"=" ","B vers A"="_rgt","A vers B" ="_lft")
  # to verify one last time
  
  # sensor
  filtre <- data[ data$segment_id==sensor, ]
  
  # direction and mobility
  S <- trimws(paste0(mobility,direction))
  filtre$total <- apply(filtre[,S], MARGIN = 1 ,FUN = sum)
  
  filtre <- filtre %>%
    filter(wday(date) %in% weekdays) %>% # choice of the weekdays
    filter_date(daterange) %>% # date
    filter_vacation(vacation) %>% # vacation
    filter_public_holidays(p_holiday) # public holidays
  return(filtre)
}



