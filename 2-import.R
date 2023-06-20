##############################################
#               Functions                    #
##############################################


########################
# Retrieve the data associated to a sensor
########################


#'  Retrieve the data associated to a sensor from the Telraam API
#'
#' @param nom Character. Name of the sensor, comes from the vector listeNom
#' @param date1 Date. Start date "aaaa-mm-jj"
#' @param date2 Date. End date "aaaa-mm-jj"
#'
#' @return Data from the API, from date1 to date2. date1 and date2 are included.
#' @export
#' @examples
#' retrieve_sensor('Burel', ymd('2021-05-21'),ymd('2021-05-30'))
retrieve_sensor <- function(nom,date1,date2){
  # Initialization
  result <- data.frame() 
  date2 <- date2 + days(1) # so that date2 is included
  idCapteur <- listeSegments[which(listeNom==nom)] # retrieve the identifier associated to the name of the sensor
  dates <- seq_by_3_month(date1,date2) # for the iteration of the retrieving, because when we call the API, the period can not exceed 3 months for each call
  
  # calling of the API
  resTraffic_list <- pmap(list(dates$debut, dates$fin), ~ {
    resTraffic <- POST("https://telraam-api.net/v1/reports/traffic", 
                       add_headers("X-Api-Key" = key),
                       body = paste0('{
                       "level": "segments",
                       "format": "per-hour",
                       "id": "', idCapteur, '",
                       "time_start": "', ..1, '",
                       "time_end": "', ..2, '"
                     }'))
    
    content <- resTraffic$content %>%
      rawToChar() %>%
      fromJSON()
    df <- content$report
    df$date <- ymd_hms(df$date, tz = df$timezone[1])
    df
  })
  
  if (length(result$date)!=0){
    result$date <- ymd_hms(result$date, tz = result$timezone[1]) # We change the class of date with a time difference of 2
    result$segment_id <- rep(nom,nrow(result)) # we remplace the identifier by the name of the sensor
    
    # we select the data that we don't consider null (arbitrary choice)
    result <- result %>% filter(uptime > 0.5,
                                heavy_lft + car_lft + pedestrian_lft + bike_lft +
                                heavy_rgt + car_rgt + pedestrian_rgt + bike_rgt >0
    )
  }
  return(result)
}


########################
# Write the database of one sensor: creation/update
########################


#'  Write the database in the data folder one sensor. This function does both creation and update.
#'
#' @param nom Character. Name of the sensor, comes from the vector listeNom.
#' @param date1 Date. Start date "aaaa-mm-jj"
#' @param date2 Date. End date "aaaa-mm-jj"
#'
#' @return
#' @export
#' @examples
#' write_update_data('Burel', ymd('2021-05-21'),ymd('2021-05-30'))
write_update_data <- function(nom, date1, date2){
  # Preparation of the dataset
  data <- retrieve_sensor(nom,date1, date2)
  # conversion from a numeric vector to a character string of car_speed_hist_0to70plus and car_speed_hist_0to120plus
  data$car_speed_hist_0to70plus <- sapply(data$car_speed_hist_0to70plus, function(x) paste(x, collapse = ", "))
  data$car_speed_hist_0to120plus <- sapply(data$car_speed_hist_0to120plus, function(x) paste(x, collapse = ", "))
  
  file_name <- paste0("data/",listeNombis[which(listeNom==nom)],".csv")
  
  if (!is.null(data)){
    if (file.exists(file_name)){
      cleaning <- read_csv2(file_name)
      data <- rbind(cleaning,data)
      data <- data[!duplicated(data$date),] # if some lines are repeated they are eliminated
    }
    write_csv2(data, file = file_name)
  }
}

########################
# Sequence generation that cut a period by 3 months
########################


#' Sequence generation that cut a period by 3 months. Only used in the function retrieve_sensor.

#'
#' @param date1 Date. Start date "aaaa-mm-jj"
#' @param date2 Date. End date "aaaa-mm-jj"
#'
#' @return A dataframe with two columns. One row correspond to one interval of three months.
#' @export
#' @examples
#' seq_by_3_month(ymd('2020-05-21'),ymd('2021-05-30'))
seq_by_3_month <- function(date1, date2){
  if (date1==date2){
    return(data.frame(start = date1, end = date1))
  }else{
    date <- seq(from = date1, to = date2, by = "3 month")
    if (date[length(date)]!=date2){
      date <- c(date,date2)
    }
    return(data.frame(start = date[1:(length(date)-1)],
                      end   = date[2:length(date)]))
  }
}

########################
# Convert a numeric vector into a character string
########################


#' Convert a character string into a numeric vector
#' @param vector Something in the shape "10,20,30"
#'
#' @return Numeric vector. Something in the shape c(10,20,30)
#' @export
#' @examples
#' convert_string_to_list("10,20,30")
convert_string_to_list <- function(vector){
  convert <- as.list(vector)
  lapply(vector, function(x) {
    elements <- unlist(strsplit(x, ",\\s*"))
    numeric_elements <- as.numeric(elements)
    return(numeric_elements)
  })
}

########################
# Importation of data associated to a list of sensor
########################


#' Importation of data associated to a list of sensor
#' @param character c("Burel","Leclerc",...)
#'
#' @return data.frame
#' @export
#' @examples
#' importation(c("Burel","Leclerc"))
importation <- function(list_sensor_name){
  list_sensor <- listeNombis[which(listeNom %in% list_sensor_name)]
  data <- data.frame()
  data <- map_dfr(list_sensor, ~ {
    file <- paste0('data/', .x, '.csv')
    if (file.exists(file)) {
      read_csv2(file)
    } else {
      NULL
    }
  })
  data$car_speed_hist_0to70plus <-  convert_string_to_list(data$car_speed_hist_0to70plus)
  data$car_speed_hist_0to120plus <- convert_string_to_list(data$car_speed_hist_0to120plus)
  data$date <- ymd_hms(data$date)
  return(data)
}


##############################################
#                  Module                    #
##############################################
ui_2 <- function(id){
  ns <- NS(id)
  tagList(h2("Les données"),
          p("L'application importe des données depuis le site de Telraam :",
            tags$a(href="https://telraam.net/#14/48.1014/-1.3862","ici"),
            ". L'import de toute les données est relativemant lourd et peut prendre du temps. Veuillez donc ne selectionner que les données que vous utiliserez :"), 
          fluidRow(
            column(5,
                   checkboxGroupInput(
                     inputId=ns("Capteurs"),
                     label="Choix des capteurs",
                     choiceNames = listeNombis,
                     choiceValues = listeNom
                   ),
                   textOutput(ns("import_state")),
                   downloadButton(ns("downloadData"), "Import des données")),
            column(7,
                   tags$img(src="carte capteur apli.jpg",height=500),
                   textOutput(ns("existence_key")),
                   actionButton(ns("update"), "Mettre à jour les données"),
                   textOutput(ns("update_state"))
            )
          )
          
  )
}

server_2 <- function(input, output, session){
  ns <- session$ns
  
  #################################
  #          Importation          #
  #################################
  # Initialization of the reactive
  data <- reactiveValues(
    sensors = NULL,
    data = NULL,
    state = "Les données sont vides"
  )
  
  
  # Update of data reactive
  observe({
    data$sensors <- input$Capteurs
    if (!is.null(input$Capteurs)){
      data$data <- NULL # so to erase the former sensors
      data$data <- importation(input$Capteurs)
      data$state <- paste("Les capteurs importés sont: ",
                          paste(input$Capteurs,collapse=', '))
    } else {
      data$state <- "Les données sont vides"
    }
  })
  
  
  output$import_state <- renderText({data$state})
  
  # For downloading the imported data
  output$downloadData <- downloadHandler(
    filename = "Donnee_brute.csv",
    content = function(file) {
      write_excel_csv2(data$data, file)
    }
  )
  
  
  #################################
  #            Update             #
  #################################
  
  ## Initialization of the update reactive 
  
  if (file.exists("data/date.txt")){ # When the database already exists
    
    date <- readLines("data/date.txt")
    
    # to see if an update is possible
    if (ymd(date) <  today){
      action <- ", une mise à jour est possible"
    } else {
      action <- ", la base de donnée est à jour"
    }
    
    update <- reactiveValues(
      state = paste0("Les données vont du 2021-01-01 au ", date, action) ,
      date = date,
      key = NULL
    )
    
  } else { # When the database is empty
    update <- reactiveValues(
      state = "La base de données est vide, veuillez mettre à jour les données",
      date = "2021-01-01", # the database begins the 2021-01-01
      key = NULL
    )
  }
  
  # Update of the database reactif after trigger of the update button
  observeEvent(input$update,{
    date <- ymd(update$date)
    
    # determination of if an update is possible, otherwise nothing happened
    if (date < today){
      
      # verification of the existence of the API's key
      if (is.null(key)){
        update$key <- "La cle pour appeler l'API n'existe pas :( "
      } else {
        
        
        # the update is launched
        withProgress(message = "Mise à jour de", value = 0,{ # progress bar
          incProgress(0, detail = paste("Capteurs"))
          k <- 2
          for (nom in listeNom) { # iteration on all sensors
            incProgress(1/(length(listeNom)+1), detail = nom) # incrementation of the progress bar
            k <- k+1
            yesterday <- today - days(1) # ! today is excluded !
            write_update_data(nom, date1 = date, date2 = yesterday) 
          }
        })
        
        # update of the date of the next update
        writeLines(as.character(today), con = "data/date.txt")
        update$date <- today
        
        # update of the state of the database
        update$state <- paste0("Les données vont du 2021-01-01 au ", today - days(1),", la base de donnée a été mise à jour, veuillez redémarrer l'application")
      }
    }
  })
  
  output$update_state <- renderText({
    update$state
  })
  
  output$existence_key <- renderText({
    update$key
  })
  
  # Return value of the module
  return(data)
}
                                           
