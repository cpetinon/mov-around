##############################################
# Les fonctions
##############################################

convert_string_to_list <- function(vector){
  convert <- as.list(vector)
  lapply(vector, function(x) {
    elements <- unlist(strsplit(x, ",\\s*"))
    numeric_elements <- as.numeric(elements)
    return(numeric_elements)
  })
}

importation <- function(list_captor_nom,daterange){
  list_captor <- listeNombis[which(list_captor_nom==listeNom)]
  data <- data.frame() # le resultat a renvoyer
  for (captor in list_captor){
    data_temp <- read_csv2(paste0('data/',captor,".csv"))
    data <- rbind(data,data_temp)
  }
  data$car_speed_hist_0to70plus <-  convert_string_to_list(data$car_speed_hist_0to70plus)
  data$car_speed_hist_0to120plus <- convert_string_to_list(data$car_speed_hist_0to120plus)
  data$date <- ymd_hms(data$date)
  data <- data[data$date > ymd(daterange[1]),]
  data <- data[data$date < ymd(daterange[2]),]
  return(data)
}


##############################################
# Le module
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
                   dateRangeInput(ns("daterange"), "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   actionButton(ns("go_import"), "Import"),
                   textOutput(ns("state"))),
            column(7,
                   tags$img(src="carte capteur apli.jpg",height=500)))
          
  )
}

server_2 <- function(input, output, session){
  ns <- session$ns
  
  # Initialization of the reactives
  data <- reactiveValues(
    captors = NULL,
    data = NULL,
    state = "Les données sont vides"
  )
  
  # Update of the reactives after triger of the import button
  observeEvent(input$go_import, {
    data$captors <- input$Capteurs
    data$data <- importation(input$Capteurs,input$daterange)
    
    if (!is.null(input$Capteurs)){
      data$state <- paste("Les capteurs importés sont: ",
                          paste(input$Capteurs,collapse=', '))
    }
    
  })
  
  # Output of the imported captors
  output$state <- renderText({data$state})
  
  # Return value of the module
  return(data)
}