##############################################
#               Functions                    #
##############################################

# Définition du Graphique (character si vide)


plot_eng_react_fct <- function(data, capteur4, daterange5, Vacance4, JF4, SM4) {

  if (is.null(data)) {
    return(NULL)
  }
  
  # Filter data based on selected sensor using the filtrage function
  donnees_filtrees <- filtrage(data, sensor = capteur4, direction = " ", mobility = c("car", "heavy", "pedestrian", "bike"), daterange = daterange5, vacation = Vacance4, p_holiday= JF4, weekdays= SM4)
  
  if (nrow(donnees_filtrees) == 0) {
    return("Pas de données pour la sélection de la période")
  }
  
  # Calculate mean by hour for vitesse, trafic_rgt, and trafic_lft
  vitesse <- donnees_filtrees %>%
    group_by(hour = hour(date)) %>% 
    summarise(Vitesse = mean(v85, na.rm = TRUE))
  
  trafic_rgt <- donnees_filtrees %>%
    group_by(hour = hour(date)) %>%
    summarise(Voiture_BversA = mean(car_rgt + heavy_rgt, na.rm = TRUE))
  
  trafic_lft <- donnees_filtrees %>%
    group_by(hour = hour(date)) %>%
    summarise(Voiture_AversB = mean(car_lft + heavy_lft, na.rm = TRUE))
  
  # Merge data frames
  trafic_horaire <- full_join(trafic_lft, trafic_rgt, by = "hour")
  trafic_horaire <- full_join(trafic_horaire, vitesse, by = "hour")
  
  if (nrow(trafic_horaire) == 0) {
    return("Pas de données pour la sélection de la période")
  }
  
  # Create the plot
  fig <- plot_ly(trafic_horaire, x = ~hour)
  fig <- fig %>% add_trace(y = ~Voiture_BversA, mode = "lines+markers", name = "B vers A", 
                           line = list(color = "blue", dash = "dot"),
                           marker = list(color = "blue"))
  fig <- fig %>% add_trace(y = ~Voiture_AversB, mode = "lines+markers", name = "A vers B", 
                           line = list(color = "blue", dash = "dash"),
                           marker = list(color = "blue"))
  ay <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = "Vitesse v85 moyenne (km/h)"
  )
  fig <- fig %>% add_trace(y = ~Vitesse, yaxis = "y2", mode = "lines+markers", name = "Vitesse v85 moyenne", 
                           line = list(color = "red"),
                           marker = list(color = "red"))
  fig <- fig %>% layout(
    title = "", yaxis2 = ay,
    xaxis = list(title = "Heure"),
    yaxis = list(title = "Nombre de véhicules moyen")
  ) %>%
    layout(plot_bgcolor = '#e5ecf6',
           xaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff',
             dtick = 1
           ),
           yaxis = list(
             tickfont = list(color = "blue"),
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'
           )
    )
  
  list(fig = fig, Donnee = trafic_horaire)
}



##############################################
#                  Module                    #
##############################################
ui_4 <- function(id){
  ns <- NS(id)
  tagList(
    column(3,wellPanel(
      selectInput(ns("sensor"),
                  label = "Choix du capteur",
                  choices = NULL),
      
      dateRangeInput(ns("daterange5"), "Période",
                     start  = "2021-01-01",
                     end    = Sys.Date()-days(1),
                     min    = "2021-01-01",
                     max    = Sys.Date()-days(1)),
      
      radioButtons(inputId = ns("Vacance4"), label = "Vacances comprises :",
                   choices = c("Oui","Non","Seulement les vacances"),selected = "Non"),
      radioButtons(inputId = ns("JF4"), label = "Jours fériés compris :",
                   choices = c("Oui","Non","Seulement les jours fériés"),selected = "Non"),
      checkboxGroupInput(
        ns("SM4"),
        "Choix des jours",
        selected = 1:5,
        choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
        choiceValues = 1:7,
        inline = TRUE
      ),
      actionButton(ns("mise_a_j"), "Mettre à jour")
    )),
    uiOutput(ns("resultat"))
  )
}

server_4 <- function(input, output, session, data){
  ns <- session$ns
  
  # pour mettre a jour le choix des capteurs selon liste_capteur
  observe({
    updateSelectInput(session, "sensor", choices = data$sensors)
  })
  
  plot_heure_eng <- reactiveVal()
  
  observeEvent(input$mise_a_j,{
    plot_heure_eng(plot_eng_react_fct(data$data, input$sensor, input$daterange5, input$Vacance4, input$JF4, input$SM4))
  })
  
  output$graph <- renderPlotly({plot_heure_eng()$fig})
  
  output$resultat <- renderUI(
    if (is.null(plot_heure_eng())){return("Import necessaire")} 
    # else if(mode(plot_heure_eng()$fig)=="character"){return()} 
    else {
      column(width = 9,
             h3("Quelle est l'heure d'engorgement ?"),
             plotlyOutput(ns("graph")),
             p("La vitesse V85 est la vitesse telle que 85% des usagers roulent plus lentement que celle ci."),
             downloadButton(ns("downloadHeure"), "Import des données")
      )
    }
  )
  
  # Affiche le bouton de téléchargement des données si tout va bien
  output$downloadHeure <- renderUI(
    downloadHandler(
      filename = "Heure_engorgement.csv", # Nom du fichier importé
      content = function(file) {
        write_excel_csv2(plot_heure_eng()$Donnee, file)
      }
    )
  )
  
}

