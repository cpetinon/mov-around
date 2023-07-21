
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
      dateRangeInput(ns("date_range"), "Période",
                     start  = "2021-01-01",
                     end    = Sys.Date()-days(1),
                     min    = "2021-01-01",
                     max    = Sys.Date()-days(1)),
      radioButtons(inputId = ns("vacation"), label = "Vacances comprises :",
                   choices = c("Oui","Non","Seulement les vacances"),selected = "Non"),
      radioButtons(inputId = ns("p_h"), label = "Jours fériés compris :",
                   choices = c("Oui","Non","Seulement les jours fériés"),selected = "Non"),
      checkboxGroupInput(
        ns("wkd"),
        "Choix des jours",
        selected = 1:5,
        choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
        choiceValues = 1:7,
        inline = TRUE
      )
    )),
    
    h3("Heure d'engorgement :"),
    p("Cet onglet permet de visualiser, pour un capteur, la circulation dans chaque direction
                 (voitures et poids lourds) et la vitesse V85 (vitesse telle que 85 % des usagers roulent 
                 plus lentement), en fonction des heures de la journée. On peut alors observer à quelle heure aparaissent les ralentissements."),
    br(),
    uiOutput(ns("display"))
  )
}

server_4 <- function(input, output, session, data){
  ns <- session$ns

  observe({  # update sensor selection according to import tab
    if (!is.null(data$sensors)){
      names_selected_sensors <- setNames(data$sensors,sensor_names[sensor_ids%in%data$sensors])
      updateSelectInput(session, "sensor", choices = names_selected_sensors)
    }
  })
  
  #--- function application ---
  result <- reactive({
    plot_hour_threshold(data = data$data, sensor = input$sensor, date_range = input$date_range, vac = input$vacation, p_h = input$p_h, wkd = input$wkd)
  })
  
  #--- output definition ---
  output$plot_hour_threshold <- renderPlot({
    result()$graph
  })

  output$display <- renderUI(
    if (is.null(data$sensors)){
      p(class="text-center", "Pour afficher le graphique, veuillez sélectionner des capteurs dans l'onglet import.")
    }
    else if (is.null(input$wkd)){
     p("Le graphique est vide pour les critères sélectionnés")
    } else {
      column(width = 9,
             h3("Quelle est l'heure d'engorgement ?"),
             plotOutput(ns("plot_hour_threshold")),
             p("La vitesse V85 est la vitesse telle que 85% des usagers roulent plus lentement que celle ci."),
             downloadButton(ns("downloadHeure"), "Import des données")
      )
    }
  )

  output$downloadHeure <- downloadHandler(
      filename = "Heure_engorgement.csv", # Nom du fichier importé
      content = function(file) {
        write_excel_csv2(result()$data, file)
      }
    )
}
