##############################################
#                  Module                    #
##############################################
ui_7 <- function(id){
  ns <- NS(id)
  tagList(
    column(3,wellPanel(
      selectInput(ns("sensor"),
                  label = "Choix du capteur",
                  choices = NULL),
      selectInput(ns("sens3"), label = "Direction du capteur",
                  choices = c("Toute" = " ", "B vers A" = "_rgt", "A vers B" = "_lft"),
                  selected = "Toute"),
      dateRangeInput(ns("date_range"), "Période",
                     start  = starting_date,
                     end    = ending_date-days(1),
                     min    = starting_date,
                     max    = ending_date-days(1)),
      radioButtons(inputId = ns("vacation"), label = "Vacances comprises :",
                   choices = c("Oui"="YES", "Non"="NO", "Seulement les vacances" = "ONLY"), selected = "NO"),
      radioButtons(inputId = ns("p_h"), label = "Jours fériés compris :",
                   choices = c("Oui"="YES", "Non"="NO", "Seulement les jours fériés" = "ONLY"), selected = "NO"),
      checkboxGroupInput(
        ns("wkd"),
        "Choix des jours",
        selected = 1:5,
        choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
        choiceValues = 1:7,
        inline = TRUE
      )
    )),
    
    h3("Seuil d'engorgement :"),
    p("Cet onglet permet de visualiser, pour un capteur, le seuil d'engorgement. 
          Pour cela deux graphiques sont proposés : "),
    p("- Un graphique montrant la vitesse (km/h) en fonction du nombre de véhicules par km."),
    p("- Un graphique montrant le nombre de véhicules par heure en fonction du nombre de véhicules par km."),
    p("Remarque : Il est important de lire les deux graphiques afin d'avoir les informations nécessaires.
      En effet, les informations dont fournies même s'il n'y a pas d'engorgement sur le réseau."),
    
    p("On peut alors observer à partir de combien de véhicules la section de route présente des engorgements ."),
    p("Le premier graphique indique à partir de combien de véhicules par km il commence 
        à y avoir des engorgements et quand est ce que le réseau est complétement saturé. 
        (2 points sur les droites)"),
    p("Le deuxième graphique indique à partir de combien de véhicules par heure il commence 
        à y avoir des engorgements et quand est ce que le réseau est complétement saturé. 
        (2 points sur les paraboles"),
    p("Le deuxième graphique nous permet de savoir si les indications apportées 
        par le premier graphique sont exploitables. 
        Si les points noirs dépassent le point orange indiquant la saturation du réseau en 
        véhicules par heure alors les informations ont une signification sinon il semblerait 
        qu'il y ait une réduction de la vitesse sans saturation complète du réseau routier étudié."),
    
    #A AJOUTER A LA BONNE PLACE (lorsque le capteur est un v1)
    p("Vous avez choisi une direction. Malheureusement elle n'est pas disponible pour ce segment."),
    p("Les graphiques affichés tiennent donc du nombre total de véhicules sur le réseau"),
    p("Il est donc normal de ne pas forcément oberserver d'engorgement malgré la réalité 
      car en général il se réalise dans un seul sens de circulation. Ici, il y a une moyenne 
      entre les deux sens de circulation, ce qui peut fausser les résultats "),
    br(),
    uiOutput(ns("display"))
  )
}

server_7 <- function(input, output, session, data) {
  ns <- session$ns
  
  observe({  # update sensor selection according to import tab
    if (!is.null(data$sensors)){
      names_selected_sensors <- setNames(data$sensors,sensor_names[sensor_ids%in%data$sensors])
      updateSelectInput(session, "sensor", choices = names_selected_sensors)
    }
  })
  
  #--- function application ---
  result <- reactive({
    plot_diagram_envelope()
  })
  
  #--- output definition ---
  output$plot_s <- renderPlot({
    plot_threshold(result(),selected_speed = input$vit, state_threshold = input$state_threshold,threshold=input$threshold)
  })
  
  output$display <- renderUI({
    if (is.null(data$sensors)) {
      p(class="text-center","Pour afficher le graphique, veuillez sélectionner des capteurs dans l'onglet import.")
    } else if (nrow(data$data) < 100) { # if there is not enough data to calculate a threshold
      p("Attention : période trop courte ou pour laquelle le capteur ne possède pas de données !")
    } else {
      column(width = 9,
             h5("Précisions sur le chartique"),
             p("Le chartique suivant indique pour chaque courbe le pourcentage de conducteurs (véhicules légers
               et poids lourds) qui arrivent à dépasser la vitesse spécifiée en fonction du nombre d'autres
               conducteurs sur la route durant une même période horaire."),
             p("Un changement brusque dans les courbes peut indiquer une présence régulière d'embouteillage
               lorsqu'on dépasse la valeur du changement. La barre rouge indique cette valeur."),
             br(),
             p("Avertissement :"),
             p("1. La barre apparait toujours, même pour les routes sans embouteillages."),
             p("2. Le calcul conduisant au placement de la barre n'est pas parfait : elle peut être mal placée."),
             plotOutput(ns("plot_s")),
             downloadButton(ns("downloadbrut"), "Import des données des courbes brutes (noire)")
      )
    }
  })
  
  
  output$downloadbrut <- downloadHandler(
    filename = "Courbes_brutes.csv",
    content = function(file) {
      write_excel_csv2(result()$data, file)
    }
  )
  
  output$threshold <- renderUI({
    if (input$state_threshold == "manual") {
      sliderInput(ns("threshold"),
                  label="Valeur du seuil",
                  min=round(min(result()$data_plot$count, na.rm = TRUE)),
                  max=round(max(result()$data_plot$count, na.rm = TRUE)),
                  value=floor(max(result()$data_plot$count, na.rm = TRUE)),
                  step = 1, round = FALSE)
    }
  })
  
}
