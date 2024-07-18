##############################################
#                  Module                    #
##############################################
ui_7 <- function(id){
  ns <- NS(id)
  ######### the "Détails statistiques" toggle ######## 
  tagList(
    tags$head(
      tags$script(HTML("
      $(document).ready(function() {
        $('#toggleMethodButton_7').click(function() {
          $('#methodText_7').toggle();
        });
      });
    "))
    ),
  ######### display #########
    column(3,wellPanel(
      selectInput(ns("sensor"),
                  label = "Choix du capteur",
                  choices = NULL),
      selectInput(ns("sens3"), label = "Direction du capteur",
                  choices = c("Toute" = " ", "B vers A" = "rgt", "A vers B" = "lft"),
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
    p("Cet onglet permet de visualiser, pour un capteur, le nombre d'usagers par heure et par km à partir duquel le réseau routier étudié est saturé.
      Pour cela deux graphiques sont proposés."),
    p("- La vitesse (km/h) en fonction du nombre de véhicules par km."),
    p("- Le nombre de véhicules par heure en fonction du nombre de véhicules par km."),
    p("On peut alors observer à partir de combien de véhicules la section de route présente des engorgements ."),
    br(),
    
    p("AVERTISSEMENT :"),
    p("- Les points oranges apparaissent toujours même s'il n'y a pas d'embouteillages."),
    p("- Le calcul conduisant au placement des points n'est pas parfait : ils peuvent être mal placés."),
    p("Il est donc important de lire les deux graphiques car le deuxième permet de savoir si le réseau présente des embouteillages."),
    br(),
    
    actionButton("toggleMethodButton_7", "Détails statistiques", style = "display: block; margin: 0 auto;"),
    div(id = "methodText_7", style = "display: none;",
        h4("Méthode pour tracer les courbes :"),
        p("On commence par filtrer les données selon les sélections de l’utilisateur. 
          On teste plusieurs courbes afin de trouver celles qui envelopperont au mieux le graphique.
          On réalise ce test deux fois : Une première fois pour le haut du graphique et une deuxième fois
          pour le bas. On cherche ensuite le point d'intersection entre les deux droites qui détermine 
          le début des engorgements. Le point d'intersection entre l'axe des abscisses et la droite enveloppant 
          le bas du graphique détermine le seuil d'engorgement total du réseau étudié."),
        br(),
        
        h4("Interprétation des courbes :"),
        p("On a utilisé le modèle de Greenshields.")
    ),
    br(),
    br(),
    
    uiOutput(ns("display"))
  )
}

server_7 <- function(input, output, session, data){
  ns <- session$ns
  
  observe({  # update sensor selection according to import tab
    if (!is.null(data$sensors)){
      names_selected_sensors <- setNames(data$sensors,sensor_names[sensor_ids%in%data$sensors])
      updateSelectInput(session, "sensor", choices = names_selected_sensors)
    }
  })
  
  #--- function application ---
  result <- reactive({
    plot_diagram_envelope(enriched_data = data$data , segment = input$sensor ,date_range = input$date_range,
                          vacation_choice = input$vacation, holiday_choice = input$p_h, weekday_choice = input$wkd,
                          direction_choice = input$sens3)
  })
  
  
  
  #--- output definition ---
  output$plot_diagram_envelope1 <- renderPlot({
    plot(result()[[1]])
  })
  
  output$plot_diagram_envelope2 <- renderPlot({
    plot(result()[[2]])
  })  
  
  output$display <- renderUI(
    if (is.null(data$sensors)){
      p(class="text-center", "Pour afficher le graphique, veuillez sélectionner des capteurs dans l'onglet import.")
    }
    else if (is.null(input$wkd)){
      p("Le graphique est vide pour les critères sélectionnés")
    } else {
      column(width = 9,
             h3("Diagramme fondamentale avec courbe linéaire"),
             plotOutput(ns("plot_diagram_envelope1")),
             p("Ce graphique donne le seuil d'engorgement en veh/km du réseau. Le point de gauche annonce le début des embouteillages tandis que celui de droite 
               donne la saturation totale du réseau."),
             h3("Diagramme fondamentale avec courbe parabolique"),
             plotOutput(ns("plot_diagram_envelope2")),
             p("Ce graphique donne le seuil en veh/h du réseau. Les points ont la même signification que précedemment."),
             p("Si les points noirs dépassent le point orange indiquant la saturation du réseau alors des embouteillages sont obervés."),
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