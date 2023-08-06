


##############################################
#                  Module                    #
##############################################
ui_2 <- function(id){
  ns <- NS(id)
  tagList( # allows the module to return several objects instead of just one
    ######## css parameter to adjust image size ########
    tags$head(
      tags$style(HTML("
        #full-width-image {
        width: 100%;
        }
      "))
    ),
    
    ######### display #########
    tags$img(id = "full-width-image", src = "images/chateaubourg2.png"),
    br(), # return to line
    br(),
    h2("Les données"),
    br(),
    ########
    # Sensors choices part
    ########
    h3("Choix des capteurs"),
        p(class="text-center",
      "Afin de pouvoir utiliser les outils d'analyse fournis par cette application, veuillez cocher les capteurs qui vous intéressent."),
    br(),
    fluidRow(
          column(2),
          column(4,
                 br(),
                 checkboxGroupInput(
                   inputId=ns("Capteurs"),
                   label="Liste des capteurs disponibles :",
                   choiceNames = sensor_names,
                   choiceValues = sensor_ids
                 ),
                 textOutput(ns("import_state"))),            
          column(3, class = "text-center",
                 h4('Carte des capteurs de Chateaubourg'),
                 p("Un numéro correspond à un capteur."),
                 tags$img(src="images/carte capteur apli.jpg",height=400),
          column(3)
                 
          )
    ),
    
    br(),
    br(),
    
    ########
    # Simple chart part
    ########
    uiOutput(ns('display_simple_plot')),
    br(),
    
    ########
    # Missing data part
    ########
    h3(class="text-center","Les valeurs manquantes"),
    p(class="text-center",'à compléter'),
    h4("Avertissement relatif à la qualité des données :"),
    p(class="text-center","Les données des capteurs Telraam ne sont pas issues d’une mesure continue sur une heure. Pour améliorer la qualité des données futures,
    les capteurs dédient une partie de leur temps d’activité à l’apprentissage. Les données totales sont reconstituées à partir du temps de mesure.
    Plus cette période de mesure est longue plus la qualité des données est grande. Telraam donne un outil de mesure de ce temps de mesure : l’uptime.
    Dans cette application, nous n'avons conservé que les données d’uptime supérieur à 0.5 (seuil conseillé par Telraam). 
    Toutefois, les capteurs placés récemment (en période d’apprentissage) et les données matinales ou dans la soirée (visibilité réduite à cause de la nuit)  peuvent présenter des uptimes plus faible. 
    De plus la suppression des données à l’uptime trop faible fait qu’on possède moins de données pour les périodes à risque. La qualité des estimations
    et des tests est moins bonne sur ces périodes. Il faut donc être prudent en interprétant ces données."),
    br(),
    
    ########
    # Warning part
    ########
    h3("Avertissement relatif aux catégories de mobilités :"),
    p(class = "text-center","Les capteurs Telraam utilisés ont des difficultés à différencier les grosses voitures comme les SUV des poids lourds. Le nombre de poids lourds est donc sur-évalué et le nombre de voitures sous-évalué. Toutefois, le total voitures + camions est précis.
    De la même façon, il faut être prudent dans la différenciation entre vélos et piétons."),
    br(),
    p(class = "text-center",
      "Pour plus d'informations, veuillez consulter",
      tags$a(href="https://telraam.net/#14/48.1014/-1.3862","le site Telraam"),"."),    
            
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
    data = tibble()
  )
  
  
  # Update of data reactive
  observe({
    if (is.null(input$Capteurs)){
      output$import_state <- renderText({"Pas de capteurs sélectionnés"})
      
    } else {
      # if truc pas NULL alors keep
      add <- setdiff(input$Capteurs, data$sensors) %>% import_sensor()
      data$data <- data$data %>% rbind(add) %>% # add new sensors
                                 filter((segment_id %in% input$Capteurs)) # remove sensors that are not selected anymore
      output$import_state <- renderText(paste("Les capteurs importés sont: ", paste(sensor_names[sensor_ids%in%input$Capteurs],collapse=', ')))
      data$sensors <- input$Capteurs
    }
  })
  
  
  
  # For downloading the imported data
  output$downloadData <- downloadHandler(
    filename = "Donnee_brute.csv",
    content = function(file) {
      write_excel_csv2(data$data, file)
    }
  )
  
  #################################
  #            Plot               #
  #################################
  
  observe({ # update sensor selection according to import tab
    if (!is.null(data$sensors)){
      names_selected_sensors <- setNames(data$sensors,sensor_names[sensor_ids%in%data$sensors])
      updateSelectInput(session, "sensor_plot", choices = names_selected_sensors)
    }
  })
  
  output$display_simple_plot <- renderUI({
    if (!is.null(data$sensors)){
      fluidRow(
        h3("Les capteurs sélectionnés"),
        column(3),
        column(3,
               selectInput(ns("sensor_plot"),
                           label = "Choix du capteur à tracer",
                           choices = NULL)
        ),
        column(3,
               dateRangeInput(ns("date_range_simple_plot"), "Période",
                              start  = starting_date,
                              end    = ending_date - days(1),
                              min    = starting_date,
                              max    = ending_date - days(1))
        ),
        column(3),
        plotOutput(ns('simple_plot')),
        br(),br(),br(),br(),
        column(2 , offset = 10,
               downloadButton(ns("downloadData"), "Import des données")
        )
      )
    }
  })
  
  output$simple_plot <- renderPlot({
    simple_plot(data$data,sensor=input$sensor_plot,date_range = input$date_range_simple_plot)
  })
  
  
  
  # Return value of the module
  return(data)
}
