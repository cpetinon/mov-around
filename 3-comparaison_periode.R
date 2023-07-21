generate_comparison_section <- function(ns, label_color, ns_date, ns_vac, ns_ph, ns_wkd, label_text) {
  tagList(
    h2(span(style = paste("color:", label_color), label_text)),
    dateRangeInput(ns_date, "Période",
                   start  = "2021-01-01",
                   end    = Sys.Date() - days(1),
                   min    = "2021-01-01",
                   max    = Sys.Date() - days(1)),
    radioButtons(inputId = ns_vac, label = "Vacances comprises :",
                 choices = c("Oui", "Non", "Seulement les vacances"), selected = "Oui"),
    radioButtons(inputId = ns_ph, label = "Jours fériés compris :",
                 choices = c("Oui", "Non", "Seulement les jours fériés"), selected = "Oui"),
    checkboxGroupInput(
      inputId = ns_wkd,
      label = "Choix des jours",
      selected = 1:7,
      choiceNames = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"),
      choiceValues = 1:7,
      inline = TRUE
    )
  )
}

##############################################
#                  Module                    #
##############################################

ui_3 <- function(id) {
  ns <- NS(id)
  tagList(
    ######### the "Détails statistiques" toggle ######## 
    tags$head(
      tags$script(HTML("
      $(document).ready(function() {
        $('#toggleMethodButton_3').click(function() {
          $('#methodText_3').toggle();
        });
      });
    "))
    ),
    ######### display #########
    column(3, wellPanel(
      selectInput(ns("sensor"),
                  label = "Choix du capteur",
                  choices = NULL),
      selectInput(ns("sens"), label = "Direction",
                  choices = c("Toute" = " ", "B vers A" = "_rgt", "A vers B" = "_lft"),
                  selected = "Toute"),
      checkboxGroupInput(
        ns("mobilite"),
        "Choix du type de mobilité",
        selected = c("car", "heavy"),
        choiceNames = c("VL", "PL", "Piéton", "Vélo"),
        choiceValues = c("car", "heavy", "pedestrian", "bike")
      ),
      generate_comparison_section(ns, "#006bb6", ns("date_range1"), ns("vac1"), ns("p_h1"), ns("wkd1"), "Période de référence"),
      generate_comparison_section(ns, "#ff5900", ns("date_range2"), ns("vac2"), ns("p_h2"), ns("wkd2"), "Première période de comparaison"),
      generate_comparison_section(ns, "#00b308", ns("date_range3"), ns("vac3"), ns("p_h3"), ns("wkd3"), "Seconde période de comparaison")
    )),
    h3("Comparaison de périodes :"),
    p("Cet onglet permet de voir, pour un même capteur, s’il y a une différence de comportement
               des usagers entre périodes différentes. Pour cela, vous devez choisir les caractéristiques
               d’une période de référence et d’une ou deux périodes avec lesquelles vous voulez les comparer."),
    p(" Les différences s’observent avec 2 outils :"),
    p("- Un graphique montrant la circulation moyenne en fonction des heures de la journée
               pour chaque période."),
    p("- Une barre colorée, donnant pour chaque heure, le résultat d’un test statistique pour
                 qualifier la différence de répartition entre 2 périodes. Pour chaque créneau horaire, la couleur indique si les flux entre la période de référence et la période comparée sont similaires."),
    div(
      style = "text-align: center;",
      HTML("<ul>
         <li> Si le résultat est <i>Significatif</i>, c'est qu'il y a très probablement un changement de comportement entre les deux périodes (pour l'heure concernée).</li>
         <li> Si le résultat est <i>Entre deux</i>, alors il est possible qu'il y ait une différence. </li>
         <li> Si le résultat est <i>Non-significatif</i>, on ne peut pas dire qu'il y ait une différence.</li>
       <ul>")
    ),
    
    actionButton("toggleMethodButton_3", "Détails statistiques", style = "display: block; margin: 0 auto;"),
    div(id = "methodText_3", style = "display: none;",
        h4("Méthode pour tracer les courbes :") ,
        p("Selon la sélection de l’utilisateur, on filtre les données pour ne garder que le trafic correspondant aux mobilités, capteur, direction et contraintes de dates sélectionnés. On réalise ensuite une moyenne pour chaque créneau horaire.
          On a rajouté un intervalle de confiance à 95% autour de nos courbes. Pour chaque créneau horaire, on a estimé la variance des données, ce qui nous a permis d’obtenir l’intervalle de confiance (à partir d’une loi de Student)."),
        br(),
        h4("Méthode pour la significativité de la différence :"),
        p("On s’appuie sur un test de Wilcoxon Mann Whitney. L’idée est de comparer, pour chaque créneau horaire, la répartition des valeurs de chacune des périodes. Le test consiste à regarder la distance entre les fonctions de répartition empirique, si elles sont éloignées, le test rejette l’hypothèse nulle :  l’égalité des lois. L’option « Significatif » indique une p-value inférieure à 0.05, celle « Entre deux » une  p-value entre 0.05 et 0.1 et celle « Non significatif » une p-value supérieure à 0.1."),
    ),
    br(),
    br(),
    uiOutput(ns("display"))
    

  )
}

server_3 <- function(input, output, session, data){
  ns <- session$ns

  observe({ # update sensor selection according to import tab
    if (!is.null(data$sensors)){
      names_selected_sensors <- setNames(data$sensors,sensor_names[sensor_ids%in%data$sensors])
      updateSelectInput(session, "sensor", choices = names_selected_sensors)
    }
  })

  #--- parameters ---
  param_general <- reactive({ # parameters needed for all periods
    list(data=data$data, sensor = input$sensor, direction = input$sens,mobility = input$mobilite)
  })
  
  param_ref <- reactive({ # parameters of the reference period
    list(date_range = input$date_range1,vac = input$vac1, wkd = input$wkd1,p_h = input$p_h1) # wkd = weekdays, p_h = public_holiday
  })
  
  param_1 <- reactive({ # parameters of the first comparison period
    list(date_range = input$date_range2,vac = input$vac2,wkd = input$wkd2,p_h = input$p_h2)
  })
  
  param_2 <- reactive({ # parameters of the second comparison period
    list(date_range = input$date_range3,vac = input$vac3,wkd = input$wkd3,p_h = input$p_h3)
  }) 

  #--- function application ---
  result1 <- reactive({
    plot_comparaison(c(param_ref(),param_general()), c(param_1(),param_general()), c("#ff5900", "#006bb6"))
  })
  
  result2 <- reactive({
    plot_comparaison(c(param_ref(),param_general()), c(param_2(),param_general()), c("#00b308", "#006bb6"))
  })
  
  #--- output definition ---
  output$graph1 <- renderPlot({
    result1()$graph
  })
  
  output$graph2 <- renderPlot({
    result2()$graph
  })

  output$display <- renderUI(
    if (is.null(data$sensors)){
      p(class="text-center","Pour afficher les graphiques, veuillez sélectionner des capteurs dans l'onglet import")
    } else if (is.null(input$mobilite)|is.null(input$p_h1)|is.null(input$p_h2)|is.null(input$p_h3)|is.null(input$wkd1)|is.null(input$wkd2)|is.null(input$wkd3)){
      p("Le graphique est vide pour les critères sélectionnés")
    } else {
      column(width = 9,
           h2("Comparaison avec la première période"),
           plotOutput(ns("graph1")),
           h2("Comparaison avec la seconde période"),
           plotOutput(ns("graph2")),
           HTML("Nombre moyen de valeur par heure pour la période de référence :", result1()$count_ref,'<br/>',
             "Nombre moyen de valeur par heure pour la période de 1 :", result1()$count_p,'<br/>',
             "Nombre moyen de valeur par heure pour la période de 2 :", result2()$count_p,'<br/>'),
          downloadButton(ns("downloadData"), "Import des données"),
    )
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = "Comparaison_periode.csv", # file name that will be written
    content = function(file) {
      write_excel_csv2(data.frame(cbind(ref = result1()$data_mean_ref,
                                        P_1 = result1()$data_mean_ref,
                                        P_2 = result2()$data_mean_compar)), file)
    }
  )
}
