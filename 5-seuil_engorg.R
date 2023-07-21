

##############################################
#                  Module                    #
##############################################
ui_5 <- function(id){
  ns <- NS(id)
  
  ######### the "Détails statistiques" toggle ######## 
  tagList(
    tags$head(
      tags$script(HTML("
      $(document).ready(function() {
        $('#toggleMethodButton_5').click(function() {
          $('#methodText_5').toggle();
        });
      });
    "))
    ),
    ######### display #########
    column(3, wellPanel(
      selectInput(ns("sensor"),
                  label = "Choix du capteur",
                  choices = NULL),
      selectInput(ns("sens3"), label = "Direction du capteur",
                  choices = c("Toute" = " ", "B vers A" = "_rgt", "A vers B" = "_lft"),
                  selected = "Toute"),
      selectInput(ns("vit"), label = "Choix de la courbe servant à déterminer le seuil",
                  choices = c("Toute", "plus de 10km/h", "plus de 20km/h", "plus de 30km/h", "plus de 40km/h"),
                  selected = "Toute"),
      dateRangeInput(ns("date_range"), "Période",
                     start = "2021-01-01",
                     end = Sys.Date() - days(1),
                     min = "2021-01-01",
                     max = Sys.Date() - days(1)),
      radioButtons(ns("state_threshold"),
                   "Choix du seuil :",
                   selected = "automatique",
                   choices = c("automatique", "manuel"),
                   inline = TRUE),
      uiOutput(ns("threshold"))
    )),
    h3("Seuil d’engorgement :"),
    p("Cet onglet permet de visualiser le pourcentage d’usagers (voitures et poids lourds) arrivant à dépasser 10km/h, 20km/h, 30km/h et 40km/h en fonction du nombre de véhicules sur la route. L’objectif est de pouvoir chercher un changement brusque dans ces courbes pour déterminer un seuil d’apparition des ralentissements. Le programme propose un seuil calculé automatiquement. Ce seuil n’est pas forcément précis, vous pouvez décider d’afficher un seuil manuel."),
    actionButton("toggleMethodButton_5", "Détails statistiques", style = "display: block; margin: 0 auto;"),
    div(id = "methodText_5", style = "display: none;",

        h4("Méthode pour tracer les courbes :"),
        p("On commence par filtrer les données selon les sélections de l’utilisateur. On isole la partie correspondant au pourcentage de conducteur dépassant chaque vitesse. On range les données dans l’ordre croissant du nombre de véhicules (voitures + camions). On a pré-lissé les données à l’aide d’une moyenne glissante d’une amplitude de 50 pour dégager un début tendance (courbe noir du graphique). À partir de cette tendance, on a lissé nos données à l’aide de l’outil geom_smooth de ggplot2. Ces courbes de lissages sont les courbes colorées du graphique."),
        br(),
        h4("Méthode pour trouver le seuil :"),
        p("L’objectif est de déterminer un seuil de rupture dans la courbe de lissage. Pour cela, on utilise un test de Darling Erdös (dérivé du test de CUSUM). La fonction est implémentée dans le Package",
          tags$a(href="https://github.com/ntguardian/CPAT","CPAT"),
          "(Curtis Miller).")
    ),
    br(),
    br(),
    uiOutput(ns("display")),
  )
}


server_5 <- function(input, output, session, data) {
  ns <- session$ns

  observe({  # update sensor selection according to import tab
    if (!is.null(data$sensors)){
      names_selected_sensors <- setNames(data$sensors,sensor_names[sensor_ids%in%data$sensors])
      updateSelectInput(session, "sensor", choices = names_selected_sensors)
    }
  })

  #--- function application ---
  result <- reactive({
    plot_speed(data=data$data, sensor=input$sensor, date_range=input$date_range,direction=input$sens3)
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
    if (input$state_threshold == "manuel") {
      sliderInput(ns("threshold"),
                  label="Valeur du seuil",
                  min=round(min(result()$data$count_car, na.rm = TRUE)),
                  max=round(max(result()$data$count_car, na.rm = TRUE)),
                  value=floor(max(result()$data$count_car, na.rm = TRUE)),
                  step = 1, round = FALSE)
    }
  })

}
