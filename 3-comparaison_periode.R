##############################################
#               Functions                    #
##############################################

ord_txt_fct <- function(mobilite){
  # Récupération des termes en français
  fr <-  c("Véhicules Légers","Poids Lourds","Piétons","Vélos")
  eng <-  c("car","heavy","pedestrian","bike")
  mobfr <- fr[eng %in% mobilite]
  # Concaténation des mobilités séparées par "+"
  return(paste(mobfr,collapse = " + "))
}

plot_comparaison <- function(Donnee_1, Donnee_2, ord_txt, color_graph) {
  Donnee_1$Periode <- rep("Période de référence",nrow(Donnee_1))
  Donnee_2$Periode <- rep("Période comparée",nrow(Donnee_2))

  Donnee <- rbind(Donnee_1,Donnee_2)

  # Selection des colonnes sur lesquelles on a une variance (2 valeurs au moins)
  Donnee <- Donnee %>% filter(Effectif>1)

  # Calcul des bornes (loi de student)
  Donnee <- Donnee%>% mutate(q=qt(.975,df=Effectif-1))
  Donnee <- Donnee %>% mutate(Born1=Nombre_usagers-q*sqrt(Variance/Effectif),
                              Born2=Nombre_usagers+q*sqrt(Variance/Effectif))
  # Sélection des heures communes
  hour_inter <- intersect(Donnee_1$Heure,Donnee_2$Heure) %>% sort()
  # Nombre d'heure
  k <- length(hour_inter)

  return(ggplot(Donnee)+aes(x = Heure, y=Nombre_usagers, group = Periode, color = Periode)+geom_line(aes(linetype=Periode),size=1.5)+
    labs(x="Heure", y = "Nombre moyen d'usagers")+
    geom_ribbon(aes(ymin=Born1, ymax=Born2, fill = Periode), linetype = "blank",alpha = 1/4)+
    ggtitle(ord_txt)+scale_x_continuous(breaks=hour_inter,limits = c(hour_inter[1]-0.5,hour_inter[k]+0.5))+
    expand_limits(y = 0)+ scale_color_manual(values=color_graph)+
    scale_fill_manual(values=color_graph)
  )
}

plot_wilcoxon <- function(tableau_1,tableau_2){
  # Création du vecteur qui va servir à stocker les valeurs des test de Wilcoxon
  Stat_wilcox <- NULL
  Couleur <- NULL
  hour_inter <- intersect(hour(tableau_1$date),hour(tableau_2$date)) %>% sort()
  k <- length(hour_inter)
  
  # Boucle sur chaque heure
  Stat_wilcox <- map_dbl(hour_inter, ~ {
    i <- .
    usager_1 <- tableau_1 %>% filter(hour(date) == i) %>% .$total
    usager_2 <- tableau_2 %>% filter(hour(date) == i) %>% .$total
    wilcox.test(usager_1,usager_2,exact=FALSE)$p.value # Test de wilcoxon pour une heure donnée
  })

  # Choix de la couleur en fonction de la p-valeur du test
  Couleur <- case_when(
    Stat_wilcox < 0.05 ~ "Significatif",
    Stat_wilcox >= 0.05 & Stat_wilcox < 0.1 ~ "Entre deux",
    Stat_wilcox >= 0.1 ~ "Non-significatif",
    TRUE ~ "Autre"
  )

  # Création du tableau pour la barre indiquant la significativité des tests
  Don2 <- data.frame(heure=hour_inter, couleur=Couleur)

  # Graphique: histogramme d'une unité de hauteur, indiquant la valeur de la significativité
  return(ggplot(Don2)+aes(x = heure , color = couleur, fill = couleur) +
          geom_histogram(bins = k+1)+scale_x_continuous(breaks=hour_inter,limits = c(hour_inter[1]-0.5,hour_inter[k]+0.5))+
          scale_color_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
          scale_fill_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
          theme(
            title = element_text(hjust = 0.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())+
          labs(title="Significativité d'une différence de comportement (*)",
               x="Heure",
               y="")
  )
}

prep <- function(table){
  result <- table %>%
    group_by(hour = hour(date)) %>%
    summarise(count = n(),
              mean = mean(total),
              var = var(total)) %>%
    arrange(hour) %>%
    select(Heure=hour,Nombre_usagers=mean,Variance=var,Effectif=count)
  
  return(result)
}



generate_comparison_section <- function(ns, label_color, ns_date, ns_vacance, ns_jf, ns_sm, label_text) {
  tagList(
    h2(span(style = paste("color:", label_color), label_text)),
    dateRangeInput(ns_date, "Période",
                   start  = "2021-01-01",
                   end    = Sys.Date() - days(1),
                   min    = "2021-01-01",
                   max    = Sys.Date() - days(1)),
    radioButtons(inputId = ns_vacance, label = "Vacances comprises :",
                 choices = c("Oui", "Non", "Seulement les vacances"), selected = "Oui"),
    radioButtons(inputId = ns_jf, label = "Jours fériés compris :",
                 choices = c("Oui", "Non", "Seulement les jours fériés"), selected = "Oui"),
    checkboxGroupInput(
      inputId = ns_sm,
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
      generate_comparison_section(ns, "#006bb6", ns("daterange1"), ns("Vacance1"), ns("JF1"), ns("SM1"), "Période de référence"),
      generate_comparison_section(ns, "#ff5900", ns("daterange2"), ns("Vacance2"), ns("JF2"), ns("SM2"), "Première période de comparaison"),
      generate_comparison_section(ns, "#00b308", ns("daterange3"), ns("Vacance3"), ns("JF3"), ns("SM3"), "Seconde période de comparaison"),
      actionButton(ns("mise_a_j"), "Mettre à jour")
    )),
    uiOutput(ns("resultat"))
  )
}



server_3 <- function(input, output, session, data){
  ns <- session$ns
  
  # pour mettre a jour le choix des capteurs selon liste_capteur
  observe({
    updateSelectInput(session, "sensor", choices = data$sensors)
  })
  
  # initialistion des reactifs
  raw_data <- reactiveValues(ref = NULL,
                             P_1 = NULL,
                             P_2 = NULL)
  
  mean_data<- reactiveValues(ref = NULL,
                             P_1 = NULL,
                             P_2 = NULL)
  
  graph <- reactiveValues(plot1 = NULL,
                          plot2 = NULL,
                          ord_txt = NULL,
                          nb = NULL)
  
  
  # Update of the reactives after the trigger of the update button
  observeEvent(input$mise_a_j, {
    raw_data$ref <- filtrage(data=data$data,sensor = input$sensor, direction = input$sens,mobility = input$mobilite,
                            daterange = input$daterange1,vacation = input$Vacance1, weekdays = input$SM1,p_holiday = input$JF1)
    raw_data$P_1 <- filtrage(data=data$data,sensor = input$sensor,direction = input$sens,mobility = input$mobilite,
                             daterange = input$daterange2,vacation = input$Vacance2,weekdays = input$SM2,p_holiday = input$JF2)
    raw_data$P_2 <- filtrage(data=data$data,sensor = input$sensor,direction = input$sens,mobility = input$mobilite,
                             daterange = input$daterange3,vacation = input$Vacance3,weekdays = input$SM3,p_holiday = input$JF3)
    
    mean_data$ref <- raw_data$ref %>% prep()
    mean_data$P_1 <- raw_data$P_1 %>% prep()
    mean_data$P_2 <- raw_data$P_2 %>% prep()


    graph$ord_txt <- ord_txt_fct(input$mobilite)
    
    compar <- plot_comparaison(mean_data$ref, mean_data$P_1, graph$ord_txt,c("#ff5900","#006bb6"))
    wilcox <- plot_wilcoxon(raw_data$ref, raw_data$P_1)
    graph$plot1 <- cowplot::plot_grid(compar, wilcox, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
 
    compar <- plot_comparaison(mean_data$ref, mean_data$P_2, graph$ord_txt,c("#00b308", "#006bb6"))
    wilcox <- plot_wilcoxon(raw_data$ref, raw_data$P_2)
    graph$plot2 <- cowplot::plot_grid(compar, wilcox, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
    
    graph$nb <- list(ref = mean_data$ref$Effectif %>% mean() %>% round(3),
                     P_1 = mean_data$P_1$Effectif %>% mean() %>% round(3),
                     P_2 = mean_data$P_2$Effectif %>% mean() %>% round(3))
  })
  
  # Preparation de certains reactifs pour la sortie sur l'UI
  output$graph1 <- renderPlot({
    graph$plot1
  })
  
  output$graph2 <- renderPlot({
    graph$plot2
  })
  

  
  # Sortie
  output$resultat <- renderUI(
    if (is.null(raw_data$ref)){
      p("Veuillez cliquer sur le bouton mettre à jour après avoir sélectionner les critères voulus")
    } else {
    column(width = 9,
           h2("Comparaison avec la première période"),
           plotOutput(ns("graph1")),
           h2("Comparaison avec la seconde période"),
           plotOutput(ns("graph2")),
           
           p("Nombre moyen de valeur par heure pour la période de reférence :",graph$nb$ref),
           p("Nombre moyen de valeur par heure pour la période de 1 :",graph$nb$P_1),
           p("Nombre moyen de valeur par heure pour la période de 2 :",graph$nb$P_2),

           downloadButton(ns("downloadData"), "Import des données"),
           
           HTML("(*) Remarques relatives à la significativité de la différence de comportement : <br/>
          Pour chaque créneau horaire, la couleur indique s'il y a un comportement différent des usagers entre les deux périodes.
          Si le résultat est <i>Significatif</i>, c'est qu'il y a très probablement un changement de comportement entre les deux périodes (pour l'heure concernée).
          Si le résultat est <i>Entre deux</i>, alors il est possible qu'il y ait une différence.
          Si le résultat est <i>Non-significatif</i>, on ne peut pas dire qu'il y ait une différence.")
    )
    }
  )
  
  
  
  # Bouton d'import des données
  output$downloadData <- downloadHandler(
    filename = "Comparaison_periode.csv", # Nom du fichier importé
    content = function(file) {
      write_excel_csv2(cbind(ref = mean_data$ref,
                             P_1 = mean_data$P_1,
                             P_2 = mean_data$P_2), file)
    }
  )
}

