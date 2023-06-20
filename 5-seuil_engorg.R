##############################################
#               Functions                    #
##############################################

plot_seuil_prep_1 <- function(orientation, tableau_temp){
  
  vitesse_df <- do.call(rbind,
                        lapply(tableau_temp$car_speed_hist_0to70plus,
                               function(i) { vitesse <- unlist(i)
                                             vitesse_cumsum <- cumsum(vitesse)
                                  c(vitesse_cumsum[1], vitesse_cumsum[2], vitesse_cumsum[3], vitesse_cumsum[4])
                                            }
                        )
  )

  colnames(vitesse_df) <- c("vit_moins10", "vit_moins20", "vit_moins30", "vit_moins40")

  tableau_temp <- cbind(tableau_temp, vitesse_df)
  
  
  # Création du tableau selon la direction choisie
  if(orientation=="Toute"){
    tableau_temp <- tableau_temp %>% 
      select(car,heavy,vit_moins10,vit_moins20,vit_moins30,vit_moins40) %>%
      mutate(vehic = car + heavy) %>%
      arrange(vehic)
  }
  if(orientation=="Rgt"){
    tableau_temp <- tableau_temp %>% 
      select(car_rgt,heavy_rgt,vit_moins10,vit_moins20,vit_moins30,vit_moins40) %>%
      mutate(vehic = car_rgt + heavy_rgt) %>%
      arrange(vehic)
  }
  if(orientation=="Lft"){
    tableau_temp <- tableau_temp %>% 
      select(car_lft,heavy_lft,vit_moins10,vit_moins20,vit_moins30,vit_moins40)  %>%
      mutate(vehic = car_lft + heavy_lft) %>%
      arrange(vehic)
  }
  # Calcul de moyenne glissante sur les parts
  vitesse10 <- embed(tableau_temp$vit_moins10, 50)
  vitesse10 <- apply(vitesse10, 1, function(x) 100 - mean(x))
  vitesse20 <- embed(tableau_temp$vit_moins20, 50)
  vitesse20 <- apply(vitesse20, 1, function(x) 100 - mean(x))
  vitesse30 <- embed(tableau_temp$vit_moins30, 50)
  vitesse30 <- apply(vitesse30, 1, function(x) 100 - mean(x))
  vitesse40 <- embed(tableau_temp$vit_moins40, 50)
  vitesse40 <- apply(vitesse40, 1, function(x) 100 - mean(x))
  
  # Réalisation des abcisses
  vehicule <- embed(tableau_temp$vehic, 50)
  vehicule <- apply(vehicule, 1, mean)
  
  # Préparation du tableau pour le graphique
  
  return(list(vitesse10=vitesse10,vitesse20=vitesse20,vitesse30=vitesse30,
              vitesse40=vitesse40, vehicule=vehicule))
}

plot_seuil_prep <- function(orientation, tableau_temp, selected_speed) {
  plot_seuil_result <- plot_seuil_prep_1(orientation, tableau_temp)
  
  vitesse10 <- plot_seuil_result$vitesse10
  vitesse20 <- plot_seuil_result$vitesse20
  vitesse30 <- plot_seuil_result$vitesse30
  vitesse40 <- plot_seuil_result$vitesse40
  vehicule <- plot_seuil_result$vehicule
  
  VehG <- rep(vehicule,4)    
  Vitesse <- c(vitesse10,vitesse20,vitesse30,vitesse40)
  
  #Création du tableau pour l'import
  TablRes <- data.frame(
    "Nombre de vehicules" = vehicule,
    "plus de 40km/h" = vitesse40,
    "plus de 30km/h" = vitesse30,
    "plus de 20km/h" = vitesse20,
    "plus de 10km/h" = vitesse10
  )
  
  # Creation du tibble pour commencer le graphique
  k <- length(vehicule)
  Legende <- c(rep("Plus de 10km/h",k),rep("Plus de 20km/h",k),rep("Plus de 30km/h",k),rep("Plus de 40km/h",k))
  donnee <- tibble(VehG,Vitesse,Legende)
  
  # Récupération des courbes lissées à partir de la méthode smooth de R
  p <- ggplot(donnee)+aes(x=VehG, y=Vitesse, color = Legende, group=Legende)+stat_smooth()
  y <- ggplot_build(p)$data[[1]][,1:3]

  # create the table for the data
  # Get unique levels of colour
  levels <- levels(as.factor(y$colour))
  
  # Vectorized binding of columns
  Donnee <- reduce(levels, function(acc, level) {
    column <- y[y$colour == level, -1]
    bind_cols(acc, column)
  }, .init = NULL)
  
  # Absisse (nombre de véhicule par heure)
  X <- Donnee[,1]
  
  # Ordonnée des courbes de lissages
  Y <- Donnee[,c(2,4,6,8)]
  
  # Rangement des colonnes par vitesse: 
  # Il y a un moins grand pourcentage d'usagers dépassant les 40km/h que ceux dépassant 30km/h (etc)
  Y <- t(t(Y)[order(t(Y)[,1]),])
  Donnee <- as.data.frame(bind_cols(X,Y))
  
  colnames(Donnee) <- c("Nombre de vehicules","plus de 40km/h","plus de 30km/h","plus de 20km/h","plus de 10km/h") #Renomage des colonnes
  
  # Calcul des seuils pour chaque courbes à partir d'un test de Darling Erdos
  
  if (selected_speed == "Toute") {
    res2 <- apply(Donnee[, 2:5], 2, function(col) {
      tt <- DE.test(col)
      Donnee[tt$estimate, 1]
    })
    moyenne <- mean(res2)
  } else {
    ind <- which(c("plus de 40km/h", "plus de 30km/h", "plus de 20km/h", "plus de 10km/h") == selected_speed)
    tt <- DE.test(Donnee[, ind + 1])
    moyenne <- Donnee[tt$estimate, 1]
  }
  
  # Préparation de l'indexation de l'abscisse
  mmV <- max(donnee$VehG)
  if(mmV<100){
    absi <- seq(0,100,10)
  }else{
    if(mmV<500){
      absi <- seq(0,500,50)
      
    }else{
      maxi <- floor(mmV/100)*100
      absi <- seq(0,maxi,100)
      
    }
  }
  
  # ordonnée de l'affichage de la valeur du seuil
  miny <- min(vitesse40)
  ordMoy <- (100+miny)/2
  
  # Graphique du seuil
  graph <- ggplot(donnee)+aes(x=VehG, y=Vitesse, color = Legende, group=Legende)+geom_line(color="black")+
    geom_smooth()+labs(x="Nombre de véhicules sur une tranche horaire", y = "Pourcentage de véhicule dépassant la vitesse données")+
    ggtitle("Evolution de la vitesse de conduite selon le nombre d'usagers")+
    scale_x_continuous(breaks=c(absi), labels=c(absi))+ labs(fill = "")
  
  # Donnees tracées
  Donnees <- list(Courbes_brute=TablRes,Courbes_lissees=Donnee)
  
  # Retour
  return(list(graph=graph,Donnees=Donnees, moyenne = moyenne, ordMoy=ordMoy))
}

plot_seuil_graph <- function(orientation, tableau_temp, selected_speed, calcul_seuil,seuil) {
  plot_seuil_result <- plot_seuil_prep(orientation, tableau_temp, selected_speed)
  
  ordMoy <- plot_seuil_result$ordMoy
  graph <- plot_seuil_result$graph
  
  if (calcul_seuil == "automatique") {
    moyenne <- plot_seuil_result$moyenne
    graph_seuil <- graph +
      geom_vline(xintercept = moyenne, color = "red", size = 1.5)
    #   geom_text(aes(x = moyenne, y = ordMoy, label = round(moyenne)),
    #             size = 5, angle = -90, vjust = -0.5, color = "red")
    
  }
  
  if (calcul_seuil == "manuel") {
    graph_seuil <- graph +
      geom_vline(xintercept = seuil, color = "red", size = 1.5)
      # geom_text(aes(x = seuil, y = ordMoy, label = seuil),
      #           size = 5, angle = -90, vjust = -0.5, color = "red")
  }
  
  return(graph_seuil)
}


##############################################
#                  Module                    #
##############################################

ui_5 <- function(id){
  ns <- NS(id)
  tagList(
    column(3, wellPanel(
      selectInput(ns("sensor"),
                  label = "Choix du capteur",
                  choices = NULL),
      selectInput(ns("sens3"), label = "Direction du capteur", 
                  choices = c("Toute"="Toute", "B vers A"="Rgt", "A vers B" ="Lft")),
      selectInput(ns("vit"), label = "Choix de la courbe servant à déterminer le seuil",
                  choices = c("Toute", "plus de 10km/h", "plus de 20km/h", "plus de 30km/h", "plus de 40km/h"),
                  selected = "Toute"),
      dateRangeInput(ns("daterange"), "Période",
                     start = "2021-01-01",
                     end = Sys.Date() - days(1),
                     min = "2021-01-01",
                     max = Sys.Date() - days(1)),
      actionButton(ns("mise_a_j"), "Mettre à jour"),
      radioButtons(ns("calcul_seuil"),
                   "Choix du seuil :",
                   selected = "automatique",
                   choices = c("automatique", "manuel"),
                   inline = TRUE),
      uiOutput(ns("reglage"))
    )),
    uiOutput(ns("resultat"))
  )
}


server_5 <- function(input, output, session, data) {
  
  ns <- session$ns
  
  # pour mettre a jour le choix des capteurs selon liste_capteur
  observe({
    updateSelectInput(session, "sensor", choices = data$sensors)
  })
  
  tableau_temp <- reactiveVal()
  
  seuil_prep_1_result <- reactiveVal()
  seuil_prep_result <- reactiveVal()
  plot_s <- reactiveVal()
  
  observeEvent(input$mise_a_j, {
    tableau_temp(filtrage(data$data, sensor = input$sensor, daterange = input$daterange))
    print(input$sens3)
    print(input$vit)
    print(input$calcul_seuil)
    print(input$seuil)
    plot_s(plot_seuil_graph(input$sens3, tableau_temp(), input$vit, input$calcul_seuil, input$seuil))
    
    seuil_prep_1_result(plot_seuil_prep_1(input$sens3, tableau_temp()))
    seuil_prep_result(plot_seuil_prep(input$sens3, tableau_temp(), input$vit))
    })
  


  
  
  output$resultat <- renderUI({
    if (is.null(tableau_temp())) {
      p("Import nécessaire") 
    } else if (nrow(tableau_temp()) < 100) { # Permet d'afficher un message d'erreur s'il n'y a pas assez de données pour calculer un seuil
      p("Attention : période trop courte ou pour laquelle le capteur ne possède pas de données !")
    } else {
      column(width = 9,
             h5("Précisions sur le graphique"),
             p("Le graphique suivant indique pour chaque courbe le pourcentage de conducteurs (véhicules légers
               et poids lourds) qui arrivent à dépasser la vitesse spécifiée en fonction du nombre d'autres 
               conducteurs sur la route durant une même période horaire."),
             p("Un changement brusque dans les courbes peut indiquer une présence régulière d'embouteillage 
               lorsqu'on dépasse la valeur du changement. La barre rouge indique cette valeur."),
             br(),
             p("Avertissement :"),
             p("1. La barre apparait toujours, même pour les routes sans embouteillages."),
             p("2. Le calcul conduisant au placement de la barre n'est pas parfait : elle peut être mal placée."),
             plotOutput(ns("plot_seuil")),
             downloadButton(ns("downloadbrut"), "Import des données des courbes brutes (noire)"),
             downloadButton(ns("downloadlisse"), "Import des données des courbes lissées"),
      )
    }
  })
  
  # Graphique du seuil d'engorgement
  output$plot_seuil <- renderPlot({
    plot_s()
  })
  
  output$downloadbrut <- downloadHandler(
    filename = "Courbes_brutes.csv",
    content = function(file) {
      write_excel_csv2(seuil_prep_result()$Donnees$Courbes_brute, file)
    }
  )
  
  output$downloadlisse <- downloadHandler(
    filename = "Courbes_lissées.csv",
    content = function(file) {
      write_excel_csv2(seuil_prep_result()$Donnees$Courbes_lissees, file)
    }
  )
  
  output$reglage <- renderUI({
    if (input$calcul_seuil == "manuel") {
      sliderInput(ns("seuil"),
                  label="Valeur du seuil", 
                  min=round(min(seuil_prep_1_result()$vehicule, na.rm = TRUE)),
                  max=round(max(seuil_prep_1_result()$vehicule, na.rm = TRUE)), 
                  value=floor(max(seuil_prep_1_result()$vehicule, na.rm = TRUE)), 
                  step = 1, round = FALSE)
    }
  })
  
  
  
}
