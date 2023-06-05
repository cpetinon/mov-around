##############################################
# Les fonctions
##############################################

premier_filtre_fct <- function(donnees,capteur, sens, mobilite){
  if(is.null(donnees)){
    donnees_filtrees <- NULL
  }else{
    # Filtrage sur le capteur sélectionnée
    donnees_filtrees <- donnees[ donnees$segment_id==capteur, ]
    
    # Filtrage sur le sens choisie
    if(sens=="Toute"){
      donnees_filtrees <- donnees_filtrees[,c("date","uptime","heavy","car","bike","pedestrian")]
    }
    if(sens=="Rgt"){
      donnees_filtrees <- donnees_filtrees %>% select(c(date,uptime,heavy_rgt,car_rgt,bike_rgt,pedestrian_rgt)) %>%
        rename(c(heavy = heavy_rgt, car = car_rgt, bike = bike_rgt, pedestrian = pedestrian_rgt))
    }
    if(sens=="Lft"){
      donnees_filtrees <- donnees_filtrees %>% select(c(date,uptime,heavy_lft,car_lft,bike_lft,pedestrian_lft)) %>%
        rename(c(heavy = heavy_lft, car = car_lft, bike = bike_lft, pedestrian = pedestrian_lft))
    }
    
    # Filtrage sur la sélection de mobilités
    if(length(mobilite)>1){
      interet <- apply(donnees_filtrees[,mobilite], MARGIN = 1 ,FUN = sum)
      donnees_filtrees$total <- interet
    }else{
      donnees_filtrees$total <- donnees_filtrees[,mobilite]
    }
  }
  return(donnees_filtrees)
}

tableau_P <- function(donnees, daterange1,Vacance1,SM1,JF1 ){
  # Test pour savoir si la sélection est vide
  if(length(donnees$date)==0){
    donnees="Selectionnez un capteur et au moins une mobilité, puis appuyez sur Mettre à jour"}else{
      
      # Sélection de la période temporelle
      date <- daterange1
      periode <- interval(ymd_hms(paste(date[1],"00:00:00")),ymd_hms(paste(date[2],"23:59:00")))
      donnees <- selection_date(donnees,periode) %>%
        .$donnees_correspondantes
      
      # Sélection des jours de toute la semaine
      donnees <- donnees %>% filter(wday(date) %in% SM1)
      
      if(length(donnees$date)==0){ # Test pour savoir si la sélection est vide
        donnees <- "Pas de données pour la selection de la période de référence"
      }else{ # Sélection de vacances
        if(Vacance1=="Non"){
          donnees <- selection_date(donnees,Vacances$interval) %>%
            .$donnees_complementaires
        }
        if(Vacance1=="Seulement les vacances"){
          donnees <- selection_date(donnees,Vacances$interval) %>%
            .$donnees_correspondantes
        }
        if(length(donnees$date)==0){  # Test pour savoir si la sélection est vide
          donnees <- "Pas de données pour la selection de la période de référence"
        }else{ # Sélection de jours fériés
          if(JF1=="Non"){
            donnees <- selection_date2(donnees,JoursFeries) %>%
              .$donnees_complementaires
          }
          if(JF1=="Seulement les jours fériés"){
            donnees <- selection_date2(donnees,JoursFeries) %>%
              .$donnees_correspondantes
          }
          if(length(donnees$date)==0){  # Test pour savoir si la sélection est vide
            donnees <- "Pas de données pour la selection de la période de référence"
          }
        }
      }}
  return(donnees)
}

ord_txt_fct <- function(mobilite){
  # Récupération des termes en français
  fr <-  c("Véhicules Légers","Poids Lourds","Piétons","Vélos")
  eng <-  c("car","heavy","pedestrian","bike")
  mobfr <- fr[eng %in% mobilite]
  # Concaténation des mobilités séparées par "+"
  return(paste(mobfr,collapse = " + "))
}

# Graphique par heure et test de wilcoxon horaire
plot_wilcoxon <- function(tableau_P1,tableau_P2,ord_txt,color_graph){
  # Récupération du tapleau de la période de référence
  Tableau_1 <- tableau_P1
  # Moyenne par tranche horaire
  n_1 <- Tableau_1 %>% group_by(hour(date)) %>% summarise(n = n())
  Donnee_1 <- Tableau_1 %>%
    group_by(hour(date)) %>%
    mutate(Moyenne=mean(total), Var=var(total)) %>%
    filter (!duplicated(hour(date))) %>%
    arrange(hour(date))
  # Sélection de la colonne heure et celle colonne horaire
  Donnee_1 <- bind_cols(Donnee_1[,8:10],n_1[,2])
  # Rajout d'une colonne répétant "Periode Ref"
  Donnee_1 <- cbind(Donnee_1,rep("Periode_Ref",length(Donnee_1[,1])))
  
  # Récupération du tapleau de la première période de comparaison
  Tableau_2 <- tableau_P2
  # Moyenne par tranche horaire
  n_2  <- Tableau_2 %>% group_by(hour(date)) %>% summarise(n = n())
  Donnee_2 <- Tableau_2 %>%
    group_by(hour(date)) %>%
    mutate(Moyenne=mean(total), Var=var(total)) %>% 
    filter (!duplicated(hour(date))) %>% 
    arrange(hour(date))
  # Sélection de la colonne heure et celle colonne horaire
  Donnee_2 <- bind_cols(Donnee_2[,8:10],n_2[,2])
  # Rajout d'une colonne répétant "Periode 1"
  Donnee_2 <- cbind(Donnee_2,rep("Periode_1",length(Donnee_2[,1])))
  
  # Concaténation à la suite des tableaux
  Donnee <- rbind(Donnee_1,Donnee_2)
  # Renomage des colonnes
  colnames(Donnee) <- c("Heure","Nombre_usagers","Variance","Effectif","Periode")
  
  # Selection des colonnes sur lesquelles on a une variance (2 valeurs au moins)
  Donnee <- Donnee %>% filter(Effectif>1)
  
  # Calcul des bornes (loi de student)
  Donnee <- Donnee%>% mutate(q=qt(.975,df=Effectif-1))
  Donnee <- Donnee %>% mutate(Born1=Nombre_usagers-q*sqrt(Variance/Effectif),
                              Born2=Nombre_usagers+q*sqrt(Variance/Effectif))
  # Sélection des heures communes
  heure1 <- as.numeric(levels(as.factor(hour(Tableau_1$date))))
  heure2 <- as.numeric(levels(as.factor(hour(Tableau_2$date))))
  heure <- intersect(heure1,heure2)
  # Nombre d'heure
  k=length(heure)
  
  # Création du graphique faisant apparaitre les courbes des deux périodes
  l <- ggplot(Donnee)+aes(x = Heure, y=Nombre_usagers, group = Periode, color = Periode)+geom_line(aes(linetype=Periode),size=1.5)+
    labs(x="Heure", y = "Nombre moyen d'usagers")+
    geom_ribbon(aes(ymin=Born1, ymax=Born2, fill = Periode), linetype = "blank",alpha = 1/4)+
    ggtitle(ord_txt)+scale_x_continuous(breaks=heure,limits = c(heure[1]-0.5,heure[k]+0.5))+
    expand_limits(y = 0)+ scale_color_manual(values=color_graph)+
    scale_fill_manual(values=color_graph)
  
  # Création du vecteur qui va servir à stocker les valeurs des test de Wilcoxon
  Stat_wilcox <- NULL
  Couleur <- NULL
  # Boucle sur chaque heure
  for(i in heure){
    # Test de wilcoxon pour une heure donnée
    stata <- wilcox.test(Tableau_1 %>% 
                           filter(hour(date)==i) %>%
                           .$total,
                         Tableau_2 %>%
                           filter(hour(Tableau_2$date)==i) %>%
                           .$total
    )$p.value
    Stat_wilcox <- c(Stat_wilcox, stata)
    # Choix de la couleur en fonction de la p-valeur du test
    if(stata<0.05){
      Couleur <- c(Couleur,"Significatif")
    }
    if(stata>=0.1){
      Couleur <- c(Couleur,"Non-significatif")
    }
    if(stata>=0.05 & stata<0.1){
      Couleur <- c(Couleur,"Entre deux")
    }
    
  }
  
  # Création du tableau pour la barre indiquant la significativité des tests
  Don2 <- as_tibble(cbind(heure,Couleur))
  colnames(Don2) <- c("heure","couleur")
  
  # Graphique: histogramme d'une unité de hauteur, indiquant la valeur de la significativité
  h <- ggplot(Don2)+aes(x = as.double(heure) , color = couleur, fill = couleur) +
    geom_histogram(bins = k+1)+scale_x_continuous(breaks=as.double(heure),limits = c(as.double(heure[1])-0.5,as.double(heure[k])+0.5))+
    scale_color_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
    scale_fill_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
    theme(
      title = element_text(hjust = 0.5),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())+
    labs(title="Significativité d'une différence de comportement (*)",
         x="Heure",
         y="")
  
  # Alignement des deux graphiques 
  cowplot::plot_grid(l, h, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
  
}

# fonction intermediaire dans prep_tabl_fct
export_prep <- function(table, colname){
  # Calculating the count per hour
  count <- table %>% group_by(hour(date)) %>% summarise(n = n())
  # Calculating the mean per hour
  mean_data <- table %>%
    group_by(hour(date)) %>%
    mutate(Moyenne=mean(total)) %>%
    filter (!duplicated(hour(date))) %>%
    arrange(hour(date))
  # Extracting necessary columns and adding count
  mean_data <- bind_cols(mean_data[,8:9],count[,2])
  
  colnames(mean_data) <- colname
  mean_data
}

prep_tabl_fct <- function(tableau_P1, tableau_P2, tableau_P3){
  
  Donnee_1 <- export_prep(tableau_P1,c("Heure","Période_Ref","Effectif_Ref"))
  Donnee_2 <-  export_prep(tableau_P2,c("Heure","Période_1","Effectif_P1"))
  Donnee_3 <- export_prep(tableau_P3,c("Heure","Période_2","Effectif_P2"))
  
  
  # Concaténation sur l'heure de la moyenne
  Donnee <-  inner_join(Donnee_1,Donnee_2,by="Heure") %>%
    inner_join(Donnee_3,by="Heure")
  
  Donnee
}


##############################################
# Le module
##############################################

ui_3 <- function(id){
  ns <- NS(id)
  tagList(
    column(3,wellPanel(
      selectInput(ns("test"),
                  label = "Choix du capteur",
                  choices = NULL),
      
      selectInput(ns("sens"), label = "Direction",
                  choices = c("Toute"="Toute","B vers A"="Rgt","A vers B" ="Lft"),
                  selected = "Toute"),
      checkboxGroupInput(
        ns("mobilite"),
        "Choix du type de mobilité",
        selected = c("car","heavy"),
        choiceNames = c("VL","PL","Piéton","Vélo"),
        choiceValues = c("car","heavy","pedestrian","bike")
      ),
      h2(span(style="color:#006bb6","Periode de référence")),
      dateRangeInput(ns("daterange1"), "Période",
                     start  = "2021-01-01",
                     end    = Sys.Date()-days(1),
                     min    = "2021-01-01",
                     max    = Sys.Date()-days(1)),
      radioButtons(ns("Vacance1"), label = "Vacances comprises :",
                   choices = c("Oui","Non","Seulement les vacances"),selected = "Oui"),
      radioButtons(ns("JF1"), label = "Jours fériés compris :",
                   choices = c("Oui","Non","Seulement les jours fériés"),selected = "Oui"),
      checkboxGroupInput(
        ns("SM1"),
        "Choix des jours",
        selected = 1:7,
        choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
        choiceValues = 1:7,
        inline = TRUE
      ),
      h2(span(style="color:#ff5900","Première période de comparaison ")),
      dateRangeInput(ns("daterange2"), "Période",
                     start  = "2021-01-01",
                     end    = Sys.Date()-days(1),
                     min    = "2021-01-01",
                     max    = Sys.Date()-days(1)),
      radioButtons(inputId = ns("Vacance2"), label = "Vacances comprises :",
                   choices = c("Oui","Non","Seulement les vacances"),selected = "Oui"),
      radioButtons(inputId = ns("JF2"), label = "Jours fériés compris :",
                   choices = c("Oui","Non","Seulement les jours fériés"),selected = "Oui"),
      checkboxGroupInput(
        ns("SM2"),
        "Choix des jours",
        selected = 1:7,
        choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
        choiceValues = 1:7,
        inline = TRUE
      ),
      h2(span(style="color:#00b308","Seconde période de comparaison")),
      dateRangeInput(ns("daterange3"), "Période",
                     start  = "2021-01-01",
                     end    = Sys.Date()-days(1),
                     min    = "2021-01-01",
                     max    = Sys.Date()-days(1)),
      radioButtons(ns("Vacance3"), label = "Vacances comprises :",
                   choices = c("Oui","Non","Seulement les vacances"),selected = "Oui"),
      radioButtons(ns("JF3"), label = "Jours fériés compris :",
                   choices = c("Oui","Non","Seulement les jours fériés"),selected = "Oui"),
      checkboxGroupInput(
        ns("SM3"),
        "Choix des jours",
        selected = 1:7,
        choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
        choiceValues = 1:7,
        inline = TRUE
      ),
      actionButton(ns("mise_a_j"), "Mettre à jour")
    )),
    uiOutput(ns("resultat"))
    )
}



server_3 <- function(input, output, session, data){
  ns <- session$ns
  
  # pour mettre a jour le choix des capteurs selon liste_capteur
  observe({
    updateSelectInput(session, "test", choices = data$captors)
  })
  
  # initialistion des reactifs

  premier_filtre <- reactiveVal()

  tableau_P1 <- reactiveVal()
  tableau_P2 <- reactiveVal()
  tableau_P3 <- reactiveVal()

  ord_txt <- reactiveVal()

  plot1 <- reactiveVal()
  plot2 <- reactiveVal()

  prep_tabl <- reactiveVal()

  nombreRef <- reactiveVal()
  nombreP1 <- reactiveVal()
  nombreP2 <- reactiveVal()

  # mise a jour des reactifs
  observeEvent(input$mise_a_j, {
    premier_filtre(premier_filtre_fct(data$data, input$test, input$sens, input$mobilite))
    donnee <- premier_filtre()
    tableau_P1(tableau_P(donnee, input$daterange1,input$Vacance1,input$SM1,input$JF1))
    tableau_P2(tableau_P(donnee, input$daterange2, input$Vacance2, input$SM2, input$JF2))
    tableau_P3(tableau_P(donnee, input$daterange3, input$Vacance3, input$SM3, input$JF3))

    ord_txt(ord_txt_fct(input$mobilite))

    plot1(plot_wilcoxon(tableau_P1(),tableau_P2(),ord_txt(),c("#ff5900","#006bb6" )))
    plot2(plot_wilcoxon(tableau_P1(),tableau_P3(),ord_txt(),c("#00b308", "#006bb6") ))

    prep_tabl(prep_tabl_fct(tableau_P1(),tableau_P2(),tableau_P3()))

    Valeur <- prep_tabl()
    nombreRef(round(mean(Valeur$Effectif_Ref),3))
    nombreP1(round(mean(Valeur$Effectif_P1),3))
    nombreP2(round(mean(Valeur$Effectif_P2),3))

  })

  # Preparation de certains reactifs pour la sortie sur l'UI
  output$graph1 <- renderPlot({plot1()})
  output$graph2 <- renderPlot({plot2()})

  output$nombreRef_out <- renderText({paste("Nombre moyen de valeur par heure pour la période de reférence :",nombreRef())})
  output$nombreP1_out <- renderText({paste("Nombre moyen de valeur par heure pour la période 1 :",nombreP1())})
  output$nombreP2_out <- renderText({paste("Nombre moyen de valeur par heure pour la période 2 :",nombreP2())})

  # Sortie
  output$resultat <- renderUI(
    if (is.null(premier_filtre())){
      p("Import nécessaire")
    } else if (length(input$SM1)==0|length(input$SM2)==0|length(input$SM3)==0){ # a debugger (mode ? voir code Tanguy)
      p("Veuillez selectionner un jour de la semaine")
    } else if (is.null(tableau_P1())|is.null(tableau_P2())|is.null(tableau_P3())) { # revoir
      p("Les données sont vides pour les critères choisies")
    } else {
      column(width = 9,
             h2("Comparaison avec la première période"),
             plotOutput(ns("graph1")),
             h2("Comparaison avec la seconde période"),
             plotOutput(ns("graph2")),

             textOutput(ns('nombreRef_out')),
             textOutput(ns('nombreP1_out')),
             textOutput(ns('nombreP2_out')),

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
      write_excel_csv2(prep_tabl(), file)
    }
  )
}

