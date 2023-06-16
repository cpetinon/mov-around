##############################################
#               Functions                    #
##############################################

deseasonality <- function(donnees,col,model){
  # Conversion en time serie
  time_serie <- ts(donnees[,col], frequency = 7)
  # Decomposition de la serie temporelle avec une moyenne mobile d'ordre 14
  decomposition <- decompose(time_serie, type=model,filter = c(0.5, rep(1, 14 - 1), 0.5)/14)
  
  return(list(trend=coredata(decomposition$trend), 
              cycle=coredata(decomposition$seasonal), 
              noise=coredata(decomposition$random)))
}

# Premiere sélection pour la comparaison de capteurs
Compar_tabl_fct <- function(data, sensor1,sensor2, heure, sens1, sens2, mobilite2){
  # Filtrage sur les segments et l'heure sélectionnées
  segments <- c(sensor1,sensor2)
  Seg1 <- data %>% filter(segment_id==sensor1, hour(date)==heure)
  Seg2 <- data %>% filter(segment_id==sensor2, hour(date)==heure)
  # Jointure sur les dates communes
  Seg1 <- Seg1[Seg1$date %in% Seg2$date,]
  Seg2 <- Seg2[Seg2$date %in% Seg1$date,]
  
  S1 <- trimws(paste0(mobilite2,sens1)) # trimws sert a enlever les espaces de debut et de fin
  S2 <- trimws(paste0(mobilite2,sens2)) 
  
  # Filtrage selon les mobilités et la direction (totA : capteur 1 et totB : capteur 2)
  totA <- apply(Seg1[,S1], MARGIN = 1 ,FUN = sum)
  totB <- apply(Seg2[,S2], MARGIN = 1 ,FUN = sum)
    
  if(length(Seg1$date)<28){
    "Période commune des deux capteurs trop courte"
  }else{ # Préparation des données pour la suite : tableau et nom de colonnes
    res <- bind_cols(date = Seg1$date, ca1 = totA, ca2 = totB)
  }
  return(res)
}

Compar_tabl2_fct <- function(Compar_tabl){
  traitment1 <- deseasonality(Compar_tabl,"ca1","additive") # Séparation du signal en tendance, cycle et bruit (période 1)
  traitment2 <- deseasonality(Compar_tabl,"ca2","additive") # Idem Période 2
  return(list(C1=traitment1,C2=traitment2)) # Retour sous la forme d'une liste à 2 éléments
}
# bind_cols

plot_deseas <- function(part, Compar_tabl, Compar_tabl2, sensor1, sensor2, sens1, sens2, Norm1){
  # part = trend, cycle, noise
  if (Norm1 == "Oui") {
    tend_1 <- scale(Compar_tabl2$C1[[part]])
    tend_2 <- scale(Compar_tabl2$C2[[part]])
  } else {
    tend_1 <- Compar_tabl2$C1[[part]]
    tend_2 <- Compar_tabl2$C2[[part]]
  }
  
  date <- Compar_tabl$date
  if (part=="cycle"){
    date <- 1:7
    tend_1 <- tend_1[1:7]
    tend_2 <- tend_2[1:7]
  }
  
  X <- c(date, date)
  Y <- c(tend_1, tend_2)
  Capteur <- c(rep(sensor1, length(tend_1)), rep(sensor2, length(tend_1)))
  g <- ggplot() + geom_line(aes(x=X,y=Y,col=Capteur))
  
  if (part=="cycle"){
    jours <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    g <- g +  scale_x_continuous(breaks = 1:7,  label = jours)
  }
  
  if (Norm1=="Non"){
    g <- g + labs(title = part,
                    x = "Date",
                    y="Variation en nombre de véhicules")
  } else {
    g <- g + labs(title = part,x = "Date",y="") + 
             theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
  }
  
  return(g)
}

# Coefficient de corrélation de Pearson pour le bruit
correlation_fct <- function(Compar_tabl2){
  noise_1 <- Compar_tabl2$C1$noise
  noise_2 <- Compar_tabl2$C2$noise
  # Coefficient de correlation de Pearson
  correl <- round(cor(noise_1,noise_2,use = "na.or.complete"),3)
  return(correl)
}


# Coefficient de synchronicité des pics pour le bruit
pics_fct <- function(Compar_tabl2){
  noise_1 <- Compar_tabl2$C1$noise %>% na.trim()
  noise_2 <- Compar_tabl2$C2$noise %>% na.trim()
  # Test de synchronicité des pics (on récupère la pvalue calculé à partir de 100 tirage)
  picVal <- peaks(noise_1,noise_2,nrands = 100)$pval
  pic <- round(peaks(noise_1,noise_2)$obs,3)
  return(list(pic=pic, picVal = picVal))
}



##############################################
#                  Module                    #
##############################################

ui_6 <- function(id){
  ns <- NS(id)
  tagList(
    column(3,wellPanel(
      selectInput(ns("sensor1"),
                  label = "Choix du premier capteur",
                  choices = NULL),
      selectInput(ns("sens1"), label = "Direction du capteur",
                  choices = c("Toute"=" ","B vers A"="_rgt","A vers B" ="_lft")),
      selectInput(ns("sensor2"),
                  label = "Choix du second capteur",
                  choices = NULL),
      selectInput(ns("sens2"), label = "Direction du capteur",
                  choices = c("Toute"=" ","B vers A"="_rgt","A vers B" ="_lft")),
      selectInput(ns("heure"), label = "Choix de l'heure",
                  choices = c("6h-7h"=7,"7h-8h"=8,"8h-9h"=9,"9h-10h"=10,"10h-11h"=11,"11h-12h"=12,
                              "12h-13h"=13,"13h-14h"=14,"14h-15h"=15,"15h-16h"=16,"16h-17h"=17,
                              "17h-18h"=18,"18h-19h"=19,"19h-20h"=20,"20h-21h"=21),
                  selected = 9),
      checkboxGroupInput(
        ns("mobilite2"),
        "Choix du type de mobilité",
        selected = c("car","heavy"),
        choiceNames = c("VL","PL","Piéton","Vélo"),
        choiceValues = c("car","heavy","pedestrian","bike")
      ),
      radioButtons(inputId = ns("Norm1"), label = "Normaliser :",
                   choices = c("Oui","Non"),selected = "Non",inline = TRUE),
      actionButton(ns("mise_a_j"), "Mettre à jour")
    )),
    
           uiOutput(ns("result"))
  )
}


server_6 <- function(input,output,session,data){
  ns <- session$ns
  
  # pour mettre a jour le choix des capteurs selon liste_capteur
  observe({
    updateSelectInput(session, "sensor1", choices = data$sensors)
    updateSelectInput(session, "sensor2", choices = data$sensors)
  })
  
  Compar_tabl <- reactive({
    Compar_tabl_fct(data$data, input$sensor1,input$sensor2, input$heure,
                    input$sens1, input$sens2, input$mobilite2)
  })
  Compar_tabl2 <- reactive({
    result <- Compar_tabl2_fct(Compar_tabl())
  })
  
  plot_tend <- reactiveVal()
  plot_cycle <- reactiveVal()
  plot_noise <- reactiveVal()
  correlation <- reactiveVal()
  pics <- reactiveVal()
  prep_tabl2 <- reactiveVal()
  
  observeEvent(input$mise_a_j,{
    plot_tend(plot_deseas('trend',Compar_tabl(),Compar_tabl2(),input$sensor1,input$sensor2, input$sens1, input$sens2, input$Norm1))
    plot_cycle(plot_deseas('cycle',Compar_tabl(),Compar_tabl2(),input$sensor1,input$sensor2, input$sens1, input$sens2, input$Norm1))
    plot_noise(plot_deseas('noise',Compar_tabl(),Compar_tabl2(),input$sensor1,input$sensor2, input$sens1, input$sens2, input$Norm1))
    correlation(correlation_fct(Compar_tabl2()))
    pics(pics_fct(Compar_tabl2()))
    # prep_tabl2(prep_tabl2_fct(Compar_tabl(),Compar_tabl2(),input$capteur1,input$sensor2,input$sens1,input$sens2))
  }) 
  
  output$graph_tend <- renderPlot({
    plot_tend()
  })
  
  output$graph_cycle <- renderPlot({
    plot_cycle()
  })
  
  output$graph_noise <- renderPlot({
    plot_noise()
  })
  
  output$corr <- renderUI({
    correl <- correlation()
    ligne1 <- paste("Coefficient de correlation :",correl) # Fabrication du texte pour l'affichage
    if(correl>0.5){
      ligne2 <- "C'est une valeur élevée, les deux courbes sont corrélées "
    }
    if(correl<=0.5){
      ligne2 <- "C'est une valeur moyenne, les deux courbes sont légèrement corrélées "
    }
    if(correl<0.2){
      ligne2 <- "C'est une valeur faible, les deux courbes ne sont pas corrélées "
    }
    HTML(paste(ligne1,ligne2,sep="<br/>"))
  })
  
  output$pic <- renderUI({
    pic <- pics()[[1]]
    picVal <- pics()[[2]]
    ligne1 <- paste("Taux de synchronicité des pics :",pic) # Fabrication du texte pour l'affichage
    if(picVal<0.05){
      ligne2 <- "Les pics des deux courbes sont atteints en même temps très souvent."
    }else{
      ligne2 <- "On ne peut pas dire que les pics des deux courbes sont souvent atteints en même temps."
    }
    HTML(paste(ligne1,ligne2,sep="<br/>"))
  })
  
  output$result <- renderUI({
    if (is.null(data$data)){
      "Import necessaire"
    }else if (mode(Compar_tabl())=="character"){
      return(Compar_tabl())
    } else {
      column(width = 9,
        tabsetPanel(
          tabPanel("Tendance",
                   h3("Comment évolue la circulation au long de la présence du capteur?"),
                   plotOutput(ns("graph_tend"))),
          tabPanel("Cycle hebdomadaire",
                   h3("Quel est l'effet du jour de la semaines ?"),
                   plotOutput(ns("graph_cycle"))),
          tabPanel("Bruit",
                   h3("Peut-on dire que les usagers ont le même comportement sur les deux segments de routes ?"),
                   plotOutput(ns("graph_noise")),
                   htmlOutput(ns("corr")),
                   htmlOutput(ns("pic")))),
          downloadButton(ns("downloaddonnees_complementaires"), "Import des données")
      )
    }
  })
  
  # Import
  output$downloaddonnees_complementaires <- downloadHandler(
    filename = "Comparaison_capteur.csv", # Nom du fichier importé
    content = function(file) {
      write_excel_csv2(prep_tabl2(), file)
    }
  )
}