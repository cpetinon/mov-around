##############################################
#               Functions                    #
##############################################

########################
# Avoid warnings from dplyr
########################

options(dplyr.summarise.inform = FALSE)

########################
# Individual visualization
########################

#' gg_na_dist
#' the function gives a graphical representation of na's distribution (monthly) during a selected period
#' for one sensor
#'
#' @param data Data.Fame. A data frame containing road traffic data 
#' @param sensor Numeric. The sensor ID on which you want to see
#' @param start Character. The start of the period in the format "yyyy-mm-dd"
#' @param end Character. The start of the period in the format "yyyy-mm-dd"
#' @param hours Vector. A vector containing hours on which you want to see the representation, ex : c("05h", "18h")
#' @param list_weekday Vector. A vector containing weekdays in french, ex : c("Monday", "Sunday")
#' @param pub_holyday "YES" : if you want to see results with public holydays inclue; 
#' "NO" if you want to exclude them from the representation;
#' "ONLY" if you only want to see them in the result
#' @param holydays "YES" : if you want to see results including holydays; 
#' "NO" if you want to exclude them from the representation;
#' "ONLY" if you only want to see them in the result
#' 
#' @import ggplot2
#' @import tidyverse
#' @import lubridate
#'
#' @return a ggplot graph
#' @examples
#'

gg_na_dist <- function(data,
                       sensor,
                       start = NULL, 
                       end = NULL, 
                       hours = "ALL",
                       list_weekday = "ALL",
                       pub_holidays = "YES",
                       holidays = "YES"){
  
  # create a month-year column 
  data <- data |> mutate(month = as.factor(str_sub(date, 1, 7))) 
  
  if (is.null(start)){
    start <- str_sub(min(data$date), 1, 10)
  }
  if (is.null(end)){
    end <- str_sub(max(data$date), 1, 10)
  }
  
  # filter data by date
  data <- data |> 
    filter(date >= start & date <= paste(end, "18:00:00")) |>
    filter(segment_id %in% sensor) |>
    arrange(date)
  
  # create ticks label
  labels_legend <- data |> select(month) |> distinct()
  
  # filter by weekdays 
  if (!setequal(list_weekday, rep("ALL", length(list_weekday)))){
    
    data <- data |>  
      filter(weekdays(ymd_hms(date)) %in% list_weekday) |> 
      mutate(date = as.character(date))
    
  }else{
    data <- data |> mutate(date = as.character(date))
  }
  
  # filter pub holidays
  if (pub_holidays == "YES"){
    
    data <- data 
    
  }else if(pub_holidays == "NO"){
    
    data <- data |> filter(period != "public holidays")
    
  }else if(pub_holidays == "ONLY"){
    
    data <- data |> filter(period == "public holidays")
    
  }
  
  # filter holidays
  
  if (holidays == "YES"){
    
    data <- data
    
  }else if(holidays == "NO"){
    
    data <- data |> filter(period != "holidays")
    
  }else if(holidays == "ONLY"){
    
    data <- data |> filter(period == "holidays")
    
  }
  
  # filter by hours
  
  if (length(hours) == 1){
    if(hours != "ALL"){
      data <- data |> filter(str_sub(date,12,19) == hours)
    }
  }else{
    data <- data |> filter(str_sub(date,12,19) %in% hours)
  }
  
  res <- data |>
    group_by(month) |>
    summarise(Avant = sum(imputation, na.rm = TRUE) + sum(is.na(imputation)),
              Apres = sum(is.na(imputation)),
              nb_obs = n()) |>
    mutate(Avant = round(Avant * 100/nb_obs),
           Apres = round(Apres * 100/nb_obs)) |>
    select(month, Avant, Apres)
  
  res <- res |> right_join(labels_legend, by = c("month" = "month"))
  
  labels_legend <- labels_legend |> mutate(month = ifelse(str_sub(month,6,8) != "06",str_sub(month,6,8), paste("06", "\n", str_sub(month,1,4))))
  labels_legend <- labels_legend$month
  
  res <- res |> pivot_longer(cols = !month, names_to = "label" ) |>
    mutate(value = ifelse(is.na(value), 0, value))
  
  ggplot(res,aes(x = month, y = value, fill = factor(label)))+
    geom_bar(stat = "identity", position = position_dodge(-0.9)) +
    scale_y_continuous(name = "Proportion de \n valeur manquantes", 
                       limits = c(0,101),
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c("0 %", "25 %", "50 %", "75 %", "100 %")) +
    scale_x_discrete(name = "Mois",
                     labels = labels_legend) +
    scale_fill_manual(values=c("Avant"="firebrick2", "Apres"="chartreuse2")) +
    labs(fill = "") +
    guides(fill = guide_legend(reverse = TRUE))
}

########################
# Proportion of NA and information
########################

#' Visualisation NA stats
#'
#' @param data Data frame. Containing road traffic data
#' @param start Character. The start date format "yyyy-mm-dd"
#' @param end Character. The end date format "yyyy-mm-dd"
#' @param sensor Character. The sensor ID on which you work
#'
#' @return
#'
#' @examples
#' 
na_prop <- function(data, start, end, sensor){
  
  # filter by date and sensor
  data_filter <- data |> filter(date >= start,
                                date <= end,
                                segment_id %in% sensor)
  
  # calcul proportion of NA, of NA imputed and of NA remaining 
  prop_na <-  round(nrow(data_filter[data_filter$imputation %in% c(TRUE,NA),]) / nrow(data_filter), digits = 2)*100
  prop_na_imp <- round(sum(is.na(data_filter$car)) / nrow(data_filter), digits = 2)*100
  prop_imp <- prop_na - prop_na_imp
  
  # prepare the text
  phrase_1 <- sprintf("Avant de traiter les données manquantes, nous avions %s pourcents de valeurs manquantes.", prop_na)
  phrase_2 <- sprintf("Post-traitement, nous avons %s pourcents de données remplacées et %s pourcents de valeurs manquantes restantes.",prop_imp, prop_na_imp)
  
  texte <- c(phrase_1, phrase_2)
  # return the text with all the information
  return(texte)
}


########################
# Resume visualisation
########################

#' gg_na_heatmap
#' the function gives a graphical representation of na's proportion (monthly) during a selected period
#'
#' @param data a data frame containing road traffic data 
#' @param start the start of the period in the format "yyyy-mm-dd"
#' @param end the start of the period in the format "yyyy-mm-dd"
#' @param hours a vector containing hours on which you want to see the representation, ex : c("05h", "18h")
#' @param list_weekday a vector containing weekdays in french, ex : c("Monday", "Sunday")
#' @param pub_holyday "YES" : if you want to see results with public holydays inclue; 
#' "NO" if you want to exclude them from the representation;
#' "ONLY" if you only want to see them in the result
#' @param holydays "YES" : if you want to see results including holydays; 
#' "NO" if you want to exclude them from the representation;
#' "ONLY" if you only want to see them in the result
#' 
#' @import ggplot2
#' @import tidyverse
#' @import lubridate
#'
#' @return a ggplot graph
#' @examples
#'

gg_na_heatmap <- function(data, 
                          start = NULL,
                          end = NULL,
                          hours = "ALL",
                          list_weekday = "ALL",
                          pub_holidays = "YES",
                          holidays = "YES"){
  if (is.null(start)){
    start <- str_sub(min(data$date), 1, 10)
  }
  if (is.null(end)){
    end <- str_sub(max(data$date), 1, 10)
  }
  
  # create a month-year column 
  data <- data |> mutate(month = str_sub(date, 1, 7))
  labels_legend <- data |> select(month) |> distinct()
  
  
  # retreive all sensors id
  list_sensor <- distinct(data, segment_id)$segment_id
  
  # filter by date
  data <- data |> filter(date >= start, date <= end)
  
  # filter by hours
  if (length(hours) == 1){
    if(hours != "ALL"){
      data <- data |> filter(endsWith(str_sub(date, 12, 19), hours))
    }
  }else{
    data <- data |> filter(str_sub(date, 12, 19) %in% hours)
  }
  
  # filter by weekdays 
  if (!setequal(list_weekday, rep("ALL", length(list_weekday)))){
    
    data <- data |>  
      filter(weekdays(ymd_hms(date)) %in% list_weekday) |> 
      mutate(date = as.character(date))
    
  }else{
    data <- data |> mutate(date = as.character(date))}
  
  # filter pub holidays
  if (pub_holidays == "YES"){
    
    data <- data 
    
  }else if(pub_holidays == "NO"){
    
    data <- data |> filter(period != "public holidays")
    
  }else if(pub_holidays == "ONLY"){
    
    data <- data |> filter(period == "public holidays")
    
  }
  
  # filter holidays
  
  if (holidays == "YES"){
    
    data <- data
    
  }else if(holidays == "NO"){
    
    data <- data |> filter(period != "holidays")
    
  }else if(holidays == "ONLY"){
    
    data <- data |> filter(period == "holidays")
    
  }
  
  # summarize proportion of NA by month by sensor
  result <- data.frame()
  for (sensor_id in list_sensor){
    res <- data |> filter(segment_id == sensor_id) |>
      group_by(month) |> 
      summarize(prop_NA = sum(is.na(car))*100/n()) 
    res <- res |> 
      right_join(labels_legend, by = c("month" = "month")) |>
      mutate(segment_id = as.factor(sensor_names[which(sensor_ids == sensor_id)])) |>
      mutate(prop_NA = ifelse(is.na(prop_NA), 100, prop_NA))
    result <- rbind(result,res)
  }
  
  labels_legend <- labels_legend |> 
    mutate(month = ifelse(str_sub(month,6,8) != "06",str_sub(month,6,8), paste("06", "\n", str_sub(month,1,4))))
  labels_legend <- labels_legend$month
  
  ggplot(result, aes(month, segment_id, fill = prop_NA)) + 
    geom_tile(col ="grey") + 
    scale_fill_gradient(low="chartreuse2",high= "firebrick2") +
    labs(fill = "Proportion de \nvaleurs manquantes") +
    scale_x_discrete(name = "Mois",
                     labels = labels_legend) +
    ylab("Capteurs")
  
}

##############################################
#                  Module                    #
##############################################

ui_8 <- function(id) {
  ns <- NS(id)
  tagList(
    column(3, wellPanel(
      selectInput(ns("sensor"),
                  label = "Choix du capteur",
                  choices = NULL),
      dateRangeInput(inputId = ns("ns_date"),
                     "Période",
                     start  = "2021-01-01",
                     end    = Sys.Date() - days(1),
                     min    = "2021-01-01",
                     max    = Sys.Date() - days(1)),
      radioButtons(inputId = ns("ns_vacance"), label = "Vacances comprises",
                   choices = c("Oui" = "YES", "Non" = "NO", "Seulement les vacances" = "ONLY"),
                   selected = "YES"),
      radioButtons(inputId = ns("ns_jf"), label = "Jours fériés compris",
                   choices = c("Oui" = "YES", "Non" = "NO", "Seulement les jours fériés" = "ONLY"),
                   selected = "YES"),
      checkboxGroupInput(
        inputId = ns("ns_sm"),
        label = "Choix des jours",
        # We have to put weekdays in English because shinyapps's server is in English
        selected = c(" Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        choiceNames = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"),
        choiceValues = c(" Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        inline = TRUE
      ),
      checkboxGroupInput(
        inputId = ns("ns_hour"),
        label = "Choix des heures",
        selected = c("08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00", "13:00:00",
                     "14:00:00", "15:00:00", "16:00:00"),
        choiceNames = c("06h", "07h", "08h", "09h", "10h", "11h", "12h", "13h", "14h", "15h", "16h", "17h", "18h"),
        choiceValues = c("06:00:00", "07:00:00", "08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00", "13:00:00",
                         "14:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00"),
        inline = TRUE
      ),
      actionButton(ns("mise_a_j"), "Mettre à jour"),
      downloadButton(ns("downloadData"), "Import des données"))),
    tags$head(
      tags$script(HTML("
      $(document).ready(function() {
        $('#toggleMethodButton_8').click(function() {
          $('#methodText_8').toggle();
        });
      });
    "))
    ),
    column(width = 9, 
           h3("Gestions des valeurs manquantes"),
           p("Dans notre analyse, nous considérons les données comme manquantes si l’uptime 
             est inférieur à 0,5. De plus, nous ne considérons que les données de 06h à 19h 
             puisque les capteurs Telraam ne fonctionne que de jours."),
           actionButton("toggleMethodButton_8", "Détails statistiques", style = "display: block; margin: 0 auto;"),
           div(id = "methodText_8", style = "display: none;",
               p( "Nous remplaçons les données de la manière suivante en suivant le schéma 
               ci-dessous, tout d’abord nous transformons le tableau de données de manière 
               à avoir les jours en lignes et les heures en colonnes. Ensuite nous faisons
               une interpolation linéaire sur les lignes du tableau puis sur ses colonnes.
               Lors de ce processus, si nous avons plus de 4 valeurs manquantes consécutives
               , en ligne ou en colonne, nous ne remplaçons pas ces données."),
               tags$img(src="logigramme.png", height=400)),
           h3("Quelques informations sur le capteur dans sa globalité"),
           uiOutput(ns("resume")),
           h3("Distribution des valeurs manquantes avant et après traitement (pour le capteur sélectionné)"),
           uiOutput(ns("plot_sensor")),
           h3("Proportion de valeurs manquantes par mois après traitement (pour tous les capteurs importés)"),
           uiOutput(ns("plot_resume")))
  )
}

server_8 <- function(input, output, session, data){
  ns <- session$ns
  
  # pour mettre a jour le choix des capteurs selon liste_capteur
  observe({
    updateSelectInput(session, "sensor", choices = sensor_names[which(sensor_ids %in% data$sensors)])
  })
  
  output$plot_sensor <- renderUI({
    
    if (is.null(data$sensors)){
      p("Import nécessaire")
    }else{
    
    input$mise_a_j
    
    isolate({
      sensor <- sensor_ids[which(sensor_names == input$sensor)]
      p <- gg_na_dist(data$data_comp,
                              sensor = sensor,
                              start = as.character(input$ns_date[1]),
                              end = as.character(input$ns_date[2]),
                              hours = input$ns_hour,
                              list_weekday = input$ns_sm,
                              pub_holidays = input$ns_jf,
                              holidays = input$ns_vacance)
    renderPlot(p)
    })}
  })
  
  output$resume <- renderUI({
    
    if (is.null(data$sensors)){
      p("Import nécessaire")
    }else{
      sensor <- sensor_ids[which(sensor_names == input$sensor)]
      
      texte <- na_prop(data$data_comp,
                       start = as.character(input$ns_date[1]), 
                       end = as.character(input$ns_date[2]),
                       sensor = sensor)
      HTML(paste(texte, collapse = '<br/>'))}
  })
  
  output$plot_resume <- renderUI({
    if (is.null(data$sensors)){
      p("Import nécessaire")
    }else{
      
      input$mise_a_j
      
      isolate({ p <- gg_na_heatmap(data$data_comp,
                                   start = as.character(input$ns_date[1]), 
                                   end = as.character(input$ns_date[2]), 
                                   hours = input$ns_hour,
                                   list_weekday = input$ns_sm,
                                   pub_holidays = input$ns_jf,
                                   holidays = input$ns_vacance)
      renderPlot(p)
      })
      }
  })
  
  output$downloadData <- downloadHandler(
    filename = "Donnee_complete.csv",
    content = function(file) {
      write_excel_csv2(data$data_comp, file)
    }
  )
}


