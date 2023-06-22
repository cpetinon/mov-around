

shinyUI(
  fluidPage(
    theme = shinytheme("journal"),
    navbarPage(
      tags$img(src="AGT.png",height=35),
      tabPanel("Accueil",ui_1("1")),
      tabPanel("Import",ui_2("2")),
      tabPanel("Comparaison de périodes",ui_3("3")),
      tabPanel("Heure d'engorgement", ui_4("4")),
      tabPanel("Seuil d'engorgement", ui_5("5")),
      tabPanel("Comparaison de deux capteurs", ui_6("6")),
      tabPanel("Méthodes et avertissements",ui_7("7"))
    ))     
)