ui_1 <- function(id){
  tagList(
           p("Bienvenue sur cette application ayant pour objectif de fournir des outils pour l’analyse
                 des données de mobilités captés par les capteurs Telraam installé à Chateaubourg.
                 Cette application se décompose en plusieurs onglets."),
           HTML('<center><img src="chateaubourg.jpg" height="500"></center>'),
           h5("Onglet import :"),
           p("Cet onglet permet d’importer les données que vous voulez analyser. 
                 L'import pouvant être long, il est judicieux de ne choisir que les capteurs et la période qui vous intéresse. 
                 Il est important de commencer par importer les données, sinon aucune analyse ne peut être faite. 
                 L’import est réussi si le texte en bas de l’onglet passe de « Import à faire » à « Fait »."),
           h5("Onglet comparaison de périodes :"),
           p("Cet onglet permet de voir, pour un même capteur, s’il y a une différence de comportement
               des usagers entre périodes différentes. Pour cela, vous devez choisir les caractéristiques
               d’une période de référence et d’une ou deux périodes avec lesquelles vous voulez les comparer."),
           p(" Les différences s’observent avec 2 outils :"),
           p("- Un graphique montrant la circulation moyenne en fonction des heures de la journée
               pour chaque période."),
           p("- Une barre colorée, donnant pour chaque heure, le résultat d’un test statistique pour
                 qualifier la différence de répartition entre 2 périodes."),
           h5("Onglet heure d’engorgement :"),
           p("Cet onglet permet de visualiser, pour un capteur, la circulation dans chaque direction
                 (voitures et poids lourds) et la vitesse V85 (vitesse telle que 85 % des usagers roulent 
                 plus lentement), en fonction des heures de la journée. Cela permet d’observer les heures
                 pouvant causer des ralentissements."),
           h5("Onglet seuil d’engorgement :"),
           p("Cet onglet permet de visualiser le pourcentage d’usagers (voitures et poids lourds) arrivant
                 à dépasser 10km/h, 20km/h, 30km/h et 40km/h en fonction du nombre de véhicules sur la route.
                 L’objectif est de pouvoir chercher un changement brusque dans ces courbes pour déterminer un
                 seuil d’apparition des ralentissements. Le programme propose un seuil calculé automatiquement.
                 Ce seuil n’est pas forcément précis, vous pouvez décider d’afficher un seuil manuel."),
           h5("Onglet comparaison de deux capteurs :"),
           p("Cet onglet permet de comparer le comportement des usagers de 2 capteurs différents. Pour
               cela 3 graphiques sont proposés, avec chacun une courbe par capteur :"),
           p("- Un graphique de la tendance, qui correspond à l’évolution liée à la période de l’année"),
           p("- Un graphique montrant l’effet de chaque jour de la semaine."),
           p("- Un graphique représentant les variations propre à chaque jour (ce qui reste après avoir enlevé
                 l’effet de la tendance et du cycle hebdomadaire ).
                 Le dernier graphique est accompagné de 2 outils pour faciliter la mesure du lien entre les courbes :
                 un concernant la corrélation et un autre la synchronicité de celles-ci."),
           br(),
           p("Vous avez le choix entre 2 options : visualiser les courbes brutes (non normalisées) pour faire
                 une analyse quantitative ou les visualiser en les ramenant à la même échelle (normalisées) 
                 pour une analyse qualitative."),
           h5("Onglet méthodes et avertissements :"),
           p("Donne des avertissements relatifs à l’interprétation des données et les méthodes statistiques
                 utilisées pour mettre en place l’application.")
  )
}