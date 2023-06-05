[![CC BY-SA 4.0][cc-by-sa-shield]][cc-by-sa]

This work is licensed under a
[Creative Commons Attribution-ShareAlike 4.0 International License][cc-by-sa].

[![CC BY-SA 4.0][cc-by-sa-image]][cc-by-sa]

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/
[cc-by-sa-image]: https://licensebuttons.net/l/by-sa/4.0/88x31.png
[cc-by-sa-shield]: https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg

# Mov'Around

Mov'Around est un outil d'analyse de données de mobilités issues de capteurs Telraam.

Mov'Around is mobility data analysis tool for Telraam sensors.

# Faire fonctionner l'appli

L'outil de data visualisation *mov-around* est une application [`R shiny`](https://shiny.rstudio.com/). On peut la faire fonctionner en local (la machine de l'utilisateur jouant à la fois le rôle d'interface utilisateur et de serveur), ou bien la déployer sur un serveur dédié afin de permettre son utilisation depuis un simple navigateur web.

## En local

L'utilisateur souhaitant faire tourner l'appli sur sa machine doit disposer sur cette dernière d'un ensemble de logiciels :

- Le [logiciel R](https://cran.r-project.org/bin/windows/base/)
- L'IDE [RStudio](https://posit.co/download/rstudio-desktop/), en version gratuite
- Le [logiciel de gestion de version Git](https://git-scm.com/downloads)
- Si la machine tourne sous Windows, le [logiciel RTools](https://cran.r-project.org/bin/windows/Rtools/) en version compatible avec la version de R

Il est également nécessaire d'installer des *packages* R :


- shiny, shinythemes : pour faire fonctionner l'appli et la mettre en forme en style "journal"
- httr, jsonlite : pour l'import des données
- tibble, dplyr, lubridate : pour mettre en forme les tableaux de données
- ggplot2, plotly, cowplot : pour produire les graphiques
- CPAT, synchrony, forecast, zoo : pour les stats
- readr : pour exporter les tableaux de données en .csv compatible Excel

On pourra lancer dans le terminal R de RStudio la commande :

```R
install.packages(c("shiny", "shinythemes", "httr", "jsonlite", "tibble", "dplyr", "lubridate", "ggplot2", "plotly", "cowplot", "CPAT", "synchrony", "forecast", "zoo", "readr"))
```

Ensuite, il faut télécharger le projet (fork ou clone) pour en disposer en local, puis le démarrer dans RStudio avec le fichier 
`Projet.Rproj ` qui est à la racine du projet. Si l'on ouvre dans RStudio le fichier `server.R` ou bien `ui.R`, l'IDE propose un bouton `Run App`. Il suffit de cliquer dessus ou d'exécuter la commande

```R
shiny::runApp()
```

## Déployer l'appli

Ici nous utilisons la solution d'hébergement *shinyapps* proposée par *RStudio*, qui nécessite l'installation préalable du package `rsconnect` et de disposer d'une autentification. Quand c'est le cas, un bouton "Publish Application" est visible dans RStudio, et il faut cliquer dessus.

## Utilisation de l'appli déployée

Les utilisateurs doivent simplement se rendre sur l'url [https://agistaterre.shinyapps.io/traffic_routier/](https://agistaterre.shinyapps.io/traffic_routier/)


