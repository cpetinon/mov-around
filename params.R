
########
# Sensor list
########

# /!\ The order of the following lists is important, as it links sensor ids to their names /!\

sensor_ids <- c(9000002156, 9000001906, 9000001618,9000003090,9000002453,9000001844,
                9000001877,9000002666,9000002181,9000002707,9000003703,
                9000003746,9000003775,9000003736,9000004971,9000004130,
                9000004042,9000004697)

sensor_names <- c("Burel-01","Leclerc-02","ParisMarche-03","rueVignes-04","ParisArcEnCiel-05","RteVitre-06",
                  "RueGdDomaine-07","StDidierNord-08","rueVallee-09","StDidierSud-10","RuePrieure-11",
                  "RueCottage-12","RueVeronniere-13","RueDesEcoles-14","RueManoirs-15","RueToursCarree-16",
                  "PlaceHotelDeVille-17","BoulevardLiberte-18")

sensor_comp_names <- c("Burel_comp-01","Leclerc_comp-02","ParisMarche_comp-03","rueVignes_comp-04",
                       "ParisArcEnCiel_comp-05","RteVitre_comp-06", "RueGdDomaine_comp-07",
                       "StDidierNord_comp-08","rueVallee_comp-09","StDidierSud_comp-10","RuePrieure_comp-11",
                       "RueCottage_comp-12","RueVeronniere_comp-13","RueDesEcoles_comp-14","RueManoirs_comp-15",
                       "RueToursCarree_comp-16", "PlaceHotelDeVille_comp-17","BoulevardLiberte_comp-18")


starting_date <- "2021-01-01"
ending_date <- Sys.Date()