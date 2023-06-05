ui_7 <- function(id){
  tagList(
           h3("Avertissement relatif à la qualité des données :"),
           p("Les données des capteurs Telraam ne sont pas issues d’une mesure continue sur une heure. Pour améliorer la qualité des données futures, les capteurs dédient une partie de leur temps d’activité à l’apprentissage. Les données totales sont reconstituées à partir du temps de mesures. Plus cette période de mesure est longue plus la qualité des données est grande. Telraam donne un outil de mesure de ce temps de mesure : l’uptime. Dans cette application, nous n'avons conservé que les données d’uptime supérieur à 0.5 (seuil conseillé par Telraam). Toutefois, les capteurs placés récemment (en période d’apprentissage) et les données matinales ou dans la soirée (visibilité réduite à cause de la nuit)  peuvent présenter des uptimes plus faible. 
          De plus la suppression des données à l’uptime trop faible fait qu’on possède moins de données pour les périodes à risque. La qualité des estimations et des tests est moins bonne sur ces périodes.
          Il faut donc être prudent en interprétant ces données."),
           h3("Avertissement relatif aux catégories de mobilités :"),
           p("Les capteurs Telraam utilisés ont des difficultés à différencier les grosses voitures comme les SUV des poids lourds. Le nombre de poids lourds est donc sur-évalué et le nombre de voitures sous-évalué. Toutefois, le total voitures + camions est précis.
          De la même façon, il faut être prudent dans la différenciation entre vélos et piétons."),
           h3("Explicitation des statistiques mis en place (nécessite des compétences en statistique) :"),
           h4("Comparaison de périodes :"),
           p("Méthode pour tracer les courbes :") ,
           p("Selon la sélection de l’utilisateur, on filtre les données pour ne garder que le trafic correspondant aux mobilités, capteur, direction et contraintes de dates sélectionnés. On réalise ensuite une moyenne pour chaque créneau horaire.
          On a rajouté un intervalle de confiance à 95% autour de nos courbes. Pour chaque créneau horaire, on a estimé la variance des données, ce qui nous a permis d’obtenir l’intervalle de confiance (à partir d’une loi de Student)."),
           br(),
           p("Méthode pour la significativité de la différence :"),
           p("On s’appuie sur un test de Wilcoxon Mann Whitney. L’idée est de comparer, pour chaque créneau horaire, la répartition des valeurs de chacune des périodes. Le test consiste à regarder la distance entre les fonctions de répartition empirique, si elles sont éloignées, le test rejette l’hypothèse nulle :  l’égalité des lois. L’option « Significatif » indique une p-value inférieure à 0.05, celle « Entre deux » une  p-value entre 0.05 et 0.1 et celle « Non significatif » une p-value supérieure à 0.1."),
           h4("Seuil d’engorgement :"),
           p("Méthode pour tracer les courbes :"),
           p("On commence par filtrer les données selon les sélections de l’utilisateur. On isole la partie correspondant au pourcentage de conducteur dépassant chaque vitesse. On range les données dans l’ordre croissant du nombre de véhicules (voitures + camions). On a pré-lissé les données à l’aide d’une moyenne glissante d’une amplitude de 50 pour dégager un début tendance (courbe noir du graphique). À partir de cette tendance, on a lissé nos données à l’aide de l’outil geom_smooth de ggplot2. Ces courbes de lissages sont les courbes colorées du graphique."),
           br(),
           p("Méthode pour trouver le seuil :"),
           p("L’objectif est de déterminer un seuil de rupture dans la courbe de lissage. Pour cela, on utilise un test de Darling Erdös (dérivé du test de CUSUM). La fonction est implémentée dans le Package",
             tags$a(href="https://github.com/ntguardian/CPAT","CPAT"), 
             "(Curtis Miller)."),
           h4(" Comparaison de période :"),
           p("Séparation en tendance, cycle et bruit :"),
           p("Après un filtrage des données selon les choix de l’utilisateur, on détermine la période d’activité commune des deux capteurs sélectionnées. 
          Pour trouver la tendance (évolution générale du flux),  pour chaque capteur, on réalise une moyenne glissante sur 14 jours (2 périodes hebdomadaires), c’est le graphique du premier onglet. 
          On soustrait la tendance au flux total pour avoir de données sans tendance. Pour ces données, on fait une moyenne sur tous les lundis, puis les mardis, etc. Cela nous donne les valeurs associées au cycle de la semaine (second onglet). La partie restante après la soustraction du cycle hebdomadaire correspond au bruit statistique (troisième onglet)."),
           br(),
           p("Indicateurs du lien entre les bruits :"),
           br(),
           p("Le premier indicateur est le coefficient de corrélation de Pearson. On a fait choix de seuils pour afficher différents commentaires :"),
           p("1. Pour un coefficient plus grand que 0.5 on considère que les courbes sont corrélées."),
           p("2. Pour un coefficient entre 0.2 et 0.5 on considère que la corrélation est légère."),
           p("3. Pour un coefficient inférieur à 0.2 on considère que les courbes sont non corrélées."),
           br(),
           p('Le second indicateur est un indicateur de la proportion d’extremums communs entre les deux courbes. Pour cela, on utilise la fonction « peaks » du package «', 
             tags$a(href="https://github.com/tgouhier/synchrony","synchrony"),
             '». Cette fonction compte le nombre de fois où les deux séries atteignent un maximum en même temps, puis les minimums pour ramener cela à la proportion totale de pics (déterminée en sommant le nombre de maximums de la série en comptant le plus à celui de minimums).
          Pour tester si ce nombre est important la fonction procède à une estimation via  une méthode de Monte Carlo, en mélangeant plusieurs fois les deux séries pour observer le nombre de pics communs dans chaque cas, et voir si ces valeurs sont éloignées ou non de la proportion initiale.
          Si on rejette l’hypothèse que la synchronicité des pics est du au hasard, on affiche "Les pics des deux courbes sont atteints en même temps très souvent.", sinon "On ne peut pas dire que les pics des deux courbes sont souvent atteints en même temps.".')
  )
}