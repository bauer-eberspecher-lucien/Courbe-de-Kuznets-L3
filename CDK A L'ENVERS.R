


install.packages("openxlsx")
library(openxlsx)

chemin_fichier <- "C:/Users/bauer/OneDrive/Bureau/économétrie/DONNES CDK.xlsm"
donnees_excel <- read.xlsx(chemin_fichier, sheet = 1)  


install.packages("ggplot2")
library(ggplot2)


chemin_fichier <- "C:/Users/bauer/OneDrive/Bureau/économétrie/DONNES CDK.xlsm"
donnees_excel <- read.xlsx(chemin_fichier, sheet = 1)

names(donnees_excel)


plot(donnees_excel$"ConsommationEnergie/Hab_kg/ep", donnees_excel$"PIB/Hab")

#EN GROS JE POUVAIS PAS TRACER LE GRAPHIQUE AVEC GGPLOT PCQ CETAIT UN ESPACE TRIDIMENSIONNEL
#AVEC DES VALEURS ABERRANTES DONC J'AI TRACé AVEC LA FONCTION PLOT SIMPLE

#MAINTENAT UNE DROITE DE RL POUR APPROXIMER LE NUAGE DE POINTS

# JSP PQ MAIS R DIT QUE LES DONNEES DE LA COLONNE 1 SONT PAS NUMERIQUES DC JE LES NUMERISES AVANT DE LES NORMALISER

donnees_excel$ConsommationEnergie_Normalisee <- scale(as.numeric(donnees_excel$`ConsommationEnergie/Hab_kg/ep)`))
donnees_excel$PIB_Hab_Normalise <- scale(as.numeric(donnees_excel$`PIB/Hab`))

#TRACONS LA DROITE DE RL
ggplot(donnees_excel, aes(x = ConsommationEnergie_Normalisee, y = PIB_Hab_Normalise)) +
  geom_point(color = "blue", size = 2) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(title = "Relation entre la Consommation d'Énergie et le PIB par Habitant",
       x = "Consommation d'Énergie (Normalisée)",
       y = "PIB par Habitant (Normalisé)")

#MTN ON VERIFIE LA THEORIE DE L'EKC EN METTANT EN PLACE UNE FORME QUADRATIQUE POUR LA DROITE DE REGRESSION LINEAIRE

names(donnees_excel)

donnees_excel$ConsommationEnergie_Normalisee <- scale(as.numeric(donnees_excel$'ConsommationEnergie/Hab_kg/ep)'))
donnees_excel$PIB_Hab_Normalise <- scale(as.numeric(donnees_excel$`PIB/Hab`))
modele_polynomial <- lm(`PIB_Hab_Normalise` ~ `ConsommationEnergie_Normalisee` + I(`ConsommationEnergie_Normalisee`^2), data = donnees_excel)
summary(modele_polynomial)
#ON A OBTENU LES INFORMATIONS PERTINENTES DE LA REGRESSION DE LA CONSOMATION D'ENERGIES SUR LE PIB PAR HABITANT
#LE MODELE A PAS L'AIR FOU DU TOUT

#MAINTENANT : EKC

donnees_excel$PIB_Hab_Pred <- predict(modele_polynomial, newdata = donnees_excel)


ggplot(donnees_excel, aes(x = ConsommationEnergie_Normalisee, y = PIB_Hab_Normalise)) +
  geom_point(color = "blue", size = 2) +
  geom_line(aes(y = PIB_Hab_Pred), color = "red", size = 1) +
  labs(title = "Courbe de Kuznets",
       x = "Consommation d'Énergie (Normalisée)",
       y = "PIB par Habitant (Normalisé)")