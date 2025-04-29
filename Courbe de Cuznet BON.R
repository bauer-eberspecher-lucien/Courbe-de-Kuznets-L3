######PARTIE REGRESSIO#######  :

install.packages("openxlsx")
library(openxlsx)

chemin_fichier <- "C:/Users/bauer/OneDrive/Bureau/économétrie/DONNES CDK.xlsm"
donnees_excel <- read.xlsx(chemin_fichier, sheet = 1)

install.packages("ggplot2")
library(ggplot2)

#ON TRACE DEJA LE NUAGE DE POINTS POUR AVOIR UNE IDEE PREMIERE DES LIENS ENTRE NOTRE VARIABLE EXPLICATIVE ET NOTRE VARIABLE EXPLIQUEE
plot(donnees_excel$"PIB/Hab", donnees_excel$"ConsommationEnergie/Hab_kg/ep", xlab = "PIB par Habitant", ylab = "Consommation d'Énergie", col = "lightblue", pch = 16)
title(main = "Nuage de points : Relation entre la consommation d'énergies et le PIB par habitant", col.main = "lightpink", cex.main = 0.8)

#ON REMARQUE QUE LA COURBE TRACEE LAISSE SUPPOSER L'APPARITION D'UNE COURBE EN U INVERSE

#JE NE SAIS PAS POURQUOI MAIS POUR R LES VALEURS DE NOTRE TABLEUR EXCEL NE SONT PAS NUMERIQUES.
#ALORS ON  LES "NUMERISES" PUIS ON LES NORMALISE CAR LES VALEURS SONT TROP GRANDES CE QUI FAIT QUE LES GRAPHIQUES SONT ERRONNES
donnees_excel$PIB_Hab_Normalise <- scale(as.numeric(donnees_excel$`PIB/Hab`))
donnees_excel$ConsommationEnergie_Normalisee <- scale(as.numeric(donnees_excel$`ConsommationEnergie/Hab_kg/ep)`))

#TRACONS UNE DROITE DE REGRESSION AFIN D'APROXIMER NOTRE NUAGE DE POINTS
ggplot(donnees_excel, aes(x = PIB_Hab_Normalise, y = ConsommationEnergie_Normalisee)) +
  geom_point(color = "lightgreen", size = 3) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "gold") +
  labs(title = "Régréssion entre PIB par Habitant et la Consommation d'Énergie",
       x = "PIB par Habitant (Normalisé)",
       y = "Consommation d'Énergie (Normalisée)")

#EXPLIQUATION : ON VEUT CREER UN MODELE POLYNOMIAL DE TYPE : y = b0 + b1x + b2x² + e 
#LA FONCTION geom_smooth  utilise le terme d'erreur de la régression linéaire pour ajuster la courbe :
#CELA SIGNIFIE QUE Le terme d'erreur est inclut implicitement dans le modèle linéaire.


#MAINTENANT NOUS CODONS LE MODELE POLYNOMIAL 
modele_polynomial <- lm(`ConsommationEnergie_Normalisee` ~ `PIB_Hab_Normalise` + I(`PIB_Hab_Normalise`^2), data = donnees_excel)
summary(modele_polynomial)
#ON REMARQUE BCP D'ETOILES DANS LE MODELE CE QUI EST BON SIGNE : INTERPRETONS :



donnees_excel$ConsommationEnergie_Pred <- predict(modele_polynomial, newdata = donnees_excel)

#TRACONS LA COURBE DE KUZNET POUR REGARDER SI ON A OU NON UNE COURBE EN U INVERSSE QUI VA APPARAITRE
ggplot(donnees_excel, aes(x = PIB_Hab_Normalise, y = ConsommationEnergie_Normalisee)) +
  geom_point(color = "deeppink", size = 2) +
  geom_line(aes(y = ConsommationEnergie_Pred), color = "black", size = 1) +
  labs(title = "Courbe de Kuznets",
       x = "PIB par Habitant (Normalisé)",
       y = "Consommation d'Énergie (Normalisée)")

#ON REMARQUE L'APPARITION D'UNE COURBE EN U INVERSE : INTERPRETONS

#L'apparition d'une courbe en U inversé indique qu'il existe une relation non linéaire entre le PIB par habitant et la consommation d'énergie. Initialement,
#une augmentation du PIB par habitant peut être associée à une augmentation de la consommation d'énergie, ce qui est conforme à l'idée de la courbe de Kuznets.
#Cependant, au-delà d'un certain niveau de richesse économique, une augmentation supplémentaire du PIB par habitant pourrait être associée à une diminution
#de la consommation d'énergie par habitant.

#POINT D'INFLEXION : 
#Le point d'inflexion indique le niveau de richesse auquel la relation entre le PIB par habitant et la consommation d'énergie commence à inverser sa direction.
#Dans votre modèle, le point d'inflexion serait associé à la valeur où la combinaison des coefficients linéaires et quadratiques équivaut à zéro.
