##### PARTIE STATISTIQUES DESCRIPTIVES #####


install.packages("openxlsx")
library(openxlsx)

chemin_fichier <- "C:/Users/bauer/OneDrive/Bureau/économétrie/DONNES CDK.xlsm"
donnees_excel <- read.xlsx(chemin_fichier, sheet = 1)

#RESUME DONNEES StATISTIQUES BASE DE DONNEES 
summary(donnees_excel)

#MOYENNE, MEDIANE, ECART TYPE
#R CONSIDERE QUE CERTAINES DONNEES DE PIB/HAB NE SONT PAS NUMERIQUES : ON CORRIGE CELA
donnees_excel$"PIB/Hab" <- as.numeric(donnees_excel$"PIB/Hab")
mean(donnees_excel$"PIB/Hab")
median(donnees_excel$"PIB/Hab")
sd(donnees_excel$"PIB/Hab")

#TABLEAU CROISES

table(donnees_excel$"PIB/Hab")

table(donnees_excel$`ConsommationEnergie/Hab_kg/ep)`)

#BOXPLOT

boxplot(donnees_excel$"PIB/Hab", donnees_excel$"AutreVariable", names = c("PIB/Hab", "Autre Variable"), col = c("lightblue", "lightgreen"), pch = 16)


#HISTOGRAMME
donnees_excel$`PIB/Hab` <- as.numeric(donnees_excel$`PIB/Hab`)

hist(donnees_excel$`PIB/Hab`,
     main = "Histogramme : Fréquence du PIB par Habitant",
     xlab = "PIB par Habitant",
     ylab = "Pourcentage",
     col = "lightblue",
     border = "black",
     freq = FALSE)


donnees_excel$`ConsommationEnergie/Hab_kg/ep)` <- as.numeric(donnees_excel$`ConsommationEnergie/Hab_kg/ep)`)
hist(donnees_excel$`ConsommationEnergie/Hab_kg/ep)`,
     main = "Histogramme : Fréquence de la Consommation d'Énergie par Habitant",
     xlab = "Consommation d'Énergie par Habitant (kg/ep)",
     ylab = "Pourcentage",
     col = "orange",
     border = "black",
     freq = FALSE,
     cex.main = 0.8)

#VARIANCE, ECART TYPE ET VALEURS EXTREMES SUR LA CONSOMATION D'ENERGIES
variance <- var(donnees_excel$`ConsommationEnergie/Hab_kg/ep)`)
cat("Variance :", variance, "\n")
EcartType <- sqrt(variance)
print(EcartType)
min_value <- min(donnees_excel$`ConsommationEnergie/Hab_kg/ep)`)
max_value <- max(donnees_excel$`ConsommationEnergie/Hab_kg/ep)`)
cat("Valeur minimale :", min_value, "\n")
cat("Valeur maximale :", max_value, "\n")


#VARIANCE ECART TYPE ET VALEURS EXTREMES SUR LE PIB PAR HABITANT
donnees_excel$`PIB/Hab` <- as.numeric(donnees_excel$`PIB/Hab`)
variance_pib <- var(donnees_excel$`PIB/Hab`)
EcartType2 <- sqrt(variance_pib)
print(EcartType2)
cat("Variance PIB/Hab :", variance_pib, "\n")
min_value_pib <- min(donnees_excel$`PIB/Hab`)
max_value_pib <- max(donnees_excel$`PIB/Hab`)
cat("Valeur minimale PIB/Hab :", min_value_pib, "\n")
cat("Valeur maximale PIB/Hab :", max_value_pib, "\n")
     

     
#HISTOGRAMME COMPLEXE

install.packages("ggplot2")
library("ggplot2")
install.packages("scales")
library("scales")

donnees_excel$`ConsommationEnergie/Hab_kg/ep)` <- as.numeric(donnees_excel$`ConsommationEnergie/Hab_kg/ep)`)
donnees_excel$`PIB/Hab` <- as.numeric(donnees_excel$`PIB/Hab`)

ggplot(donnees_excel, aes(x = `ConsommationEnergie/Hab_kg/ep)`, fill = `PIB/Hab`)) +
  geom_histogram(binwidth = 10, position = "dodge", color = "black") +
  labs(title = "Histogramme : Consommation d'Énergie vs PIB par Habitant",
       x = "Consommation d'Énergie",
       y = "Fréquence",
       fill = "PIB par Habitant") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "PIB par Habitant", breaks = pretty(donnees_excel$`PIB/Hab`, n = 5)) +
  facet_wrap(~Année, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#PAS ASSEZ LISIBLE


