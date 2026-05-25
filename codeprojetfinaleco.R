library(tidyverse)
library(readxl) # Indispensable pour lire les fichiers .xlsx !

df_projet <- read_excel("C:/Users/Abdul/Desktop/excelprojetfinaleco.xlsx", sheet = "Ensemble")
colnames(df_projet) <- c("Annee", "Deficit", "Croissance", "Dette")

df_projet <- df_projet %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

cor_deficit_croissance <- cor(df_projet$Deficit, df_projet$Croissance, method = "pearson")
cor_dette_croissance <- cor(df_projet$Dette, df_projet$Croissance, method = "pearson")

cat("=============================================================\n")
cat("RESULTATS DES CALCULS DE CORRELATION :\n")
cat("1. Coefficient Pearson (Déficit vs Croissance) :", round(cor_deficit_croissance, 4), "\n")
cat("2. Coefficient Pearson (Dette vs Croissance) :", round(cor_dette_croissance, 4), "\n")
cat("=============================================================\n")

graph1 <- ggplot(df_projet, aes(x = Deficit, y = Croissance)) +
  geom_point(color = "#1e3d59", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "#ff6e40", se = FALSE, size = 1.2) +
  geom_text(aes(label = ifelse(Annee %in% c(1993, 2008, 2020), as.character(Annee), "")), 
            vjust = -1, color = "black", fontface = "bold") + # Marquer les grandes crises
  labs(
    title = "Graphique 1 : Corrélation entre le Déficit Public et la Croissance",
    x = "Déficit Public (en % du PIB)",
    y = "Taux de croissance du PIB (en %)"
  ) +
  theme_minimal()

print(graph1)

graph2 <- ggplot(df_projet, aes(x = Annee)) +
  geom_line(aes(y = Croissance, color = "Croissance du PIB (Économie réelle)"), size = 1.2) +
  geom_line(aes(y = Deficit, color = "Déficit Public (Intervention État)"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Croissance du PIB (Économie réelle)" = "#21bf73", "Déficit Public (Intervention État)" = "#b00020")) +
  labs(
    title = "Graphique 2 : Chronologie des crises et des interventions publiques",
    x = "Année", y = "Pourcentage (%)", color = "Indicateurs"
  ) +
  theme_minimal() + theme(legend.position = "bottom")

print(graph2)

graph3 <- ggplot(df_projet, aes(x = Annee)) +
  geom_bar(aes(y = Dette), stat = "identity", fill = "#3a4f7c", alpha = 0.5) +
  geom_line(aes(y = Croissance * 5), color = "#ff6e40", size = 1.2) + # Axe secondaire simulé pour la croissance
  scale_y_continuous(
    name = "Dette Publique (en % du PIB - Barres)",
    sec.axis = sec_axis(~./5, name = "Croissance du PIB (en % - Ligne Orange)")
  ) +
  labs(
    title = "Graphique 3 : Accumulation de la Dette face à la stagnation du PIB",
    x = "Année"
  ) +
  theme_minimal()

print(graph3)