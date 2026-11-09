# Prepare les signaux du batiment de 18 etages (UBC, Vancouver) pour le package.
# Source : Miyazu Y. et Loss C. (2023). Lateral vibration data of an actual
# 18-story timber-concrete hybrid building. Mendeley Data,
# DOI 10.17632/rt7w3txv2z.1. Licence : CC BY 4.0.
# A executer UNE fois, depuis le projet taylorosc.

library(readxl)

dossier <- "C:\\Users\\DELL\\Desktop\\Nouveau dossier (5)\\Lateral vibration data of an actual 18-story timber-concrete hybrid building\\Data"
#if (is.na(dossier)) stop("Aucun dossier choisi.")

# Fichier Excel -> fichier CSV du capteur 8 (le plus haut : amplitude maximale)
fichiers <- c(
  "Case 1_Y.xlsx" = "ubc_case1_y_top.csv",
  "Case 1_X.xlsx" = "ubc_case1_x_top.csv",
  "Case 3_Y_free.xlsx" = "ubc_case3_y_free_top.csv"
)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

for (nom in names(fichiers)) {
  donnees <- read_excel(file.path(dossier, nom), sheet = "Data")
  signal <- data.frame(t = donnees[["Time (s)"]], v = donnees[["Sensor 8"]])
  utils::write.csv(signal, file.path("inst/extdata", fichiers[[nom]]),
                   row.names = FALSE)
  message(nom, " : ", nrow(signal), " lignes enregistrees")
}
