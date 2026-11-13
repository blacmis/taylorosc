# Preparation des signaux du batiment de 18 etages de l'UBC (Vancouver)
# Source : Miyazu Y. et Loss C. (2023), Mendeley Data,
# DOI 10.17632/rt7w3txv2z.1. Licence : CC BY 4.0.
# A executer UNE fois si l'on souhaite regenerer les CSV dans inst/extdata/.

library(readxl)

# Selection interactive du dossier contenant les fichiers Excel bruts
dossier <- if (interactive()) {
  rstudioapi::selectDirectory(caption = "Choisir le dossier Data des fichiers UBC")
} else {
  stop("Executez ce script en mode interactif ou definissez la variable 'dossier'.")
}

if (is.null(dossier) || is.na(dossier)) stop("Aucun dossier choisi.")

fichiers <- c(
  "Case 1_Y.xlsx"      = "ubc_case1_y_top.csv",
  "Case 1_X.xlsx"      = "ubc_case1_x_top.csv",
  "Case 3_Y_free.xlsx" = "ubc_case3_y_free_top.csv"
)

if (!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)

for (nom in names(fichiers)) {
  donnees <- read_excel(file.path(dossier, nom), sheet = "Data")
  signal <- data.frame(
    t = donnees[["Time (s)"]],
    v = donnees[["Sensor 8"]]
  )
  utils::write.csv(
    signal,
    file.path("inst/extdata", fichiers[[nom]]),
    row.names = FALSE
  )
  message(nom, " : ", nrow(signal), " lignes enregistrees.")
}
