# Genere toutes les figures du PowerPoint dans un dossier HORS du depot Git
# Auteur : KUEKAM ZEUFACK JEAMS NAEL

devtools::load_all()

dossier <- file.path(path.expand("~"), "figures_taylorosc")
dir.create(dossier, showWarnings = FALSE)

enregistrer <- function(graphique, nom) {
  ggplot2::ggsave(file.path(dossier, nom), plot = graphique,
                  width = 11, height = 6.2, dpi = 200)
}

# Taylor : approximation
enregistrer(plot_taylor("sin(x)"), "01_taylor_sin.png")
enregistrer(plot_taylor("exp(x)", xlim = c(-3, 3), orders = c(1, 2, 3, 5)),
            "02_taylor_exp.png")
for (n in c(1, 3, 5, 9)) {
  enregistrer(plot_taylor("sin(x)", orders = n),
              sprintf("03_taylor_sin_ordre_%d.png", n))
}

# Taylor : erreur et majorant de Lagrange
enregistrer(plot_taylor_error("sin(x)", n = 3, h_max = 1),
            "04_erreur_lagrange.png")

# Exemple de batiment (valeurs illustratives), zoom sur 10 secondes
enregistrer(
  compare_models(m = 2e6, k = 8e7, k3 = -1.6e8, F0 = 4e5,
                 Omega = sqrt(8e7 / 2e6), t_end = 60) +
    ggplot2::coord_cartesian(xlim = c(50, 60)),
  "05_modele_exemple.png")

# Etude de cas : batiment KIUT de Tachkent (mesures reelles)
enregistrer(kiut_compare(ratio = 10) +
              ggplot2::coord_cartesian(xlim = c(50, 60)),
            "06_kiut_ratio10_zoom.png")
enregistrer(kiut_compare(ratio = 100), "07_kiut_ratio100_complet.png")
enregistrer(kiut_compare(ratio = 100) +
              ggplot2::coord_cartesian(xlim = c(50, 60)),
            "08_kiut_ratio100_zoom.png")

# Tableau des scenarios
write.csv(kiut_scenarios(), file.path(dossier, "09_scenarios_kiut.csv"),
          row.names = FALSE)

message("Figures enregistrees dans : ", dossier)
