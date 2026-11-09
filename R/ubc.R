#' Signaux du batiment de 18 etages de l'UBC (Vancouver)
#'
#' Charge un essai de vibration mesure sur un batiment hybride bois-beton de
#' 18 etages (53 m de haut). Les donnees sont les vitesses (cm/s), echantillonnees
#' a 200 Hz, du capteur le plus haut (capteur 8).
#'
#' Source : Miyazu Y. et Loss C. (2023). Lateral vibration data of an actual
#' 18-story timber-concrete hybrid building. Mendeley Data,
#' DOI 10.17632/rt7w3txv2z.1. Licence CC BY 4.0.
#'
#' @param case `"3_Y_free"` (vibration libre apres des poussees humaines, 60 s),
#'   `"1_Y"` (microtremblements, direction transversale, 300 s) ou `"1_X"`
#'   (microtremblements, direction longitudinale, 300 s).
#' @return Un data.frame avec les colonnes `t` (s) et `v` (cm/s).
#' @examples
#' head(ubc_signal("3_Y_free"))
#' @export
ubc_signal <- function(case = c("3_Y_free", "1_Y", "1_X")) {
  case <- match.arg(case)
  fichier <- switch(case,
                    "3_Y_free" = "ubc_case3_y_free_top.csv",
                    "1_Y" = "ubc_case1_y_top.csv",
                    "1_X" = "ubc_case1_x_top.csv"
  )
  chemin <- system.file("extdata", fichier, package = "taylorosc")
  if (chemin == "") stop("fichier de donnees introuvable : reinstaller le package.")
  utils::read.csv(chemin)
}

#' Bâtiment UBC de 18 etages : parametres estimes sur des mesures brutes
#'
#' Applique la chaine complete a des signaux reels : la frequence propre est
#' estimee par le spectre (vibration libre apres l'arret des poussees), le taux
#' d'amortissement par la decroissance de cette vibration, et les deplacements
#' mesures a partir des vitesses maximales ( \eqn{x = v_{max} / \omega_0} ).
#'
#' @return Une liste du meme type que [kiut_building()] : `nom`, `nom_court`,
#'   `source`, `etages`, `hauteur` (m), `plan` (m), `f0` (Hz), `omega0` (rad/s),
#'   `xi`, `amplification`, `deplacement_ambiant` (m, microtremblements),
#'   `deplacement_essai` (m, essai de poussee), et les frequences des
#'   microtremblements `f0_microtremor_y` et `f0_microtremor_x`.
#' @examples
#' bat <- ubc_building()
#' bat$f0
#' bat$xi
#' @export
ubc_building <- function() {
  micro_y <- ubc_signal("1_Y")
  micro_x <- ubc_signal("1_X")
  libre <- ubc_signal("3_Y_free")
  fs <- 1 / stats::median(diff(micro_y$t))

  f_micro_y <- estimate_frequency(micro_y$v, fs, fmin = 0.3, fmax = 5)$f0
  f_micro_x <- estimate_frequency(micro_x$v, fs, fmin = 0.3, fmax = 5)$f0

  # Frequence propre : phase de vibration libre (apres t = 20 s)
  f0 <- estimate_frequency(libre$v[libre$t >= 20], fs, fmin = 0.3, fmax = 5)$f0
  amort <- estimate_damping(libre$t, libre$v, f0 = f0, t_start = 20,
                            t_end = 45, min_amplitude = 0.012)

  omega0 <- 2 * pi * f0
  v_ambiant <- max(abs(micro_y$v))  # cm/s
  v_essai <- max(abs(libre$v))      # cm/s

  list(
    nom = "Batiment hybride bois-beton de 18 etages, UBC, Vancouver (Canada)",
    nom_court = "Batiment UBC de 18 etages (Vancouver)",
    source = "Miyazu et Loss (2023), Mendeley Data, DOI 10.17632/rt7w3txv2z.1",
    etages = 18,
    hauteur = 53,
    plan = c(15, 56),
    direction = "Y",
    f0 = f0,
    omega0 = omega0,
    xi = amort$xi,
    r2_amortissement = amort$r2,
    amplification = 1 / (2 * amort$xi),
    f0_microtremor_y = f_micro_y,
    f0_microtremor_x = f_micro_x,
    deplacement_ambiant = v_ambiant / 100 / omega0,
    deplacement_essai = v_essai / 100 / omega0
  )
}
