#' Donnees mesurees du batiment KIUT de Tachkent
#'
#' Rassemble les donnees REELLES publiees pour un batiment en beton arme de
#' 6 etages a Tachkent (Ouzbekistan), et les grandeurs qu'on en deduit.
#'
#' Source : Minowa, Kaniev et Rustamov (2023), E3S Web of Conferences,
#' vol. 401, article 05031, licence CC BY 4.0. Donnees mesurees : frequence
#' propre d'environ 1,55 Hz, amplification entre le 1er et le 6e etage
#' (environ 26 dans la direction Y et 15 dans la direction X), vibration
#' ambiante d'environ 2 cm/s2, geometrie de 72 m sur 48 m et 29,1 m de haut.
#'
#' L'amortissement est estime par \eqn{\xi \approx 1 / (2 A)}, ou \eqn{A} est
#' l'amplification a la resonance (estimation grossiere : un seul mode).
#'
#' @param direction `"Y"` (par defaut) ou `"X"`.
#' @return Une liste avec le nom, la source, le nombre d'etages, la hauteur
#'   (m), le plan (m), la direction, `f0` (Hz), `omega0` (rad/s),
#'   `amplification`, `xi`, `acceleration_ambiante` (m/s2) et
#'   `deplacement_ambiant` (m).
#' @examples
#' kiut_building()
#' @export
kiut_building <- function(direction = c("Y", "X")) {
  direction <- match.arg(direction)
  amplification <- c(Y = 26, X = 15)[[direction]]
  f0 <- 1.55
  omega0 <- 2 * pi * f0
  acceleration <- 0.02
  list(
    nom = "Batiment de 6 etages, Tachkent (Ouzbekistan)",
    nom_court = "Batiment KIUT (Tachkent)",
    source = "Minowa, Kaniev et Rustamov (2023), E3S Web of Conferences 401, 05031",
    etages = 6,
    hauteur = 29.1,
    plan = c(72, 48),
    direction = direction,
    f0 = f0,
    omega0 = omega0,
    amplification = amplification,
    xi = 1 / (2 * amplification),
    acceleration_ambiante = acceleration,
    deplacement_ambiant = acceleration / omega0^2
  )
}

#' Scenarios de validite du modele lineaire pour un batiment
#'
#' Pour plusieurs valeurs du rapport \eqn{|k_3|/k} (en m^-2), calcule
#' l'amplitude limite du modele lineaire ([linear_limit()]) et la compare aux
#' deplacements mesures et au deplacement de service \eqn{H/500}.
#'
#' Seul le rapport \eqn{k_3/k} intervient : la masse n'a pas besoin d'etre
#' connue. Ce rapport ne peut pas etre mesure sur un batiment sain : on le
#' traite comme un scenario.
#'
#' @param case Une liste decrivant le batiment, comme celles renvoyees par
#'   [kiut_building()] ou [ubc_building()] (champs `hauteur`,
#'   `deplacement_ambiant` et, facultatif, `deplacement_essai`).
#' @param ratios Valeurs de \eqn{|k_3|/k} (m^-2).
#' @param tol Tolerance relative (par defaut `0.05`).
#' @return Un data.frame avec les colonnes `rapport_k3_k`,
#'   `amplitude_limite_m`, `deplacement_ambiant_m`, `marge_ambiant`,
#'   `deplacement_service_m`, `lineaire_valide_service` et, si le batiment
#'   a un `deplacement_essai`, `deplacement_essai_m` et `marge_essai`.
#' @examples
#' case_scenarios(kiut_building())
#' @export
case_scenarios <- function(case, ratios = c(1, 10, 100), tol = 0.05) {
  x_service <- case$hauteur / 500
  limite <- linear_limit(k = 1, k3 = -ratios, tol = tol)
  res <- data.frame(
    rapport_k3_k = ratios,
    amplitude_limite_m = limite,
    deplacement_ambiant_m = case$deplacement_ambiant,
    marge_ambiant = limite / case$deplacement_ambiant,
    deplacement_service_m = x_service,
    lineaire_valide_service = x_service <= limite
  )
  if (!is.null(case$deplacement_essai)) {
    res$deplacement_essai_m <- case$deplacement_essai
    res$marge_essai <- limite / case$deplacement_essai
  }
  res
}

#' Simulation d'un batiment : modele lineaire et non lineaire
#'
#' Simule le batiment force a sa frequence propre. La force est choisie pour
#' que le deplacement lineaire en regime etabli atteigne le deplacement de
#' service \eqn{H/500}. La raideur vaut \eqn{k = m \omega_0^2} et
#' \eqn{k_3 = -ratio \cdot k}. La masse `mass` n'influence pas les
#' deplacements, car elle se simplifie dans l'equation du mouvement.
#'
#' @param case Une liste decrivant le batiment ([kiut_building()] ou
#'   [ubc_building()]).
#' @param ratio Rapport \eqn{|k_3|/k} (m^-2), par defaut `10`.
#' @param mass Masse (kg), par defaut `2.1e7` (estimation).
#' @param t_end Duree de la simulation (s). Si `NULL`, elle est choisie assez
#'   longue pour atteindre le regime etabli (au moins 60 s).
#' @return Un objet `ggplot` (voir [compare_models()]).
#' @examples
#' case_compare(kiut_building(), ratio = 10)
#' @export
case_compare <- function(case, ratio = 10, mass = 2.1e7, t_end = NULL) {
  if (is.null(t_end)) t_end <- max(60, 8 / (case$xi * case$omega0))
  k <- mass * case$omega0^2
  x_service <- case$hauteur / 500
  x_statique <- 2 * case$xi * x_service
  F0 <- k * x_statique
  compare_models(m = mass, k = k, k3 = -ratio * k, xi = case$xi, F0 = F0,
                 Omega = case$omega0, t_end = t_end) +
    ggplot2::labs(
      title = sprintf("%s : scenario k3/k = -%g m^-2", case$nom_court, ratio)
    )
}

#' Scenarios de validite pour le batiment KIUT
#'
#' Raccourci de [case_scenarios()] applique a [kiut_building()].
#'
#' @param ratios Valeurs de \eqn{|k_3|/k} (m^-2).
#' @param tol Tolerance relative (par defaut `0.05`).
#' @param direction `"Y"` ou `"X"`.
#' @return Un data.frame (voir [case_scenarios()]).
#' @examples
#' kiut_scenarios()
#' @export
kiut_scenarios <- function(ratios = c(1, 10, 100), tol = 0.05,
                           direction = "Y") {
  case_scenarios(kiut_building(direction), ratios = ratios, tol = tol)
}

#' Simulation du batiment KIUT : modele lineaire et non lineaire
#'
#' Raccourci de [case_compare()] applique a [kiut_building()].
#'
#' @param ratio Rapport \eqn{|k_3|/k} (m^-2), par defaut `10`.
#' @param mass Masse (kg), par defaut `2.1e7` (estimation).
#' @param t_end Duree de la simulation (s), par defaut `60`.
#' @param direction `"Y"` ou `"X"`.
#' @return Un objet `ggplot` (voir [compare_models()]).
#' @examples
#' kiut_compare(ratio = 10)
#' @export
kiut_compare <- function(ratio = 10, mass = 2.1e7, t_end = 60,
                         direction = "Y") {
  case_compare(kiut_building(direction), ratio = ratio, mass = mass,
               t_end = t_end)
}
