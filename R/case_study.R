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
#'   `amplification`, `xi` et `acceleration_ambiante` (m/s2).
#' @examples
#' kiut_building()
#' @export
kiut_building <- function(direction = c("Y", "X")) {
  direction <- match.arg(direction)
  amplification <- c(Y = 26, X = 15)[[direction]]
  f0 <- 1.55
  list(
    nom = "Batiment de 6 etages, Tachkent (Ouzbekistan)",
    source = "Minowa, Kaniev et Rustamov (2023), E3S Web of Conferences 401, 05031",
    etages = 6,
    hauteur = 29.1,
    plan = c(72, 48),
    direction = direction,
    f0 = f0,
    omega0 = 2 * pi * f0,
    amplification = amplification,
    xi = 1 / (2 * amplification),
    acceleration_ambiante = 0.02
  )
}

#' Scenarios de validite du modele lineaire pour le batiment KIUT
#'
#' Pour plusieurs valeurs du rapport \eqn{|k_3|/k} (en m^-2), calcule
#' l'amplitude limite du modele lineaire ([linear_limit()]) et la compare au
#' deplacement ambiant mesure (acceleration / \eqn{\omega_0^2}) et au
#' deplacement de service \eqn{H/500}.
#'
#' Seul le rapport \eqn{k_3/k} intervient : la masse n'a pas besoin d'etre
#' connue. Le rapport \eqn{k_3/k} ne peut pas etre mesure sur un batiment
#' sain : on le traite comme un scenario.
#'
#' @param ratios Valeurs de \eqn{|k_3|/k} (m^-2).
#' @param tol Tolerance relative (par defaut `0.05`).
#' @param direction `"Y"` ou `"X"`.
#' @return Un data.frame avec les colonnes `rapport_k3_k`,
#'   `amplitude_limite_m`, `deplacement_ambiant_m`, `marge_ambiant`,
#'   `deplacement_service_m` et `lineaire_valide_service`.
#' @examples
#' kiut_scenarios()
#' @export
kiut_scenarios <- function(ratios = c(1, 10, 100), tol = 0.05,
                           direction = "Y") {
  bat <- kiut_building(direction)
  x_ambiant <- bat$acceleration_ambiante / bat$omega0^2
  x_service <- bat$hauteur / 500
  limite <- linear_limit(k = 1, k3 = -ratios, tol = tol)
  data.frame(
    rapport_k3_k = ratios,
    amplitude_limite_m = limite,
    deplacement_ambiant_m = x_ambiant,
    marge_ambiant = limite / x_ambiant,
    deplacement_service_m = x_service,
    lineaire_valide_service = x_service <= limite
  )
}

#' Simulation du batiment KIUT : modele lineaire et non lineaire
#'
#' Simule le batiment KIUT force a sa frequence propre. La force est choisie
#' pour que le deplacement lineaire en regime etabli atteigne le deplacement
#' de service \eqn{H/500}. La raideur vaut \eqn{k = m \omega_0^2} et
#' \eqn{k_3 = -ratio \cdot k}. La masse `mass` n'influence pas les
#' deplacements, car elle se simplifie dans l'equation du mouvement.
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
  bat <- kiut_building(direction)
  k <- mass * bat$omega0^2
  x_service <- bat$hauteur / 500
  x_statique <- 2 * bat$xi * x_service
  F0 <- k * x_statique
  compare_models(m = mass, k = k, k3 = -ratio * k, xi = bat$xi, F0 = F0,
                 Omega = bat$omega0, t_end = t_end) +
    ggplot2::labs(
      title = sprintf("Batiment KIUT (Tachkent) : scenario k3/k = -%g m^-2",
                      ratio)
    )
}
