#' Force de rappel non lineaire
#'
#' Calcule \eqn{g(x) = k x + k_3 x^3}, le polynome de Taylor d'ordre 3 de la
#' force de rappel d'une structure (fonction impaire).
#'
#' @param x Deplacement (m), vecteur numerique.
#' @param k Raideur lineaire (N/m).
#' @param k3 Coefficient cubique (N/m^3). Par defaut `0` (modele lineaire).
#' @return Vecteur numerique de la force de rappel (N).
#' @examples
#' restoring_force(x = 0.1, k = 8e7, k3 = -1.6e8)
#' @export
restoring_force <- function(x, k, k3 = 0) {
  k * x + k3 * x^3
}

#' Raideur et coefficient cubique a partir d'une force de rappel
#'
#' Developpe une force de rappel \eqn{g(x)} en serie de Taylor en 0 et en
#' extrait \eqn{k = g'(0)} et \eqn{k_3 = g'''(0)/3!}. Une structure reagit de
#' la meme facon a gauche et a droite : \eqn{g(0) = 0} et \eqn{g''(0) = 0}.
#' Un avertissement est emis si ce n'est pas le cas.
#'
#' @param g Force de rappel donnee sous forme de chaine (par exemple
#'   `"8e7 * atan(x)"`), d'expression ou d'appel.
#' @param var Nom de la variable (par defaut `"x"`).
#' @return Une liste avec `k`, `k3`, `c0` (valeur en 0) et `c2` (coefficient
#'   d'ordre 2, attendu nul).
#' @examples
#' restoring_force_taylor("8e7 * atan(x)")
#' @export
restoring_force_taylor <- function(g, var = "x") {
  coef <- taylor_coef(g, a = 0, n = 3, var = var)
  scale <- max(abs(coef[2]), 1)
  if (abs(coef[1]) > 1e-8 * scale || abs(coef[3]) > 1e-8 * scale) {
    warning("g n'est pas impaire ou ne s'annule pas en 0 : ",
            "le modele k*x + k3*x^3 est incomplet.")
  }
  list(k = coef[2], k3 = coef[4], c0 = coef[1], c2 = coef[3])
}

#' Frequence propre d'un batiment (modele lineaire)
#'
#' Calcule \eqn{\omega_0 = \sqrt{k/m}}, la frequence \eqn{f_0 = \omega_0 /
#' (2\pi)} et la periode \eqn{T_0 = 2\pi/\omega_0}.
#'
#' @param k Raideur (N/m), strictement positive.
#' @param m Masse (kg), strictement positive.
#' @return Une liste avec `omega0` (rad/s), `f0` (Hz) et `T0` (s).
#' @examples
#' natural_frequency(k = 8e7, m = 2e6)
#' @export
natural_frequency <- function(k, m) {
  if (any(k <= 0) || any(m <= 0)) {
    stop("`k` et `m` doivent etre strictement positifs.")
  }
  omega0 <- sqrt(k / m)
  list(omega0 = omega0, f0 = omega0 / (2 * pi), T0 = 2 * pi / omega0)
}

#' Amplitude limite du modele lineaire
#'
#' Donne l'amplitude \eqn{x} pour laquelle le terme cubique represente une
#' fraction `tol` du terme lineaire : \eqn{|k_3| x^2 / k = tol}, soit
#' \eqn{x = \sqrt{tol \cdot k / |k_3|}}. En dessous de cette amplitude, le
#' modele lineaire est valable a la tolerance `tol` pres.
#'
#' @param k Raideur lineaire (N/m), strictement positive.
#' @param k3 Coefficient cubique (N/m^3). Si `k3 = 0`, le resultat est `Inf`.
#' @param tol Tolerance relative (par defaut `0.05`, soit 5 %).
#' @return Amplitude limite (m).
#' @examples
#' linear_limit(k = 8e7, k3 = -1.6e8, tol = 0.05)
#' @export
linear_limit <- function(k, k3, tol = 0.05) {
  if (any(k <= 0)) stop("`k` doit etre strictement positif.")
  if (any(tol <= 0)) stop("`tol` doit etre strictement positive.")
  sqrt(tol * k / abs(k3))
}
