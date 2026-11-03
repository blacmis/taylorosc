#' Simulation du deplacement d'un batiment (oscillateur force)
#'
#' Resout \eqn{m x'' + c x' + g(x) = F(t)} par la methode des differences
#' centrees, obtenue avec des developpements de Taylor. La force de rappel est
#' \eqn{g(x) = k x + k_3 x^3} et la force exterieure est
#' \eqn{F(t) = F_0 \sin(\Omega t)}. L'amortissement vaut
#' \eqn{c = 2 \xi m \omega_0}.
#'
#' @param m Masse (kg), strictement positive.
#' @param k Raideur lineaire (N/m), strictement positive.
#' @param k3 Coefficient cubique (N/m^3). Par defaut `0` (modele lineaire).
#' @param xi Taux d'amortissement (sans unite). Par defaut `0.02`.
#' @param F0 Amplitude de la force exterieure (N). Par defaut `0`.
#' @param Omega Pulsation de la force exterieure (rad/s). Par defaut `0`.
#' @param x0 Deplacement initial (m).
#' @param v0 Vitesse initiale (m/s).
#' @param t_end Duree de la simulation (s).
#' @param dt Pas de temps (s). Si `NULL`, on prend T0 / 50.
#' @return Un data.frame avec les colonnes `t` (s), `x` (m) et `v` (m/s).
#' @examples
#' res <- simulate_oscillator(m = 2e6, k = 8e7, xi = 0.02, F0 = 4e5,
#'                            Omega = sqrt(8e7 / 2e6), t_end = 60)
#' head(res)
#' @export
simulate_oscillator <- function(m, k, k3 = 0, xi = 0.02, F0 = 0, Omega = 0,
                                x0 = 0, v0 = 0, t_end, dt = NULL) {
  if (m <= 0 || k <= 0) stop("`m` et `k` doivent etre strictement positifs.")
  if (xi < 0) stop("`xi` doit etre positif ou nul.")
  if (t_end <= 0) stop("`t_end` doit etre strictement positif.")

  omega0 <- sqrt(k / m)
  if (is.null(dt)) dt <- (2 * pi / omega0) / 50
  if (dt <= 0) stop("`dt` doit etre strictement positif.")
  if (omega0 * dt >= 2) {
    stop("pas de temps trop grand : il faut omega0 * dt < 2 (methode instable).")
  }

  c_damp <- 2 * xi * m * omega0
  n_steps <- round(t_end / dt)
  if (n_steps < 2) stop("`t_end` doit valoir au moins 2 pas de temps.")

  time <- (0:n_steps) * dt
  F_t <- F0 * sin(Omega * time)
  x <- numeric(n_steps + 1)
  v <- numeric(n_steps + 1)
  x[1] <- x0

  # Pas fictif x_{-1}, obtenu par Taylor d'ordre 2
  a0 <- (F_t[1] - c_damp * v0 - restoring_force(x0, k, k3)) / m
  x_prev <- x0 - dt * v0 + dt^2 / 2 * a0

  denom <- m + c_damp * dt / 2
  coef_prev <- m - c_damp * dt / 2

  for (i in 1:n_steps) {
    x_now <- x[i]
    x[i + 1] <- (dt^2 * (F_t[i] - restoring_force(x_now, k, k3)) +
                   2 * m * x_now - coef_prev * x_prev) / denom
    v[i] <- (x[i + 1] - x_prev) / (2 * dt)
    x_prev <- x_now
  }
  v[1] <- v0
  v[n_steps + 1] <- (x[n_steps + 1] - x[n_steps]) / dt

  data.frame(t = time, x = x, v = v)
}
