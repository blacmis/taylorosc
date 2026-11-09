#' Estime la frequence propre d'un signal par son spectre
#'
#' Calcule le spectre du signal (periodogramme lisse, [stats::spec.pgram()]) et
#' retient la frequence du pic le plus haut dans l'intervalle `[fmin, fmax]`. La
#' position du pic est affinee par interpolation parabolique du logarithme de la
#' puissance entre le pic et ses deux voisins.
#'
#' @param signal Vecteur numerique (par exemple une vitesse mesuree).
#' @param fs Frequence d'echantillonnage (Hz).
#' @param fmin,fmax Intervalle de recherche du pic (Hz).
#' @param spans Largeurs (nombres impairs) du lissage du spectre.
#' @return Une liste avec `f0` (Hz), `freq` (Hz) et `power` (puissance spectrale).
#' @examples
#' t <- (1:12000) / 200
#' estimate_frequency(sin(2 * pi * 1.25 * t), fs = 200)$f0
#' @export
estimate_frequency <- function(signal, fs, fmin = 0.2, fmax = 10,
                               spans = c(5, 5)) {
  x <- as.numeric(signal)
  x <- x[is.finite(x)]
  if (length(x) < 100) stop("le signal est trop court (moins de 100 points).")
  if (fs <= 0) stop("`fs` doit etre strictement positive.")

  sp <- stats::spec.pgram(stats::ts(x, frequency = fs), spans = spans,
                          taper = 0.1, detrend = TRUE, plot = FALSE)
  freq <- sp$freq
  power <- sp$spec

  zone <- which(freq >= fmin & freq <= fmax)
  if (length(zone) < 3) stop("l'intervalle [fmin, fmax] est trop etroit.")
  i <- zone[which.max(power[zone])]
  f0 <- freq[i]

  # Interpolation parabolique autour du pic
  if (i > 1 && i < length(freq)) {
    p_avant <- log(power[i - 1])
    p_pic <- log(power[i])
    p_apres <- log(power[i + 1])
    denominateur <- p_avant - 2 * p_pic + p_apres
    if (is.finite(denominateur) && denominateur != 0) {
      f0 <- f0 + 0.5 * (p_avant - p_apres) / denominateur * (freq[2] - freq[1])
    }
  }
  list(f0 = f0, freq = freq, power = power)
}

#' Estime le taux d'amortissement d'une vibration libre
#'
#' Releve, a chaque periode, le plus grand pic du signal (apres lissage), puis
#' ajuste une droite sur \eqn{\ln(\mathrm{amplitude})} en fonction du temps. Les
#' pics suivent \eqn{A_0 e^{-\xi \omega_0 t}}, donc la pente vaut
#' \eqn{-\xi \omega_0} et \eqn{\xi = -\mathrm{pente} / (2 \pi f_0)}.
#'
#' @param t Temps (s).
#' @param v Signal (par exemple la vitesse en cm/s).
#' @param f0 Frequence propre (Hz).
#' @param t_start,t_end Debut et fin de la zone de vibration libre (s).
#' @param min_amplitude Les pics plus petits que cette valeur (bruit) sont ignores.
#' @param smooth Duree du lissage par moyenne glissante (s).
#' @return Une liste avec `xi`, `r2` (qualite de l'ajustement), `slope`,
#'   `intercept`, `peaks` (tableau `t` et `amplitude`), `t_start` et `t_end`.
#' @examples
#' t <- (1:12000) / 200
#' v <- exp(-0.02 * 2 * pi * t) * sin(2 * pi * t)
#' estimate_damping(t, v, f0 = 1, t_start = 2, t_end = 20)$xi
#' @export
estimate_damping <- function(t, v, f0, t_start, t_end = max(t),
                             min_amplitude = 0, smooth = 0.1) {
  if (f0 <= 0) stop("`f0` doit etre strictement positive.")
  keep <- is.finite(t) & is.finite(v)
  t <- t[keep]
  v <- v[keep] - mean(v[keep])

  fs <- 1 / stats::median(diff(t))
  k <- max(1, round(smooth * fs))
  if (k > 1) v <- as.numeric(stats::filter(v, rep(1 / k, k), sides = 2))

  periode <- 1 / f0
  n_cycles <- floor((t_end - t_start) / periode)
  if (n_cycles < 4) {
    stop("pas assez de periodes entre `t_start` et `t_end` (il en faut au moins 4).")
  }

  amplitude <- rep(NA_real_, n_cycles)
  t_pic <- rep(NA_real_, n_cycles)
  for (j in seq_len(n_cycles)) {
    debut <- t_start + (j - 1) * periode
    idx <- which(t >= debut & t < debut + periode & !is.na(v))
    if (length(idx) == 0) next
    i_max <- idx[which.max(v[idx])]
    amplitude[j] <- v[i_max]
    t_pic[j] <- t[i_max]
  }

  garde <- !is.na(amplitude) & amplitude > min_amplitude
  if (sum(garde) < 4) stop("trop peu de pics au-dessus de `min_amplitude`.")

  ajustement <- stats::lm(log(amplitude[garde]) ~ t_pic[garde])
  ordonnee <- unname(stats::coef(ajustement)[1])
  pente <- unname(stats::coef(ajustement)[2])

  list(
    xi = -pente / (2 * pi * f0),
    r2 = summary(ajustement)$r.squared,
    slope = pente,
    intercept = ordonnee,
    peaks = data.frame(t = t_pic[garde], amplitude = amplitude[garde]),
    t_start = t_start,
    t_end = t_end
  )
}
