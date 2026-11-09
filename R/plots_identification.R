utils::globalVariables(c("freq", "puissance", "v", "amplitude", "enveloppe"))

#' Spectre d'un signal et frequence propre estimee
#'
#' @param estimate Le resultat de [estimate_frequency()].
#' @param fmax Frequence maximale affichee (Hz).
#' @param title Titre du graphique.
#' @return Un objet `ggplot` (axe vertical en echelle logarithmique).
#' @examples
#' t <- (1:12000) / 200
#' plot_spectrum(estimate_frequency(sin(2 * pi * 1.25 * t), fs = 200))
#' @export
plot_spectrum <- function(estimate, fmax = 3, title = "Spectre du signal") {
  spectre <- data.frame(freq = estimate$freq, puissance = estimate$power)
  spectre <- spectre[spectre$freq <= fmax, ]
  ggplot2::ggplot(spectre, ggplot2::aes(x = freq, y = puissance)) +
    ggplot2::geom_line(colour = "#0B4A82", linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = estimate$f0, linetype = "dashed",
                        colour = "#D95F02", linewidth = 0.9) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Pic a f0 = %.3f Hz (periode T0 = %.2f s)",
                         estimate$f0, 1 / estimate$f0),
      x = "frequence (Hz)", y = "puissance spectrale (echelle log)"
    ) +
    .theme_taylor()
}

#' Vibration libre et decroissance exponentielle ajustee
#'
#' Trace le signal, les pics retenus et la courbe de decroissance ajustee
#' \eqn{A_0 e^{-\xi \omega_0 t}} du resultat de [estimate_damping()].
#'
#' @param t Temps (s).
#' @param v Signal (par exemple la vitesse en cm/s).
#' @param damping Le resultat de [estimate_damping()].
#' @param title Titre du graphique.
#' @return Un objet `ggplot`.
#' @export
plot_free_decay <- function(t, v, damping, title = "Vibration libre du batiment") {
  signal <- data.frame(t = t, v = v - mean(v))
  env <- data.frame(t = seq(damping$t_start, damping$t_end, length.out = 200))
  env$enveloppe <- exp(damping$intercept + damping$slope * env$t)

  ggplot2::ggplot() +
    ggplot2::geom_line(data = signal, ggplot2::aes(x = t, y = v),
                       colour = "#1F78B4", linewidth = 0.5) +
    ggplot2::geom_point(data = damping$peaks,
                        ggplot2::aes(x = t, y = amplitude),
                        colour = "#D95F02", size = 2) +
    ggplot2::geom_line(data = env, ggplot2::aes(x = t, y = enveloppe),
                       colour = "#1B9E77", linetype = "dashed", linewidth = 1.1) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Amortissement estime : xi = %.2f %% (R2 = %.2f)",
                         100 * damping$xi, damping$r2),
      x = "temps (s)", y = "vitesse (cm/s)"
    ) +
    .theme_taylor()
}
