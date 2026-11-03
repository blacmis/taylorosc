# Noms de colonnes utilises dans les graphiques
utils::globalVariables(c("x", "y", "t", "serie", "valeur", "modele"))

# Palette de couleurs bien distinctes
.palette <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
              "#e6ab02", "#a6761d", "#1f78b4")

# Theme commun a tous les graphiques du package
.theme_taylor <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      plot.caption = ggplot2::element_text(colour = "grey40", hjust = 0),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Courbes de Taylor de plusieurs ordres
#'
#' Trace la fonction `f` (en noir) et ses polynomes de Taylor de differents
#' ordres autour du point `a`.
#'
#' @inheritParams taylor_coef
#' @param orders Vecteur des ordres a tracer (par defaut `c(1, 3, 5, 7)`).
#' @param xlim Intervalle de trace (par defaut `a` plus ou moins 2 pi).
#' @return Un objet `ggplot`.
#' @examples
#' plot_taylor("sin(x)")
#' plot_taylor("exp(x)", xlim = c(-3, 3), orders = c(1, 2, 3, 5))
#' @export
plot_taylor <- function(f, a = 0, orders = c(1, 3, 5, 7),
                        xlim = c(a - 2 * pi, a + 2 * pi), var = "x") {
  orders <- unique(orders)
  expr <- .as_call(f)
  grid <- seq(xlim[1], xlim[2], length.out = 400)
  courbes <- data.frame(x = grid, y = .eval_at(expr, grid, var),
                        serie = "f(x)")
  yr <- range(courbes$y)
  pad <- diff(yr) / 2
  for (n in orders) {
    poly_n <- data.frame(x = grid,
                         y = taylor_poly(expr, grid, a, n, var),
                         serie = paste("ordre", n))
    courbes <- rbind(courbes, poly_n)
  }
  courbes$serie <- factor(courbes$serie, levels = unique(courbes$serie))

  # Couleurs liees aux NOMS des courbes (f(x) en noir)
  niveaux <- levels(courbes$serie)
  couleurs <- c("black", rep_len(.palette, length(orders)))
  names(couleurs) <- niveaux

  fa <- .eval_at(expr, a, var)
  y_bas <- yr[1] - pad
  nom_f <- paste(deparse(expr), collapse = "")

  ggplot2::ggplot(courbes, ggplot2::aes(x = x, y = y, colour = serie)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey75") +
    ggplot2::geom_vline(xintercept = a, linetype = "dotted",
                        colour = "grey40") +
    ggplot2::geom_line(data = courbes[courbes$serie != "f(x)", ],
                       linewidth = 0.9) +
    ggplot2::geom_line(data = courbes[courbes$serie == "f(x)", ],
                       linewidth = 1.7) +
    ggplot2::annotate("point", x = a, y = fa, size = 3.5, colour = "black") +
    ggplot2::annotate("text", x = a, y = y_bas + pad * 0.2,
                      label = paste0("a = ", a), hjust = -0.15,
                      colour = "grey30", size = 4.5) +
    ggplot2::scale_colour_manual(values = couleurs, breaks = niveaux) +
    ggplot2::coord_cartesian(ylim = c(yr[1] - pad, yr[2] + pad)) +
    ggplot2::labs(
      title = paste0("Polynomes de Taylor de f(", var, ") = ", nom_f),
      subtitle = paste0("Developpement autour de a = ", a,
                        " ; ordres traces : ", paste(orders, collapse = ", ")),
      caption = "Plus l'ordre est eleve, plus le polynome reste proche de f(x) loin de a.",
      x = var, y = "valeur", colour = NULL
    ) +
    .theme_taylor()
}

#' Erreur de Taylor contre majorant de Lagrange
#'
#' Trace, pour un ordre `n`, l'erreur reelle \eqn{|f(x) - P_n(x)|} et son
#' majorant de Lagrange, pour \eqn{x} entre `a` et `a + h_max`.
#'
#' @inheritParams taylor_coef
#' @param h_max Distance maximale a droite de `a` (par defaut `1`).
#' @return Un objet `ggplot` (axe vertical en echelle logarithmique).
#' @examples
#' plot_taylor_error("sin(x)", n = 3, h_max = 1)
#' @export
plot_taylor_error <- function(f, a = 0, n = 3, h_max = 1, var = "x") {
  expr <- .as_call(f)
  x <- seq(a + h_max / 100, a + h_max, length.out = 100)
  res <- taylor_error(expr, x, a = a, n = n, var = var)
  courbes <- rbind(
    data.frame(x = res$x, valeur = res$error, serie = "erreur reelle"),
    data.frame(x = res$x, valeur = res$bound, serie = "majorant de Lagrange")
  )
  courbes <- courbes[courbes$valeur > 0, ]
  nom_f <- paste(deparse(expr), collapse = "")

  ggplot2::ggplot(courbes, ggplot2::aes(x = x, y = valeur, colour = serie,
                                        linetype = serie)) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::scale_colour_manual(values = c("erreur reelle" = "#d95f02",
                                            "majorant de Lagrange" = "#1b9e77")) +
    ggplot2::scale_linetype_manual(values = c("erreur reelle" = "solid",
                                              "majorant de Lagrange" = "dashed")) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = paste0("Erreur du polynome de Taylor d'ordre ", n),
      subtitle = paste0("f(", var, ") = ", nom_f, " ; a = ", a,
                        " ; l'erreur reelle reste sous le majorant de Lagrange"),
      caption = "Majorant : M / (n+1)! * |x - a|^(n+1), avec M = maximum de |f^(n+1)| entre a et x.",
      x = var, y = "erreur absolue (echelle logarithmique)",
      colour = NULL, linetype = NULL
    ) +
    .theme_taylor()
}

#' Comparaison du modele lineaire et du modele non lineaire
#'
#' Simule le batiment avec `k3 = 0` (modele lineaire) puis avec la valeur
#' `k3` donnee, et trace les deux deplacements sur un meme graphique. Si
#' `k3` n'est pas nul, une zone verte marque l'amplitude limite donnee par
#' [linear_limit()].
#'
#' @inheritParams simulate_oscillator
#' @return Un objet `ggplot`.
#' @examples
#' compare_models(m = 2e6, k = 8e7, k3 = -1.6e8, F0 = 4e5,
#'                Omega = sqrt(8e7 / 2e6), t_end = 60)
#' @export
compare_models <- function(m, k, k3, xi = 0.02, F0 = 0, Omega = 0,
                           x0 = 0, v0 = 0, t_end, dt = NULL) {
  lin <- simulate_oscillator(m, k, k3 = 0, xi = xi, F0 = F0, Omega = Omega,
                             x0 = x0, v0 = v0, t_end = t_end, dt = dt)
  nonlin <- simulate_oscillator(m, k, k3 = k3, xi = xi, F0 = F0,
                                Omega = Omega, x0 = x0, v0 = v0,
                                t_end = t_end, dt = dt)
  courbes <- rbind(
    data.frame(t = lin$t, x = lin$x, modele = "lineaire"),
    data.frame(t = nonlin$t, x = nonlin$x, modele = "non lineaire")
  )
  amp_lin <- max(abs(lin$x))
  amp_nl <- max(abs(nonlin$x))
  ecart <- if (amp_lin > 0) 100 * (amp_nl - amp_lin) / amp_lin else 0
  sous_titre <- sprintf(
    "Amplitude max : lineaire = %.3f m | non lineaire = %.3f m (ecart : %+.1f %%)",
    amp_lin, amp_nl, ecart
  )
  parametres <- sprintf(
    "m = %.3g kg ; k = %.3g N/m ; k3 = %.3g N/m^3 ; xi = %.3g",
    m, k, k3, xi
  )

  graphique <- ggplot2::ggplot(courbes,
                               ggplot2::aes(x = t, y = x, colour = modele))
  if (k3 != 0) {
    limite <- linear_limit(k, k3)
    graphique <- graphique +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = -limite, ymax = limite,
                        fill = "#1b9e77", alpha = 0.08) +
      ggplot2::geom_hline(yintercept = c(-limite, limite),
                          linetype = "dashed", colour = "#1b9e77") +
      ggplot2::annotate("text", x = max(courbes$t), y = limite,
                        label = sprintf("limite du modele lineaire : +/- %.3f m",
                                        limite),
                        hjust = 1, vjust = -0.5, colour = "#1b9e77",
                        size = 4.5)
  }
  graphique +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = c("lineaire" = "#1f78b4",
                                            "non lineaire" = "#d95f02")) +
    ggplot2::labs(
      title = "Deplacement du batiment : modele lineaire et non lineaire",
      subtitle = sous_titre, caption = parametres,
      x = "temps (s)", y = "deplacement (m)", colour = NULL
    ) +
    .theme_taylor()
}

#' Animation : le polynome de Taylor s'ameliore quand l'ordre augmente
#'
#' Affiche successivement, dans la fenetre des graphiques, le polynome de
#' Taylor d'ordre 1, 2, ..., `n_max` avec la fonction `f`.
#'
#' @inheritParams plot_taylor
#' @param n_max Ordre maximal (par defaut `10`).
#' @param delay Pause entre deux images, en secondes (par defaut `1.5`).
#' @param wait_key Si `TRUE`, attend que l'on appuie sur Entree avant chaque
#'   image au lieu d'utiliser `delay` (pratique pour une presentation).
#' @return `NULL`, de facon invisible.
#' @examples
#' \dontrun{
#' animate_taylor("sin(x)", n_max = 10)
#' animate_taylor("sin(x)", n_max = 10, wait_key = TRUE)
#' }
#' @export
animate_taylor <- function(f, a = 0, n_max = 10, delay = 1.5,
                           wait_key = FALSE,
                           xlim = c(a - 2 * pi, a + 2 * pi), var = "x") {
  for (n in 1:n_max) {
    graphique <- plot_taylor(f, a = a, orders = n, xlim = xlim, var = var) +
      ggplot2::labs(subtitle = paste0("Ordre n = ", n, " sur ", n_max,
                                      " ; developpement autour de a = ", a))
    print(graphique)
    if (wait_key) {
      readline(prompt = "Appuie sur Entree pour l'ordre suivant...")
    } else {
      Sys.sleep(delay)
    }
  }
  invisible(NULL)
}
