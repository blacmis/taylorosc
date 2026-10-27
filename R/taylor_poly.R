#' Convertit une fonction en appel R evaluable
#'
#' @param f Chaine de caracteres, expression ou appel.
#' @return Un appel R.
#' @noRd
.as_call <- function(f) {
  if (is.character(f)) return(parse(text = f)[[1]])
  if (is.expression(f)) return(f[[1]])
  if (is.call(f) || is.name(f) || is.numeric(f)) return(f)
  stop("`f` doit etre une chaine de caracteres, une expression ou un appel.")
}

#' Evalue un appel R en un ou plusieurs points
#'
#' @param expr Appel R.
#' @param x Vecteur numerique.
#' @param var Nom de la variable.
#' @return Vecteur numerique de meme longueur que `x`.
#' @noRd
.eval_at <- function(expr, x, var = "x") {
  value <- eval(expr, envir = stats::setNames(list(x), var))
  rep_len(value, length(x))
}

#' Coefficients du polynome de Taylor
#'
#' Calcule les coefficients \eqn{c_k = f^{(k)}(a) / k!} pour
#' \eqn{k = 0, \dots, n}, avec les derivees symboliques de [stats::D()].
#'
#' @param f Fonction donnee sous forme de chaine (`"sin(x)"`), d'expression
#'   ou d'appel.
#' @param a Point de developpement.
#' @param n Ordre du developpement (entier positif ou nul).
#' @param var Nom de la variable (par defaut `"x"`).
#' @return Vecteur numerique de longueur `n + 1` : `c_0, c_1, ..., c_n`.
#' @examples
#' taylor_coef("exp(x)", a = 0, n = 4)
#' taylor_coef("sin(x)", a = 0, n = 5)
#' @export
taylor_coef <- function(f, a = 0, n = 3, var = "x") {
  expr <- .as_call(f)
  coef <- numeric(n + 1)
  deriv_k <- expr
  for (k in 0:n) {
    if (k > 0) deriv_k <- stats::D(deriv_k, var)
    value <- eval(deriv_k, envir = stats::setNames(list(a), var))
    coef[k + 1] <- value / factorial(k)
  }
  coef
}

#' Polynome de Taylor evalue en un ou plusieurs points
#'
#' Calcule \eqn{P_n(x) = \sum_{k=0}^{n} c_k (x - a)^k}.
#'
#' @inheritParams taylor_coef
#' @param x Vecteur numerique des points d'evaluation.
#' @return Vecteur numerique des valeurs de \eqn{P_n(x)}.
#' @examples
#' taylor_poly("sin(x)", x = 0.5, a = 0, n = 5)
#' sin(0.5)
#' @export
taylor_poly <- function(f, x, a = 0, n = 3, var = "x") {
  coef <- taylor_coef(f, a = a, n = n, var = var)
  vapply(x - a, function(h) sum(coef * h^(0:n)), numeric(1))
}

#' Erreur d'un polynome de Taylor et majorant de Lagrange
#'
#' Compare l'erreur reelle \eqn{|f(x) - P_n(x)|} au majorant
#' \eqn{M/(n+1)! \cdot |x-a|^{n+1}}, ou \eqn{M} est le maximum de
#' \eqn{|f^{(n+1)}|} entre `a` et `x` (estime sur une grille de 1000 points).
#'
#' @inheritParams taylor_poly
#' @return Un data.frame avec les colonnes `x`, `exact`, `approx`, `error`
#'   et `bound`.
#' @examples
#' taylor_error("sin(x)", x = c(0.1, 0.5, 1), a = 0, n = 3)
#' @export
taylor_error <- function(f, x, a = 0, n = 3, var = "x") {
  expr <- .as_call(f)
  exact <- .eval_at(expr, x, var)
  approx <- taylor_poly(expr, x, a, n, var)
  d <- expr
  for (k in seq_len(n + 1)) d <- stats::D(d, var)
  bound <- vapply(x, function(xx) {
    grid <- seq(min(a, xx), max(a, xx), length.out = 1000)
    M <- max(abs(.eval_at(d, grid, var)))
    M / factorial(n + 1) * abs(xx - a)^(n + 1)
  }, numeric(1))
  data.frame(x = x, exact = exact, approx = approx,
             error = abs(exact - approx), bound = bound)
}
