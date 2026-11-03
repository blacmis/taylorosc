test_that("au repos, le batiment reste immobile", {
  res <- simulate_oscillator(m = 2e6, k = 8e7, t_end = 10)
  expect_equal(max(abs(res$x)), 0)
})

test_that("oscillation libre non amortie : le batiment passe de l'autre cote", {
  T0 <- natural_frequency(k = 8e7, m = 2e6)$T0
  res <- simulate_oscillator(m = 2e6, k = 8e7, xi = 0, x0 = 0.01,
                             t_end = 5 * T0)
  expect_equal(min(res$x), -0.01, tolerance = 0.01)
})

test_that("a la resonance, l'amplitude vaut F0 / (2 xi k)", {
  w0 <- natural_frequency(k = 8e7, m = 2e6)$omega0
  res <- simulate_oscillator(m = 2e6, k = 8e7, xi = 0.02, F0 = 4e5,
                             Omega = w0, t_end = 100)
  amplitude <- max(abs(res$x[res$t >= 80]))
  expect_equal(amplitude, 4e5 / (2 * 0.02 * 8e7), tolerance = 0.01)
})

test_that("hors resonance (r = 0.5), l'amplitude suit la theorie", {
  w0 <- natural_frequency(k = 8e7, m = 2e6)$omega0
  r <- 0.5
  res <- simulate_oscillator(m = 2e6, k = 8e7, xi = 0.02, F0 = 4e5,
                             Omega = r * w0, t_end = 200)
  amplitude <- max(abs(res$x[res$t >= 150]))
  theorie <- (4e5 / 8e7) / sqrt((1 - r^2)^2 + (2 * 0.02 * r)^2)
  expect_equal(amplitude, theorie, tolerance = 0.01)
})

test_that("une vitesse initiale donne un deplacement maximal v0 / omega0", {
  nf <- natural_frequency(k = 8e7, m = 2e6)
  res <- simulate_oscillator(m = 2e6, k = 8e7, xi = 0, v0 = 0.1,
                             t_end = nf$T0)
  expect_equal(max(res$x), 0.1 / nf$omega0, tolerance = 0.01)
})

test_that("les conditions initiales sont respectees", {
  res <- simulate_oscillator(m = 2e6, k = 8e7, x0 = 0.02, v0 = 0.3, t_end = 5)
  expect_equal(res$x[1], 0.02)
  expect_equal(res$v[1], 0.3)
})

test_that("un pas de temps trop grand est refuse", {
  expect_error(simulate_oscillator(m = 2e6, k = 8e7, t_end = 10, dt = 0.5))
})

test_that("le resultat a les bonnes colonnes", {
  res <- simulate_oscillator(m = 2e6, k = 8e7, t_end = 5)
  expect_named(res, c("t", "x", "v"))
})
