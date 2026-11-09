test_that("estimate_frequency retrouve la frequence d'un sinus bruite", {
  set.seed(1)
  t <- (1:12000) / 200
  x <- sin(2 * pi * 1.25 * t) + 0.3 * rnorm(length(t))
  res <- estimate_frequency(x, fs = 200)
  expect_equal(res$f0, 1.25, tolerance = 0.01)
})

test_that("estimate_frequency respecte l'intervalle de recherche", {
  t <- (1:12000) / 200
  x <- sin(2 * pi * 1 * t) + 3 * sin(2 * pi * 4 * t)
  res <- estimate_frequency(x, fs = 200, fmin = 0.2, fmax = 2)
  expect_equal(res$f0, 1, tolerance = 0.02)
})

test_that("estimate_frequency refuse un signal trop court", {
  expect_error(estimate_frequency(1:50, fs = 200))
})

test_that("estimate_damping retrouve l'amortissement d'une vibration libre", {
  set.seed(2)
  t <- (1:12000) / 200
  xi <- 0.02
  w <- 2 * pi
  v <- exp(-xi * w * t) * sin(w * sqrt(1 - xi^2) * t) + 0.005 * rnorm(length(t))
  res <- estimate_damping(t, v, f0 = 1, t_start = 2, t_end = 30,
                          min_amplitude = 0.05)
  expect_equal(res$xi, 0.02, tolerance = 0.05)
  expect_gt(res$r2, 0.95)
})

test_that("estimate_damping refuse une fenetre trop courte", {
  t <- (1:12000) / 200
  v <- sin(2 * pi * t)
  expect_error(estimate_damping(t, v, f0 = 1, t_start = 0, t_end = 2))
})
