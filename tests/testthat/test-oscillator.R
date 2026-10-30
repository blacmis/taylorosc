test_that("restoring_force calcule k*x + k3*x^3", {
  expect_equal(restoring_force(0.1, k = 8e7, k3 = -1.6e8), 7.84e6)
})

test_that("restoring_force est lineaire par defaut", {
  expect_equal(restoring_force(2, k = 5), 10)
})

test_that("natural_frequency donne omega0 et T0", {
  res <- natural_frequency(k = 8e7, m = 2e6)
  expect_equal(res$omega0, sqrt(40))
  expect_equal(res$T0, 2 * pi / sqrt(40))
})

test_that("natural_frequency refuse les valeurs non positives", {
  expect_error(natural_frequency(k = -1, m = 2))
  expect_error(natural_frequency(k = 1, m = 0))
})

test_that("linear_limit donne l'amplitude attendue", {
  expect_equal(linear_limit(k = 8e7, k3 = -1.6e8, tol = 0.05), sqrt(0.025))
})

test_that("linear_limit vaut Inf si k3 est nul", {
  expect_equal(linear_limit(k = 8e7, k3 = 0), Inf)
})

test_that("a l'amplitude limite, la correction vaut tol", {
  x <- linear_limit(k = 8e7, k3 = -1.6e8, tol = 0.05)
  expect_equal(abs(-1.6e8) * x^2 / 8e7, 0.05)
})

test_that("restoring_force_taylor retrouve k et k3", {
  res <- restoring_force_taylor("8e7 * atan(x)")
  expect_equal(res$k, 8e7)
  expect_equal(res$k3, -8e7 / 3)
})
