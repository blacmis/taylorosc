test_that("coefficients de exp(x) en 0 valent 1/k!", {
  expect_equal(taylor_coef("exp(x)", a = 0, n = 4), 1 / factorial(0:4))
})

test_that("coefficients de sin(x) en 0", {
  expect_equal(taylor_coef("sin(x)", a = 0, n = 5),
               c(0, 1, 0, -1/6, 0, 1/120))
})

test_that("un polynome est reproduit exactement", {
  expect_equal(taylor_poly("x^2", x = 3, a = 1, n = 2), 9)
})

test_that("taylor_poly approche sin(x) en 0.5", {
  expect_equal(taylor_poly("sin(x)", x = 0.5, a = 0, n = 5),
               sin(0.5), tolerance = 1e-5)
})

test_that("l'erreur reelle est inferieure au majorant de Lagrange", {
  res <- taylor_error("sin(x)", x = c(0.1, 0.5, 1), a = 0, n = 3)
  expect_true(all(res$error <= res$bound + 1e-12))
})
