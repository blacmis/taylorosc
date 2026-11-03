test_that("plot_taylor renvoie un graphique avec f et les ordres demandes", {
  p <- plot_taylor("sin(x)")
  expect_s3_class(p, "ggplot")
  expect_equal(nlevels(p$data$serie), 5)
})

test_that("plot_taylor_error trace l'erreur et le majorant", {
  p <- plot_taylor_error("sin(x)", n = 3, h_max = 1)
  expect_s3_class(p, "ggplot")
  expect_equal(length(unique(p$data$serie)), 2)
})

test_that("compare_models trace les deux modeles", {
  p <- compare_models(m = 2e6, k = 8e7, k3 = -1.6e8, F0 = 4e5,
                      Omega = sqrt(8e7 / 2e6), t_end = 20)
  expect_s3_class(p, "ggplot")
  expect_equal(length(unique(p$data$modele)), 2)
})
