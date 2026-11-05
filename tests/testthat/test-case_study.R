test_that("kiut_building contient les valeurs mesurees (direction Y)", {
  bat <- kiut_building("Y")
  expect_equal(bat$f0, 1.55)
  expect_equal(bat$xi, 1 / 52)
})

test_that("l'amortissement de la direction X vient de l'amplification 15", {
  expect_equal(kiut_building("X")$xi, 1 / 30)
})

test_that("amplitude limite selon le rapport k3/k", {
  expect_equal(kiut_scenarios()$amplitude_limite_m,
               sqrt(0.05 / c(1, 10, 100)))
})

test_that("validite du modele lineaire au deplacement de service H/500", {
  expect_equal(kiut_scenarios()$lineaire_valide_service,
               c(TRUE, TRUE, FALSE))
})

test_that("deplacement ambiant = acceleration / omega0^2", {
  res <- kiut_scenarios()
  expect_equal(res$deplacement_ambiant_m[1], 0.02 / (2 * pi * 1.55)^2)
})

test_that("kiut_compare reproduit le deplacement de service en lineaire", {
  p <- kiut_compare(ratio = 10, t_end = 60)
  expect_s3_class(p, "ggplot")
  lin <- p$data[p$data$modele == "lineaire", ]
  expect_equal(max(abs(lin$x[lin$t >= 50])), 29.1 / 500, tolerance = 0.01)
})
