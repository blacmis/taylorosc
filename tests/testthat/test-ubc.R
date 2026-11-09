bat <- ubc_building()

test_that("ubc_signal lit les trois essais", {
  expect_equal(nrow(ubc_signal("1_Y")), 60000)
  expect_equal(nrow(ubc_signal("1_X")), 60000)
  expect_equal(nrow(ubc_signal("3_Y_free")), 12000)
  expect_named(ubc_signal("1_Y"), c("t", "v"))
})

test_that("l'essai de vibration libre dure 60 secondes", {
  expect_equal(max(ubc_signal("3_Y_free")$t), 60)
})

test_that("la frequence propre estimee est voisine de 0,95 Hz", {
  expect_gt(bat$f0, 0.93)
  expect_lt(bat$f0, 0.96)
})

test_that("l'amortissement estime est de l'ordre de 1 %", {
  expect_gt(bat$xi, 0.006)
  expect_lt(bat$xi, 0.011)
})

test_that("les deplacements mesures sont tres petits", {
  expect_lt(bat$deplacement_essai, 0.001)
  expect_gt(bat$deplacement_essai, bat$deplacement_ambiant)
})

test_that("case_scenarios fonctionne sur le batiment UBC", {
  res <- case_scenarios(bat)
  expect_equal(res$amplitude_limite_m, sqrt(0.05 / c(1, 10, 100)))
  expect_equal(res$lineaire_valide_service, c(TRUE, FALSE, FALSE))
})

test_that("case_compare reproduit le deplacement de service en lineaire", {
  p <- case_compare(bat, ratio = 10)
  expect_s3_class(p, "ggplot")
  lin <- p$data[p$data$modele == "lineaire", ]
  expect_equal(max(abs(lin$x[lin$t >= 0.8 * max(lin$t)])), 53 / 500,
               tolerance = 0.02)
})
