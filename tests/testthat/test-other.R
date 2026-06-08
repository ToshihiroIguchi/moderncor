test_that("Other correlations compute correctly", {
  skip_if_not_installed("Ball")
  skip_if_not_installed("TauStar")
  
  set.seed(42)
  x <- rnorm(25)
  y <- rnorm(25)
  
  # Ball correlation
  res_ball <- moderncor(x, y, method = "ball", p_value = TRUE, num.permutations = 20)
  expect_s3_class(res_ball, "moderncor")
  expect_equal(res_ball$estimate, as.numeric(Ball::bcor(x, y)))
  expect_true(res_ball$p.value >= 0 && res_ball$p.value <= 1)
  
  # TauStar correlation
  res_taustar <- moderncor(x, y, method = "tau_star", p_value = TRUE)
  expect_s3_class(res_taustar, "moderncor")
  expect_equal(res_taustar$estimate, TauStar::tauStarTest(x, y)$tStar)
  expect_equal(res_taustar$p.value, as.numeric(TauStar::tauStarTest(x, y)$pVal))
})
