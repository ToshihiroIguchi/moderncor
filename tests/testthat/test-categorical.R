test_that("Categorical associations compute correctly", {
  skip_if_not_installed("DescTools")
  
  set.seed(42)
  x <- factor(sample(c("A", "B", "C"), 40, replace = TRUE))
  y <- factor(sample(c("X", "Y"), 40, replace = TRUE))
  
  # Cramer's V
  res_cv <- moderncor_cat(x, y, method = "cramers_v")
  expect_s3_class(res_cv, "moderncor_cat")
  expect_equal(res_cv$estimate, DescTools::CramerV(x, y))
  expect_true(res_cv$p.value >= 0 && res_cv$p.value <= 1)
  
  # Phi
  res_phi <- moderncor_cat(x, y, method = "phi")
  expect_equal(res_phi$estimate, DescTools::Phi(x, y))
  
  # Gamma
  res_gamma <- moderncor_cat(x, y, method = "gamma")
  expect_equal(res_gamma$estimate, DescTools::GoodmanKruskalGamma(x, y))
  expect_null(res_gamma$p.value)
  
  # Somers' D
  res_somers <- moderncor_cat(x, y, method = "somers_d")
  expect_equal(res_somers$estimate, DescTools::SomersDelta(x, y))
  expect_null(res_somers$p.value)
  
  # Contingency Coefficient
  res_cont <- moderncor_cat(x, y, method = "contingency")
  expect_equal(res_cont$estimate, DescTools::ContCoef(x, y))
  
  # Tschuprow's T
  res_tsch <- moderncor_cat(x, y, method = "tschuprow")
  expect_equal(res_tsch$estimate, DescTools::TschuprowT(x, y))
  
  # Matrix input
  df <- data.frame(
    v1 = factor(sample(c("A", "B"), 20, replace = TRUE)),
    v2 = factor(sample(c("X", "Y"), 20, replace = TRUE)),
    v3 = factor(sample(c("Yes", "No"), 20, replace = TRUE))
  )
  res_mat <- moderncor_cat(df, method = "cramers_v")
  expect_s3_class(res_mat, "moderncor_cat")
  expect_true(is.matrix(res_mat$estimate))
  expect_equal(dim(res_mat$estimate), c(3, 3))
  expect_equal(res_mat$estimate[1, 1], 1.0)
})
