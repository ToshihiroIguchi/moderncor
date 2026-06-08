test_that("Ordinal correlations compute correctly", {
  skip_if_not_installed("psych")
  
  set.seed(42)
  # polychoric
  x_poly <- sample(1:3, 30, replace = TRUE)
  y_poly <- sample(1:3, 30, replace = TRUE)
  res_poly <- moderncor(x_poly, y_poly, method = "polychoric")
  expect_s3_class(res_poly, "moderncor")
  expect_equal(res_poly$estimate, psych::polychoric(data.frame(x = x_poly, y = y_poly))$rho[1, 2])
  expect_null(res_poly$p.value)
  
  # tetrachoric
  x_tetra <- sample(0:1, 30, replace = TRUE)
  y_tetra <- sample(0:1, 30, replace = TRUE)
  res_tetra <- moderncor(x_tetra, y_tetra, method = "tetrachoric")
  expect_s3_class(res_tetra, "moderncor")
  expect_equal(res_tetra$estimate, psych::tetrachoric(data.frame(x = x_tetra, y = y_tetra))$rho[1, 2])
  expect_null(res_tetra$p.value)
})
