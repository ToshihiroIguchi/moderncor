test_that("Partial and semi-partial correlations compute correctly", {
  skip_if_not_installed("ppcor")
  
  set.seed(42)
  x <- rnorm(30)
  y <- rnorm(30)
  z <- rnorm(30)
  
  # partial
  res_part <- moderncor(x, y, z = z, method = "partial")
  expect_s3_class(res_part, "moderncor")
  expect_equal(res_part$estimate, ppcor::pcor.test(x, y, z)$estimate)
  expect_equal(res_part$p.value, ppcor::pcor.test(x, y, z)$p.value)
  
  # semi-partial
  res_spart <- moderncor(x, y, z = z, method = "semi_partial")
  expect_s3_class(res_spart, "moderncor")
  expect_equal(res_spart$estimate, ppcor::spcor.test(x, y, z)$estimate)
  expect_equal(res_spart$p.value, ppcor::spcor.test(x, y, z)$p.value)
  
  # validation errors when z is missing
  expect_error(moderncor(x, y, method = "partial"), "z \\(control variables\\) must be provided")
})
