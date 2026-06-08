test_that("Robust correlations compute correctly", {
  skip_if_not_installed("asbio")
  skip_if_not_installed("WRS2")
  
  set.seed(42)
  x <- rnorm(15)
  y <- rnorm(15)
  
  # biweight
  res_bw <- moderncor(x, y, method = "biweight")
  expect_s3_class(res_bw, "moderncor")
  expect_equal(res_bw$estimate, as.numeric(asbio::r.bw(x, y)[1, "r.xy"]))
  expect_true(res_bw$p.value >= 0 && res_bw$p.value <= 1)
  
  # percentage_bend
  res_pb <- moderncor(x, y, method = "percentage_bend")
  expect_s3_class(res_pb, "moderncor")
  expect_equal(res_pb$estimate, WRS2::pbcor(x, y)$cor)
  expect_equal(res_pb$p.value, WRS2::pbcor(x, y)$p.value)
  
  # winsorized
  res_win <- moderncor(x, y, method = "winsorized")
  expect_s3_class(res_win, "moderncor")
  expect_equal(res_win$estimate, WRS2::wincor(x, y)$cor)
  expect_equal(res_win$p.value, WRS2::wincor(x, y)$p.value)
})
