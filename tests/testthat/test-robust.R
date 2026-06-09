test_that("biweight midcorrelation computes correctly", {
  set.seed(42)
  x <- rnorm(30)
  y <- rnorm(30)

  res_bw <- moderncor(x, y, method = "biweight")
  expect_s3_class(res_bw, "moderncor")
  expect_true(is.numeric(res_bw$estimate))
  expect_true(res_bw$estimate >= -1 && res_bw$estimate <= 1)
  expect_true(res_bw$p.value >= 0 && res_bw$p.value <= 1)
})

test_that("biweight returns 1 for perfect positive correlation", {
  x <- 1:20
  y <- 1:20
  res <- moderncor(x, y, method = "biweight")
  expect_equal(res$estimate, 1, tolerance = 1e-6)
})

test_that("biweight returns -1 for perfect negative correlation", {
  x <- 1:20
  y <- -(1:20)
  res <- moderncor(x, y, method = "biweight")
  expect_equal(res$estimate, -1, tolerance = 1e-6)
})

test_that("biweight one-sided alternatives produce valid p-values", {
  set.seed(1)
  x <- 1:20
  y <- x + rnorm(20, sd = 0.5)

  res_gt <- moderncor(x, y, method = "biweight", alternative = "greater")
  res_lt <- moderncor(x, y, method = "biweight", alternative = "less")
  res_ts <- moderncor(x, y, method = "biweight", alternative = "two.sided")

  expect_true(res_gt$p.value <= 0.5)
  expect_true(res_lt$p.value >= 0.5)
  # One-sided p-values are complementary; two-sided = 2 * min(one-sided)
  expect_equal(res_gt$p.value + res_lt$p.value, 1, tolerance = 1e-10)
  expect_equal(res_ts$p.value, 2 * min(res_gt$p.value, res_lt$p.value), tolerance = 1e-10)
})

test_that("biweight p_value = FALSE omits statistic and p.value", {
  x <- rnorm(20)
  y <- rnorm(20)
  res <- moderncor(x, y, method = "biweight", p_value = FALSE)
  expect_null(res$statistic)
  expect_null(res$p.value)
})

test_that("percentage_bend and winsorized compute correctly", {
  skip_if_not_installed("WRS2")

  set.seed(42)
  x <- rnorm(15)
  y <- rnorm(15)

  res_pb <- moderncor(x, y, method = "percentage_bend")
  expect_s3_class(res_pb, "moderncor")
  expect_equal(res_pb$estimate, WRS2::pbcor(x, y)$cor)
  expect_equal(res_pb$p.value, WRS2::pbcor(x, y)$p.value)

  res_win <- moderncor(x, y, method = "winsorized")
  expect_s3_class(res_win, "moderncor")
  expect_equal(res_win$estimate, WRS2::wincor(x, y)$cor)
  expect_equal(res_win$p.value, WRS2::wincor(x, y)$p.value)
})
