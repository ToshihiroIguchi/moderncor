test_that("distance correlation works", {
  set.seed(42)
  x <- rnorm(15)
  y <- x^2 + rnorm(15, sd = 0.1) # quadratic dependency
  
  res <- moderncor(x, y, method = "dcor")
  expect_s3_class(res, "moderncor")
  expect_gt(res$estimate, 0)
  expect_true(!is.null(res$p.value))
  
  # Test without p-value
  res_no_p <- moderncor(x, y, method = "dcor", p_value = FALSE)
  expect_null(res_no_p$p.value)
})

test_that("Chatterjee's xi works", {
  set.seed(42)
  x <- rnorm(15)
  y <- x^2 + rnorm(15, sd = 0.1)
  
  res <- moderncor(x, y, method = "xi")
  expect_s3_class(res, "moderncor")
  expect_gt(res$estimate, 0)
  expect_true(!is.null(res$p.value))
  
  # Test without p-value
  res_no_p <- moderncor(x, y, method = "xi", p_value = FALSE)
  expect_null(res_no_p$p.value)
})

test_that("MIC works when minerva is installed", {
  skip_if_not_installed("minerva")
  
  set.seed(42)
  x <- rnorm(15)
  y <- x^2 + rnorm(15, sd = 0.1)
  
  res <- moderncor(x, y, method = "mic", B = 10) # use small B for speed
  expect_s3_class(res, "moderncor")
  expect_gt(res$estimate, 0.3)
  expect_true(!is.null(res$p.value))
  
  res_no_p <- moderncor(x, y, method = "mic", p_value = FALSE)
  expect_null(res_no_p$p.value)
})

test_that("HSIC works when dHSIC is installed", {
  skip_if_not_installed("dHSIC")
  
  set.seed(42)
  x <- rnorm(15)
  y <- x^2 + rnorm(15, sd = 0.1)
  
  res <- moderncor(x, y, method = "hsic")
  expect_s3_class(res, "moderncor")
  expect_gt(res$estimate, 0)
  expect_true(!is.null(res$p.value))

  res_no_p <- moderncor(x, y, method = "hsic", p_value = FALSE)
  expect_null(res_no_p$p.value)

  # estimate must be the dHSIC value and independent of whether p_value is
  # requested (regression guard: it previously returned the test statistic).
  expect_equal(res$estimate, dHSIC::dhsic(list(x, y))$dHSIC)
  expect_equal(res$estimate, res_no_p$estimate)
})

test_that("Hoeffding's D works when Hmisc is installed", {
  skip_if_not_installed("Hmisc")
  
  set.seed(42)
  x <- rnorm(15)
  y <- x^2 + rnorm(15, sd = 0.1)
  
  res <- moderncor(x, y, method = "hoeffding")
  expect_s3_class(res, "moderncor")
  expect_gt(res$estimate, 0)
  expect_true(!is.null(res$p.value))
  
  res_no_p <- moderncor(x, y, method = "hoeffding", p_value = FALSE)
  expect_null(res_no_p$p.value)
})

test_that("Mutual Information works when infotheo is installed", {
  skip_if_not_installed("infotheo")
  
  set.seed(42)
  x <- rnorm(15)
  y <- x^2 + rnorm(15, sd = 0.1)
  
  res <- moderncor(x, y, method = "mutual_info", B = 10) # small B for speed
  expect_s3_class(res, "moderncor")
  expect_gt(res$estimate, 0)
  expect_true(!is.null(res$p.value))
  
  res_no_p <- moderncor(x, y, method = "mutual_info", p_value = FALSE)
  expect_null(res_no_p$p.value)
})
