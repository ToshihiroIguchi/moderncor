test_that("moderncor works with vectors", {
  set.seed(42)
  x <- rnorm(10)
  y <- rnorm(10)
  
  res <- moderncor(x, y, method = "pearson")
  expect_s3_class(res, "moderncor")
  expect_equal(res$estimate, cor(x, y, method = "pearson"))
  expect_length(res, 7) # estimate, method, method_label, statistic, p.value, n, call
  
  # Pearson p-value check
  cor_test_res <- cor.test(x, y, method = "pearson")
  expect_equal(res$p.value, cor_test_res$p.value)
})

test_that("moderncor works with Spearman and Kendall", {
  set.seed(42)
  x <- rnorm(10)
  y <- rnorm(10)
  
  res_spearman <- moderncor(x, y, method = "spearman")
  expect_equal(res_spearman$estimate, cor(x, y, method = "spearman"))
  
  res_kendall <- moderncor(x, y, method = "kendall")
  expect_equal(res_kendall$estimate, cor(x, y, method = "kendall"))
})

test_that("moderncor matrix input works", {
  mat <- iris[1:10, 1:4]
  res <- moderncor(mat, method = "pearson")
  
  expect_s3_class(res, "moderncor")
  expect_true(is.matrix(res$estimate))
  expect_equal(res$estimate, cor(mat, method = "pearson"))
  expect_equal(dim(res$estimate), c(4, 4))
  expect_equal(res$estimate[1, 1], 1.0)
})

test_that("moderncor handles input validation errors", {
  # Non-numeric
  expect_error(moderncor(c("a", "b"), c(1, 2)), "Both x and y must be numeric vectors")
  
  # Different lengths
  expect_error(moderncor(c(1, 2), c(1, 2, 3)), "x and y must have the same length")
  
  # Matrix input with y provided
  expect_error(moderncor(matrix(1:4, 2), c(1, 2)), "y must be NULL if x is a matrix or data.frame")
  
  # Vector input without y
  expect_error(moderncor(c(1, 2), NULL), "y must be provided if x is a vector")
})

test_that("as.data.frame works for moderncor objects", {
  set.seed(42)
  x <- rnorm(10)
  y <- rnorm(10)
  
  # Vector output conversion
  res_vec <- moderncor(x, y, method = "pearson")
  df_vec <- as.data.frame(res_vec)
  expect_s3_class(df_vec, "data.frame")
  expect_equal(nrow(df_vec), 1)
  expect_equal(df_vec$estimate, res_vec$estimate)
  
  # Matrix output conversion
  mat <- iris[1:5, 1:3]
  res_mat <- moderncor(mat, method = "pearson")
  df_mat <- as.data.frame(res_mat)
  expect_s3_class(df_mat, "data.frame")
  # 3 variables -> 3 * 3 = 9 pairs. Diagonals removed -> 9 - 3 = 6 pairs.
  expect_equal(nrow(df_mat), 6)
  expect_setequal(df_mat$var1, c("Sepal.Length", "Sepal.Width", "Petal.Length"))
})

test_that("p_value parameter controls calculation", {
  set.seed(42)
  x <- rnorm(10)
  y <- rnorm(10)
  
  res <- moderncor(x, y, method = "pearson", p_value = FALSE)
  expect_null(res$p.value)
  expect_null(res$statistic)
  
  mat <- iris[1:5, 1:3]
  res_mat <- moderncor(mat, method = "pearson", p_value = FALSE)
  expect_null(res_mat$p.value)
})

test_that("NA handling works according to use parameter", {
  x <- c(1, 2, NA, 4, 5)
  y <- c(5, NA, 3, 2, 1)
  
  # complete.obs deletes rows with NA
  res <- moderncor(x, y, method = "pearson", use = "complete.obs")
  expect_equal(res$n, 3) # index 1, 4, 5 are complete
  
  # everything preserves NA and should return NA for estimate
  res_na <- moderncor(x, y, method = "pearson", use = "everything")
  expect_true(is.na(res_na$estimate))
})
