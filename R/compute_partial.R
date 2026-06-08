#' Compute Partial Correlation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param z Numeric vector, matrix, or data.frame representing control variables.
#' @param method_partial Character: correlation method to use ("pearson", "spearman", "kendall").
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments passed to ppcor::pcor.test.
#' @return A list of correlation results.
#' @keywords internal
compute_partial <- function(x, y, z, method_partial = "pearson", alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("ppcor", "partial")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for partial correlation. Using two-sided test.", call. = FALSE)
  }
  
  method_partial <- match.arg(method_partial, c("pearson", "spearman", "kendall"))
  
  res <- ppcor::pcor.test(x, y, z, method = method_partial, ...)
  
  list(
    estimate = res$estimate,
    method = "partial",
    method_label = sprintf("Partial Correlation (%s)", tools::toTitleCase(method_partial)),
    statistic = res$statistic,
    p.value = if (p_value) res$p.value else NULL
  )
}

#' Compute Semi-partial Correlation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param z Numeric vector, matrix, or data.frame representing control variables.
#' @param method_partial Character: correlation method to use ("pearson", "spearman", "kendall").
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments passed to ppcor::spcor.test.
#' @return A list of correlation results.
#' @keywords internal
compute_semi_partial <- function(x, y, z, method_partial = "pearson", alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("ppcor", "semi_partial")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for semi-partial correlation. Using two-sided test.", call. = FALSE)
  }
  
  method_partial <- match.arg(method_partial, c("pearson", "spearman", "kendall"))
  
  res <- ppcor::spcor.test(x, y, z, method = method_partial, ...)
  
  list(
    estimate = res$estimate,
    method = "semi_partial",
    method_label = sprintf("Semi-partial Correlation (%s)", tools::toTitleCase(method_partial)),
    statistic = res$statistic,
    p.value = if (p_value) res$p.value else NULL
  )
}
