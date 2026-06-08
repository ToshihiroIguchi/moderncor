#' Compute Pearson correlation coefficient
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis.
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments.
#' @return A list of correlation results.
#' @keywords internal
compute_pearson <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  if (p_value) {
    # cor.test handles alternative and computes p-value/statistics
    res <- stats::cor.test(x, y, method = "pearson", alternative = alternative, ...)
    list(
      estimate = unname(res$estimate),
      method = "pearson",
      method_label = "Pearson Product-Moment Correlation",
      statistic = unname(res$statistic),
      p.value = res$p.value
    )
  } else {
    est <- stats::cor(x, y, method = "pearson", ...)
    list(
      estimate = est,
      method = "pearson",
      method_label = "Pearson Product-Moment Correlation",
      statistic = NULL,
      p.value = NULL
    )
  }
}

#' Compute Spearman rank correlation coefficient
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis.
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments.
#' @return A list of correlation results.
#' @keywords internal
compute_spearman <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  if (p_value) {
    res <- stats::cor.test(x, y, method = "spearman", alternative = alternative, ...)
    list(
      estimate = unname(res$estimate),
      method = "spearman",
      method_label = "Spearman Rank Correlation",
      statistic = unname(res$statistic),
      p.value = res$p.value
    )
  } else {
    est <- stats::cor(x, y, method = "spearman", ...)
    list(
      estimate = est,
      method = "spearman",
      method_label = "Spearman Rank Correlation",
      statistic = NULL,
      p.value = NULL
    )
  }
}

#' Compute Kendall rank correlation coefficient
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis.
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments.
#' @return A list of correlation results.
#' @keywords internal
compute_kendall <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  if (p_value) {
    res <- stats::cor.test(x, y, method = "kendall", alternative = alternative, ...)
    list(
      estimate = unname(res$estimate),
      method = "kendall",
      method_label = "Kendall Rank Correlation",
      statistic = unname(res$statistic),
      p.value = res$p.value
    )
  } else {
    est <- stats::cor(x, y, method = "kendall", ...)
    list(
      estimate = est,
      method = "kendall",
      method_label = "Kendall Rank Correlation",
      statistic = NULL,
      p.value = NULL
    )
  }
}
