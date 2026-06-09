#' Compute Biweight Midcorrelation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis ("two.sided", "greater", "less").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments.
#' @return A list of correlation results.
#' @keywords internal
compute_biweight <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("WGCNA", "biweight")

  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))

  if (p_value) {
    # WGCNA::bicorAndPvalue() returns the biweight midcorrelation together with
    # its t statistic and p-value (including one-sided alternatives), so both the
    # statistic and the p-value are delegated to WGCNA rather than self-implemented.
    res <- WGCNA::bicorAndPvalue(x, y, alternative = alternative, ...)
    list(
      estimate = as.numeric(res$bicor[1, 1]),
      method = "biweight",
      method_label = "Biweight Midcorrelation",
      statistic = as.numeric(res$t[1, 1]),
      p.value = as.numeric(res$p[1, 1])
    )
  } else {
    est <- as.numeric(WGCNA::bicor(x, y, ...))
    list(
      estimate = est,
      method = "biweight",
      method_label = "Biweight Midcorrelation",
      statistic = NULL,
      p.value = NULL
    )
  }
}

#' Compute Percentage Bend Correlation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments passed to WRS2::pbcor.
#' @return A list of correlation results.
#' @keywords internal
compute_percentage_bend <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("WRS2", "percentage_bend")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for Percentage Bend correlation. Using two-sided test.", call. = FALSE)
  }
  
  res <- WRS2::pbcor(x, y, ...)
  
  list(
    estimate = res$cor,
    method = "percentage_bend",
    method_label = "Percentage Bend Correlation",
    statistic = res$test,
    p.value = if (p_value) res$p.value else NULL
  )
}

#' Compute Winsorized Correlation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments passed to WRS2::wincor.
#' @return A list of correlation results.
#' @keywords internal
compute_winsorized <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("WRS2", "winsorized")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for Winsorized correlation. Using two-sided test.", call. = FALSE)
  }
  
  res <- WRS2::wincor(x, y, ...)
  
  list(
    estimate = res$cor,
    method = "winsorized",
    method_label = "Winsorized Correlation",
    statistic = res$test,
    p.value = if (p_value) res$p.value else NULL
  )
}
