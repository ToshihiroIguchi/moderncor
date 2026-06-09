# No CRAN package provides biweight midcorrelation without WGCNA (Bioconductor-only).
# Self-implementing the standard formula (Wilcox 2012 / WGCNA) per CLAUDE.md exception:
# CRAN-only constraint prevents using WGCNA; the formula is well-established.
.bicor <- function(x, y) {
  m_x <- stats::median(x)
  m_y <- stats::median(y)
  # Unscaled MAD (constant = 1) as in the biweight midcorrelation standard definition
  s_x <- 9 * stats::mad(x, constant = 1)
  s_y <- 9 * stats::mad(y, constant = 1)
  if (s_x == 0 || s_y == 0) return(NA_real_)
  u  <- (x - m_x) / s_x
  v  <- (y - m_y) / s_y
  wx <- ifelse(abs(u) < 1, (1 - u^2)^2, 0)
  wy <- ifelse(abs(v) < 1, (1 - v^2)^2, 0)
  num   <- sum((x - m_x) * wx * (y - m_y) * wy)
  denom <- sqrt(sum((x - m_x)^2 * wx^2)) * sqrt(sum((y - m_y)^2 * wy^2))
  if (denom == 0) return(NA_real_)
  max(-1, min(1, num / denom))
}

#' Compute Biweight Midcorrelation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis ("two.sided", "greater", "less").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments (currently unused).
#' @return A list of correlation results.
#' @keywords internal
compute_biweight <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))

  est <- .bicor(x, y)

  if (!p_value || is.na(est)) {
    return(list(
      estimate = est,
      method = "biweight",
      method_label = "Biweight Midcorrelation",
      statistic = NULL,
      p.value = NULL
    ))
  }

  n      <- length(x)
  t_stat <- est * sqrt((n - 2) / (1 - est^2))
  pval   <- switch(alternative,
    two.sided = 2 * stats::pt(-abs(t_stat), df = n - 2),
    greater   = stats::pt(t_stat, df = n - 2, lower.tail = FALSE),
    less      = stats::pt(t_stat, df = n - 2)
  )

  list(
    estimate = est,
    method = "biweight",
    method_label = "Biweight Midcorrelation",
    statistic = t_stat,
    p.value = pval
  )
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
