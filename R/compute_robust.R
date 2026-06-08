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
  check_suggested("asbio", "biweight")
  
  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))
  
  # Compute correlation using asbio::r.bw
  res <- asbio::r.bw(x, y, ...)
  # r.bw returns a matrix/data.frame-like object with column 'r.xy'
  est <- as.numeric(res[1, "r.xy"])
  
  n <- length(x)
  pval <- NULL
  stat <- NULL
  
  if (p_value) {
    if (n <= 2) {
      stat <- NA_real_
      pval <- NA_real_
    } else if (abs(est) >= 1) {
      stat <- Inf * sign(est)
      pval <- 0
    } else {
      stat <- est * sqrt((n - 2) / (1 - est^2))
      pval <- switch(
        alternative,
        "two.sided" = 2 * stats::pt(abs(stat), df = n - 2, lower.tail = FALSE),
        "greater"   = stats::pt(stat, df = n - 2, lower.tail = FALSE),
        "less"      = stats::pt(stat, df = n - 2, lower.tail = TRUE)
      )
    }
  }
  
  list(
    estimate = est,
    method = "biweight",
    method_label = "Biweight Midcorrelation",
    statistic = stat,
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
