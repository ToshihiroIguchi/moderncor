#' Compute Polychoric Correlation
#'
#' @param x Numeric/factor vector.
#' @param y Numeric/factor vector.
#' @param alternative Character: alternative hypothesis (ignored).
#' @param p_value Logical: whether to compute p-value (not supported for this method).
#' @param ... Additional arguments passed to psych::polychoric.
#' @return A list of correlation results.
#' @keywords internal
compute_polychoric <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("psych", "polychoric")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for polychoric correlation.", call. = FALSE)
  }
  
  # Ensure input is suitable. psych::polychoric expects data.frame or matrix
  df <- data.frame(x = x, y = y)
  
  res <- psych::polychoric(df, ...)
  
  # Extract correlation value
  est <- res$rho[1, 2]
  
  list(
    estimate = est,
    method = "polychoric",
    method_label = "Polychoric Correlation",
    statistic = NULL,
    p.value = NULL
  )
}

#' Compute Tetrachoric Correlation
#'
#' @param x Numeric/factor vector.
#' @param y Numeric/factor vector.
#' @param alternative Character: alternative hypothesis (ignored).
#' @param p_value Logical: whether to compute p-value (not supported for this method).
#' @param ... Additional arguments passed to psych::tetrachoric.
#' @return A list of correlation results.
#' @keywords internal
compute_tetrachoric <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("psych", "tetrachoric")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for tetrachoric correlation.", call. = FALSE)
  }
  
  df <- data.frame(x = x, y = y)
  
  res <- psych::tetrachoric(df, ...)
  
  est <- res$rho[1, 2]
  
  list(
    estimate = est,
    method = "tetrachoric",
    method_label = "Tetrachoric Correlation",
    statistic = NULL,
    p.value = NULL
  )
}
