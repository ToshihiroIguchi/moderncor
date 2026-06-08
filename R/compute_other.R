#' Compute Ball Correlation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments passed to Ball::bcov.test or Ball::bcor.
#' @return A list of correlation results.
#' @keywords internal
compute_ball <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("Ball", "ball")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for Ball correlation. Using two-sided test.", call. = FALSE)
  }
  
  # For Ball::bcor
  # Extract arguments for bcor vs bcov.test
  args <- list(...)
  bcor_args <- args
  bcor_args$num.permutations <- NULL
  bcor_args$R <- NULL
  
  est <- do.call(Ball::bcor, c(list(x, y), bcor_args))
  est <- as.numeric(est)
  
  pval <- NULL
  stat <- NULL
  
  if (p_value) {
    # Default replicates in Ball::bcov.test is 99. We can pass num.permutations or R.
    test_args <- args
    if (is.null(test_args$num.permutations) && !is.null(test_args$R)) {
      test_args$num.permutations <- test_args$R
      test_args$R <- NULL
    }
    
    test_res <- do.call(Ball::bcov.test, c(list(x, y), test_args))
    stat <- unname(test_res$statistic)
    pval <- unname(test_res$p.value)
  }
  
  list(
    estimate = est,
    method = "ball",
    method_label = "Ball Correlation",
    statistic = stat,
    p.value = pval
  )
}

#' Compute Bergsma-Dassios Tau* Correlation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments passed to TauStar::tauStarTest.
#' @return A list of correlation results.
#' @keywords internal
compute_tau_star <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("TauStar", "tau_star")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for Bergsma-Dassios Tau* correlation. Using two-sided test.", call. = FALSE)
  }
  
  if (p_value) {
    res <- TauStar::tauStarTest(x, y, ...)
    list(
      estimate = res$tStar,
      method = "tau_star",
      method_label = "Bergsma-Dassios Tau*",
      statistic = res$tStar,
      p.value = as.numeric(res$pVal)
    )
  } else {
    est <- TauStar::tStar(x, y)
    list(
      estimate = est,
      method = "tau_star",
      method_label = "Bergsma-Dassios Tau*",
      statistic = NULL,
      p.value = NULL
    )
  }
}
