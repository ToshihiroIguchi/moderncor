#' Compute Distance Correlation (dCor)
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments passed to energy::dcor.test or energy::dcor.
#' @return A list of correlation results.
#' @keywords internal
compute_dcor <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  if (alternative != "two.sided") {
    warning("alternative is ignored for distance correlation. Using default independence test.", call. = FALSE)
  }
  
  if (p_value) {
    # Default number of replicates R is 199 in energy
    # We can pass it via ...
    args <- list(...)
    if (is.null(args$R)) {
      args$R <- 199
    }
    
    test_res <- do.call(energy::dcor.test, c(list(x, y), args))
    # dcor.test$statistic is nV_n (test statistic), not the dCor estimate
    est <- energy::dcor(x, y)
    list(
      estimate = est,
      method = "dcor",
      method_label = "Distance Correlation",
      statistic = unname(test_res$statistic),
      p.value = test_res$p.value
    )
  } else {
    est <- energy::dcor(x, y, ...)
    list(
      estimate = est,
      method = "dcor",
      method_label = "Distance Correlation",
      statistic = NULL,
      p.value = NULL
    )
  }
}

#' Compute Maximal Information Coefficient (MIC)
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value via permutation.
#' @param ... Additional arguments passed to minerva::mine.
#' @return A list of correlation results.
#' @keywords internal
compute_mic <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("minerva", "mic")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for MIC. Using default independence test.", call. = FALSE)
  }
  
  # Compute observed MIC
  # Remove B from ... since it is used for our permutation test, not for mine()
  args <- list(...)
  mine_args <- args
  mine_args$B <- NULL
  mine_res <- do.call(minerva::mine, c(list(x, y), mine_args))
  est <- mine_res$MIC[1]
  
  pval <- NULL
  if (p_value) {
    # minerva::mine returns only the MIC estimate, not a p-value, and no CRAN
    # package provides an MIC independence test, so a permutation test is
    # self-implemented here (CLAUDE.md permits self-implementation in this case).
    # Extract optional B (number of permutations) from ... or default to 99
    B <- if (!is.null(args$B)) args$B else 99
    
    perm_mics <- numeric(B)
    n <- length(y)
    for (b in seq_len(B)) {
      perm_y <- y[sample.int(n)]
      perm_res <- do.call(minerva::mine, c(list(x, perm_y), mine_args))
      perm_mics[b] <- perm_res$MIC[1]
    }
    
    pval <- (sum(perm_mics >= est) + 1) / (B + 1)
  }
  
  # MIC has no distinct test statistic; the permutation test uses the estimate
  # itself, so `statistic` is left NULL to avoid duplicating `estimate`.
  list(
    estimate = est,
    method = "mic",
    method_label = "Maximal Information Coefficient (MIC)",
    statistic = NULL,
    p.value = pval
  )
}

#' Compute Hilbert-Schmidt Independence Criterion (HSIC)
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments passed to dHSIC::dhsic.test or dHSIC::dhsic.
#' @return A list of correlation results.
#' @keywords internal
compute_hsic <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("dHSIC", "hsic")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for HSIC. Using default independence test.", call. = FALSE)
  }
  
  if (p_value) {
    # dhsic.test$statistic is the test statistic (n * dHSIC), not the dHSIC
    # estimate itself. Compute the estimate separately via dHSIC::dhsic so that
    # `estimate` is consistent regardless of whether p_value is requested.
    test_res <- dHSIC::dhsic.test(list(x, y), ...)
    est <- dHSIC::dhsic(list(x, y))$dHSIC
    list(
      estimate = est,
      method = "hsic",
      method_label = "Hilbert-Schmidt Independence Criterion (HSIC)",
      statistic = unname(test_res$statistic),
      p.value = test_res$p.value
    )
  } else {
    res <- dHSIC::dhsic(list(x, y), ...)
    list(
      estimate = res$dHSIC,
      method = "hsic",
      method_label = "Hilbert-Schmidt Independence Criterion (HSIC)",
      statistic = NULL,
      p.value = NULL
    )
  }
}

#' Compute Chatterjee's Xi Correlation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments passed to XICOR::xicor.
#' @return A list of correlation results.
#' @keywords internal
compute_xi <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  if (alternative != "two.sided") {
    warning("alternative is ignored for Chatterjee's Xi. Using default independence test.", call. = FALSE)
  }
  
  res <- XICOR::xicor(x, y, pvalue = p_value, ...)
  
  if (p_value) {
    # XICOR reports the xi coefficient and its p-value but not a separate test
    # statistic, so `statistic` is left NULL rather than duplicating `estimate`.
    list(
      estimate = unname(res$xi),
      method = "xi",
      method_label = "Chatterjee's Xi Correlation",
      statistic = NULL,
      p.value = res$pval
    )
  } else {
    list(
      estimate = unname(res),
      method = "xi",
      method_label = "Chatterjee's Xi Correlation",
      statistic = NULL,
      p.value = NULL
    )
  }
}

#' Compute Hoeffding's D
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value.
#' @param ... Additional arguments.
#' @return A list of correlation results.
#' @keywords internal
compute_hoeffding <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("Hmisc", "hoeffding")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for Hoeffding's D. Using default independence test.", call. = FALSE)
  }
  
  res <- Hmisc::hoeffd(x, y)
  
  pval <- if (p_value) res$P[1, 2] else NULL

  # Hmisc::hoeffd derives the p-value from the asymptotic distribution of D and
  # exposes no separate test statistic, so `statistic` is left NULL.
  list(
    estimate = res$D[1, 2],
    method = "hoeffding",
    method_label = "Hoeffding's D",
    statistic = NULL,
    p.value = pval
  )
}
