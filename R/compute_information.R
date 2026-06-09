#' Compute Mutual Information
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param alternative Character: alternative hypothesis (ignored with warning if not "two.sided").
#' @param p_value Logical: whether to compute p-value via permutation.
#' @param ... Additional arguments passed to infotheo::mutinformation or discretization.
#' @return A list of correlation results.
#' @keywords internal
compute_mutual_info <- function(x, y, alternative = "two.sided", p_value = TRUE, ...) {
  check_suggested("infotheo", "mutual_info")
  
  if (alternative != "two.sided") {
    warning("alternative is ignored for Mutual Information. Using default independence test.", call. = FALSE)
  }
  
  args <- list(...)
  
  # Discretize continuous vectors
  disc_method <- if (!is.null(args$discretize_method)) args$discretize_method else "equalwidth"
  
  # Default nbins is floor(N^(1/3))
  N <- length(x)
  disc_nbins <- if (!is.null(args$discretize_nbins)) args$discretize_nbins else floor(N^(1/3))
  
  # Remove discretization arguments from args
  args$discretize_method <- NULL
  args$discretize_nbins <- NULL
  
  # infotheo::discretize expects a numeric vector or matrix, returns a data.frame/matrix
  dx <- infotheo::discretize(x, disc = disc_method, nbins = disc_nbins)
  dy <- infotheo::discretize(y, disc = disc_method, nbins = disc_nbins)
  
  # mutinformation expects discrete variables (columns of data.frames or vectors)
  # Convert to standard numeric vector as representation of bins
  dx_vec <- dx[[1]]
  dy_vec <- dy[[1]]
  
  mi_args <- args
  mi_args$B <- NULL
  
  mi <- do.call(infotheo::mutinformation, c(list(dx_vec, dy_vec), mi_args))
  
  pval <- NULL
  if (p_value) {
    # infotheo::mutinformation returns only the MI estimate, not a p-value, and
    # no CRAN package provides an MI-based independence test, so a permutation
    # test is self-implemented here (permitted by CLAUDE.md in this case).
    B <- if (!is.null(args$B)) args$B else 99

    perm_mis <- numeric(B)
    for (b in seq_len(B)) {
      perm_dy_vec <- dy_vec[sample.int(N)]
      perm_mis[b] <- do.call(infotheo::mutinformation, c(list(dx_vec, perm_dy_vec), mi_args))
    }
    
    pval <- (sum(perm_mis >= mi) + 1) / (B + 1)
  }
  
  # MI has no distinct test statistic; the permutation test uses the estimate
  # itself, so `statistic` is left NULL to avoid duplicating `estimate`.
  list(
    estimate = mi,
    method = "mutual_info",
    method_label = "Mutual Information",
    statistic = NULL,
    p.value = pval
  )
}
