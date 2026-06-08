#' Check if a suggested package is available
#'
#' @param pkg The package name.
#' @param method The method requesting the package.
#' @keywords internal
check_suggested <- function(pkg, method) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      'Package "%s" is required for method = "%s". Install it with: install.packages("%s")',
      pkg, method, pkg
    ), call. = FALSE)
  }
}

#' Validate and normalize input for moderncor
#'
#' @param x A numeric vector, matrix, or data.frame.
#' @param y A numeric vector, or NULL.
#' @param method Character: the correlation method.
#' @param use Character: how to handle missing values.
#' @return A list containing the normalized data and input type.
#' @keywords internal
validate_input <- function(x, y, method, use) {
  # Handle data.frame or matrix input
  if (is.data.frame(x) || is.matrix(x)) {
    if (!is.null(y)) {
      stop("y must be NULL if x is a matrix or data.frame", call. = FALSE)
    }
    
    # Convert data.frame to matrix
    mat <- as.matrix(x)
    if (!is.numeric(mat)) {
      stop("x must be numeric", call. = FALSE)
    }
    
    # Handle NA according to "use"
    if (use == "complete.obs") {
      mat <- mat[stats::complete.cases(mat), , drop = FALSE]
      if (nrow(mat) == 0) {
        warning("No complete observations remaining after removing NA", call. = FALSE)
      }
    }
    
    return(list(
      type = "matrix",
      data = mat
    ))
  }
  
  # Handle vector input
  if (is.null(y)) {
    stop("y must be provided if x is a vector", call. = FALSE)
  }
  
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric vectors", call. = FALSE)
  }
  
  if (length(x) != length(y)) {
    stop("x and y must have the same length", call. = FALSE)
  }
  
  # Handle NA according to "use"
  if (use == "complete.obs" || use == "pairwise.complete.obs") {
    complete_idx <- !is.na(x) & !is.na(y)
    x <- x[complete_idx]
    y <- y[complete_idx]
  }
  
  return(list(
    type = "vector",
    x = x,
    y = y
  ))
}

#' Dispatch to the appropriate pair-wise compute function
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param method Character: correlation method.
#' @param alternative Character: alternative hypothesis.
#' @param p_value Logical: whether to calculate p-value.
#' @param ... Additional arguments.
#' @return A list of correlation results.
#' @keywords internal
compute_pair <- function(x, y, method, alternative, p_value, ...) {
  if (any(is.na(x)) || any(is.na(y))) {
    return(list(
      estimate = NA_real_,
      method = method,
      method_label = method_info(method)$label,
      statistic = NA_real_,
      p.value = NA_real_
    ))
  }
  
  if (length(x) < 3) {
    return(list(
      estimate = NA_real_,
      method = method,
      method_label = method_info(method)$label,
      statistic = NA_real_,
      p.value = NA_real_
    ))
  }
  
  switch(method,
    pearson     = compute_pearson(x, y, alternative = alternative, p_value = p_value, ...),
    spearman    = compute_spearman(x, y, alternative = alternative, p_value = p_value, ...),
    kendall     = compute_kendall(x, y, alternative = alternative, p_value = p_value, ...),
    dcor        = compute_dcor(x, y, alternative = alternative, p_value = p_value, ...),
    mic         = compute_mic(x, y, alternative = alternative, p_value = p_value, ...),
    hsic        = compute_hsic(x, y, alternative = alternative, p_value = p_value, ...),
    xi          = compute_xi(x, y, alternative = alternative, p_value = p_value, ...),
    hoeffding   = compute_hoeffding(x, y, alternative = alternative, p_value = p_value, ...),
    mutual_info = compute_mutual_info(x, y, alternative = alternative, p_value = p_value, ...)
  )
}

#' Compute correlation matrix for all variable pairs
#'
#' @param mat Numeric matrix.
#' @param method Character: correlation method.
#' @param use Character: missing value treatment.
#' @param alternative Character: alternative hypothesis.
#' @param p_value Logical: whether to compute p-values.
#' @param call The original function call.
#' @param ... Additional arguments.
#' @return A moderncor object for matrix input.
#' @keywords internal
compute_matrix <- function(mat, method, use, alternative, p_value, call, ...) {
  n_cols <- ncol(mat)
  col_names <- colnames(mat)
  if (is.null(col_names)) {
    col_names <- paste0("V", seq_len(n_cols))
  }
  
  est_mat <- matrix(NA_real_, nrow = n_cols, ncol = n_cols, dimnames = list(col_names, col_names))
  diag(est_mat) <- 1.0 # default self-correlation
  
  pval_mat <- NULL
  if (p_value) {
    pval_mat <- matrix(NA_real_, nrow = n_cols, ncol = n_cols, dimnames = list(col_names, col_names))
    diag(pval_mat) <- 0.0
  }
  
  # For symmetric measures, we only compute the upper triangle.
  # Check if the method is symmetric.
  # Pearson, Spearman, Kendall, dCor, MIC, HSIC, Hoeffding, Mutual Info are symmetric.
  # Chatterjee's Xi is asymmetric! (xi(x,y) != xi(y,x))
  is_symmetric <- method != "xi"
  
  for (i in seq_len(n_cols)) {
    for (j in seq_len(n_cols)) {
      if (i == j) next
      
      # For symmetric methods, if we already computed (j, i), copy it.
      if (is_symmetric && j < i) {
        est_mat[i, j] <- est_mat[j, i]
        if (!is.null(pval_mat)) {
          pval_mat[i, j] <- pval_mat[j, i]
        }
        next
      }
      
      col_i <- mat[, i]
      col_j <- mat[, j]
      
      # Handle pairwise NA removal if requested
      if (use == "pairwise.complete.obs") {
        complete_idx <- !is.na(col_i) & !is.na(col_j)
        col_i <- col_i[complete_idx]
        col_j <- col_j[complete_idx]
      }
      
      res <- compute_pair(col_i, col_j, method = method, alternative = alternative, p_value = p_value, ...)
      
      est_mat[i, j] <- res$estimate
      if (!is.null(pval_mat)) {
        pval_mat[i, j] <- res$p.value
      }
    }
  }
  
  structure(
    list(
      estimate = est_mat,
      method = method,
      method_label = method_info(method)$label,
      p.value = pval_mat,
      n = nrow(mat),
      call = call
    ),
    class = "moderncor"
  )
}
