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
validate_input <- function(x, y, z = NULL, method, use) {
  is_partial <- method %in% c("partial", "semi_partial")
  
  if (is_partial && is.null(z)) {
    stop("z (control variables) must be provided for partial or semi_partial correlation", call. = FALSE)
  }
  
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
    
    n_obs <- nrow(mat)
    z_val <- NULL
    
    if (!is.null(z)) {
      z_len <- if (is.matrix(z) || is.data.frame(z)) nrow(z) else length(z)
      if (z_len != n_obs) {
        stop("z must have the same number of observations (rows) as x", call. = FALSE)
      }
      if (is.data.frame(z)) {
        z_val <- as.matrix(z)
      } else {
        z_val <- z
      }
    }
    
    # Handle NA according to "use"
    if (use == "complete.obs") {
      if (!is.null(z_val)) {
        z_complete <- if (is.matrix(z_val)) stats::complete.cases(z_val) else !is.na(z_val)
        complete_idx <- stats::complete.cases(mat) & z_complete
        mat <- mat[complete_idx, , drop = FALSE]
        if (is.matrix(z_val)) {
          z_val <- z_val[complete_idx, , drop = FALSE]
        } else {
          z_val <- z_val[complete_idx]
        }
      } else {
        mat <- mat[stats::complete.cases(mat), , drop = FALSE]
      }
      
      if (nrow(mat) == 0) {
        warning("No complete observations remaining after removing NA", call. = FALSE)
      }
    }
    
    return(list(
      type = "matrix",
      data = mat,
      z = z_val
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
  
  n_obs <- length(x)
  z_val <- NULL
  
  if (!is.null(z)) {
    z_len <- if (is.matrix(z) || is.data.frame(z)) nrow(z) else length(z)
    if (z_len != n_obs) {
      stop("z must have the same length (or number of rows) as x and y", call. = FALSE)
    }
    if (is.data.frame(z)) {
      z_val <- as.matrix(z)
    } else {
      z_val <- z
    }
  }
  
  # Handle NA according to "use"
  if (use == "complete.obs" || use == "pairwise.complete.obs") {
    complete_idx <- !is.na(x) & !is.na(y)
    if (!is.null(z_val)) {
      z_complete <- if (is.matrix(z_val)) stats::complete.cases(z_val) else !is.na(z_val)
      complete_idx <- complete_idx & z_complete
    }
    
    x <- x[complete_idx]
    y <- y[complete_idx]
    if (!is.null(z_val)) {
      if (is.matrix(z_val)) {
        z_val <- z_val[complete_idx, , drop = FALSE]
      } else {
        z_val <- z_val[complete_idx]
      }
    }
  }
  
  return(list(
    type = "vector",
    x = x,
    y = y,
    z = z_val
  ))
}

#' Dispatch to the appropriate pair-wise compute function
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param z Numeric vector, matrix, or data.frame representing control variables.
#' @param method Character: correlation method.
#' @param alternative Character: alternative hypothesis.
#' @param p_value Logical: whether to calculate p-value.
#' @param method_partial Character: correlation method to use for partial/semi-partial.
#' @param ... Additional arguments.
#' @return A list of correlation results.
#' @keywords internal
compute_pair <- function(x, y, z = NULL, method, alternative, p_value, method_partial = "pearson", ...) {
  # If there is z, check NAs for z as well
  z_has_na <- FALSE
  if (!is.null(z)) {
    z_has_na <- if (is.matrix(z) || is.data.frame(z)) any(is.na(z)) else any(is.na(z))
  }
  
  if (any(is.na(x)) || any(is.na(y)) || z_has_na) {
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
    pearson          = compute_pearson(x, y, alternative = alternative, p_value = p_value, ...),
    spearman         = compute_spearman(x, y, alternative = alternative, p_value = p_value, ...),
    kendall          = compute_kendall(x, y, alternative = alternative, p_value = p_value, ...),
    dcor             = compute_dcor(x, y, alternative = alternative, p_value = p_value, ...),
    mic              = compute_mic(x, y, alternative = alternative, p_value = p_value, ...),
    hsic             = compute_hsic(x, y, alternative = alternative, p_value = p_value, ...),
    xi               = compute_xi(x, y, alternative = alternative, p_value = p_value, ...),
    hoeffding        = compute_hoeffding(x, y, alternative = alternative, p_value = p_value, ...),
    mutual_info      = compute_mutual_info(x, y, alternative = alternative, p_value = p_value, ...),
    biweight         = compute_biweight(x, y, alternative = alternative, p_value = p_value, ...),
    percentage_bend  = compute_percentage_bend(x, y, alternative = alternative, p_value = p_value, ...),
    winsorized       = compute_winsorized(x, y, alternative = alternative, p_value = p_value, ...),
    polychoric       = compute_polychoric(x, y, alternative = alternative, p_value = p_value, ...),
    tetrachoric      = compute_tetrachoric(x, y, alternative = alternative, p_value = p_value, ...),
    partial          = compute_partial(x, y, z = z, method_partial = method_partial, alternative = alternative, p_value = p_value, ...),
    semi_partial     = compute_semi_partial(x, y, z = z, method_partial = method_partial, alternative = alternative, p_value = p_value, ...),
    ball             = compute_ball(x, y, alternative = alternative, p_value = p_value, ...),
    tau_star         = compute_tau_star(x, y, alternative = alternative, p_value = p_value, ...)
  )
}

#' Compute correlation matrix for all variable pairs
#'
#' @param mat Numeric matrix.
#' @param z Numeric vector, matrix, or data.frame representing control variables.
#' @param method Character: correlation method.
#' @param use Character: missing value treatment.
#' @param alternative Character: alternative hypothesis.
#' @param p_value Logical: whether to compute p-values.
#' @param call The original function call.
#' @param method_partial Character: correlation method to use for partial/semi-partial.
#' @param ... Additional arguments.
#' @return A moderncor object for matrix input.
#' @keywords internal
compute_matrix <- function(mat, z = NULL, method, use, alternative, p_value, call, method_partial = "pearson", ...) {
  n_cols <- ncol(mat)
  col_names <- colnames(mat)
  if (is.null(col_names)) {
    col_names <- paste0("V", seq_len(n_cols))
  }
  
  est_mat <- matrix(NA_real_, nrow = n_cols, ncol = n_cols, dimnames = list(col_names, col_names))
  # For normalized correlation measures [-1,1] or [0,1], self-correlation is 1.0
  # For unnormalized measures (HSIC, mutual_info), diagonal is not necessarily 1.0
  normalized_methods <- c("pearson", "spearman", "kendall", "dcor", "mic", "xi", "hoeffding",
                          "biweight", "percentage_bend", "winsorized", "polychoric", "tetrachoric",
                          "partial", "semi_partial", "ball", "tau_star")
  if (method %in% normalized_methods) {
    diag(est_mat) <- 1.0
  }
  
  pval_mat <- NULL
  if (p_value) {
    pval_mat <- matrix(NA_real_, nrow = n_cols, ncol = n_cols, dimnames = list(col_names, col_names))
    diag(pval_mat) <- 0.0
  }
  
  # For symmetric measures, we only compute the upper triangle.
  # Chatterjee's Xi is asymmetric!
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
        if (!is.null(z)) {
          z_complete <- if (is.matrix(z)) stats::complete.cases(z) else !is.na(z)
          complete_idx <- !is.na(col_i) & !is.na(col_j) & z_complete
          col_i <- col_i[complete_idx]
          col_j <- col_j[complete_idx]
          col_z <- if (is.matrix(z)) z[complete_idx, , drop = FALSE] else z[complete_idx]
        } else {
          complete_idx <- !is.na(col_i) & !is.na(col_j)
          col_i <- col_i[complete_idx]
          col_j <- col_j[complete_idx]
          col_z <- NULL
        }
      } else {
        col_z <- z
      }
      
      res <- compute_pair(col_i, col_j, z = col_z, method = method, alternative = alternative, p_value = p_value, method_partial = method_partial, ...)
      
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
