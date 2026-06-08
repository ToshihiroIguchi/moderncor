#' Compute correlation/association coefficients for categorical variables
#'
#' @param x A factor vector, character vector, numeric vector (treated as categorical), data.frame, or matrix.
#' @param y A factor vector, character vector, numeric vector (treated as categorical), or NULL.
#' @param method Character: the categorical association method to compute. Must be one of:
#'   \itemize{
#'     \item \code{"cramers_v"}: Cramer's V.
#'     \item \code{"phi"}: Phi Coefficient.
#'     \item \code{"gamma"}: Goodman-Kruskal Gamma (for ordinal factors).
#'     \item \code{"somers_d"}: Somers' D (for ordinal factors).
#'     \item \code{"contingency"}: Contingency Coefficient.
#'     \item \code{"tschuprow"}: Tschuprow's T.
#'   }
#' @param use Character: how to handle missing values. Must be one of:
#'   \code{"complete.obs"}, \code{"everything"}, or \code{"pairwise.complete.obs"}.
#' @param ... Additional arguments passed to the underlying compute functions.
#'
#' @return An object of class \code{"moderncor_cat"}.
#'
#' @export
moderncor_cat <- function(x, y = NULL,
                          method = c("cramers_v", "phi", "gamma", "somers_d",
                                     "contingency", "tschuprow"),
                          use = c("complete.obs", "everything",
                                  "pairwise.complete.obs"),
                          ...) {
  method <- match.arg(method)
  use <- match.arg(use)
  call <- match.call()
  
  # Validate and normalize input
  input <- validate_input_cat(x, y, use)
  
  # Compute matrix of pairwise correlations
  if (input$type == "matrix") {
    return(compute_matrix_cat(input$data, method, use, call, ...))
  }
  
  # Compute single pair correlation
  result <- compute_pair_cat(input$x, input$y, method, ...)
  
  structure(
    c(result, list(n = length(input$x), call = call)),
    class = "moderncor_cat"
  )
}

#' Validate and normalize input for moderncor_cat
#'
#' @keywords internal
validate_input_cat <- function(x, y, use) {
  # Handle data.frame or matrix input
  if (is.data.frame(x) || is.matrix(x)) {
    if (!is.null(y)) {
      stop("y must be NULL if x is a matrix or data.frame", call. = FALSE)
    }
    
    # We want columns to be treated as factors
    df <- as.data.frame(x)
    df[] <- lapply(df, as.factor)
    
    # Handle NA according to "use"
    if (use == "complete.obs") {
      df <- df[stats::complete.cases(df), , drop = FALSE]
      if (nrow(df) == 0) {
        warning("No complete observations remaining after removing NA", call. = FALSE)
      }
    }
    
    return(list(
      type = "matrix",
      data = df
    ))
  }
  
  # Handle vector input
  if (is.null(y)) {
    stop("y must be provided if x is a vector", call. = FALSE)
  }
  
  # Ensure both are converted to factors of the same length
  x_fact <- as.factor(x)
  y_fact <- as.factor(y)
  
  if (length(x_fact) != length(y_fact)) {
    stop("x and y must have the same length", call. = FALSE)
  }
  
  # Handle NA according to "use"
  if (use == "complete.obs" || use == "pairwise.complete.obs") {
    complete_idx <- !is.na(x_fact) & !is.na(y_fact)
    x_fact <- x_fact[complete_idx]
    y_fact <- y_fact[complete_idx]
  }
  
  return(list(
    type = "vector",
    x = x_fact,
    y = y_fact
  ))
}

#' Dispatch to the appropriate pair-wise compute function for categorical variables
#'
#' @keywords internal
compute_pair_cat <- function(x, y, method, ...) {
  if (any(is.na(x)) || any(is.na(y))) {
    return(list(
      estimate = NA_real_,
      method = method,
      method_label = method_info_cat(method)$label,
      statistic = NA_real_,
      p.value = NA_real_
    ))
  }
  
  if (length(x) < 3) {
    return(list(
      estimate = NA_real_,
      method = method,
      method_label = method_info_cat(method)$label,
      statistic = NA_real_,
      p.value = NA_real_
    ))
  }
  
  # Check for at least two levels in each variable
  if (nlevels(droplevels(x)) < 2 || nlevels(droplevels(y)) < 2) {
    return(list(
      estimate = NA_real_,
      method = method,
      method_label = method_info_cat(method)$label,
      statistic = NA_real_,
      p.value = NA_real_
    ))
  }
  
  check_suggested("DescTools", method)
  
  # Compute estimate
  est <- switch(method,
    cramers_v   = DescTools::CramerV(x, y, ...),
    phi         = DescTools::Phi(x, y, ...),
    gamma       = DescTools::GoodmanKruskalGamma(x, y, ...),
    somers_d    = DescTools::SomersDelta(x, y, ...),
    contingency = DescTools::ContCoef(x, y, ...),
    tschuprow   = DescTools::TschuprowT(x, y, ...)
  )
  
  # Compute p-value and statistic based on chi-square test for nominal variables
  nominal_methods <- c("cramers_v", "phi", "contingency", "tschuprow")
  
  if (method %in% nominal_methods) {
    test_res <- suppressWarnings(stats::chisq.test(x, y, correct = FALSE))
    stat <- unname(test_res$statistic)
    pval <- unname(test_res$p.value)
  } else {
    stat <- NULL
    pval <- NULL
  }
  
  list(
    estimate = est,
    method = method,
    method_label = method_info_cat(method)$label,
    statistic = stat,
    p.value = pval
  )
}

#' Compute categorical correlation matrix for all variable pairs
#'
#' @keywords internal
compute_matrix_cat <- function(df, method, use, call, ...) {
  n_cols <- ncol(df)
  col_names <- colnames(df)
  if (is.null(col_names)) {
    col_names <- paste0("V", seq_len(n_cols))
  }
  
  est_mat <- matrix(NA_real_, nrow = n_cols, ncol = n_cols, dimnames = list(col_names, col_names))
  diag(est_mat) <- 1.0
  
  pval_mat <- matrix(NA_real_, nrow = n_cols, ncol = n_cols, dimnames = list(col_names, col_names))
  diag(pval_mat) <- 0.0
  
  nominal_methods <- c("cramers_v", "phi", "contingency", "tschuprow")
  if (!(method %in% nominal_methods)) {
    pval_mat <- NULL
  }
  
  is_symmetric <- method %in% c("cramers_v", "phi", "gamma", "contingency", "tschuprow")
  
  for (i in seq_len(n_cols)) {
    for (j in seq_len(n_cols)) {
      if (i == j) next
      
      if (is_symmetric && j < i) {
        est_mat[i, j] <- est_mat[j, i]
        if (!is.null(pval_mat)) {
          pval_mat[i, j] <- pval_mat[j, i]
        }
        next
      }
      
      col_i <- df[[i]]
      col_j <- df[[j]]
      
      if (use == "pairwise.complete.obs") {
        complete_idx <- !is.na(col_i) & !is.na(col_j)
        col_i <- col_i[complete_idx]
        col_j <- col_j[complete_idx]
      }
      
      res <- compute_pair_cat(col_i, col_j, method = method, ...)
      
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
      method_label = method_info_cat(method)$label,
      p.value = pval_mat,
      n = nrow(df),
      call = call
    ),
    class = "moderncor_cat"
  )
}

#' Information about categorical association methods
#'
#' @keywords internal
method_info_cat <- function(method) {
  info <- list(
    cramers_v   = list(label = "Cramer's V"),
    phi         = list(label = "Phi Coefficient"),
    gamma       = list(label = "Goodman-Kruskal Gamma"),
    somers_d    = list(label = "Somers' D"),
    contingency = list(label = "Contingency Coefficient"),
    tschuprow   = list(label = "Tschuprow's T")
  )
  info[[method]]
}
