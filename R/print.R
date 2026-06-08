#' Print a moderncor object
#'
#' @param x An object of class "moderncor".
#' @param digits Integer: number of decimal places to print.
#' @param ... Additional arguments.
#' @return The input object invisibly.
#' @export
print.moderncor <- function(x, digits = 4, ...) {
  cat("\n  ", x$method_label, "\n\n")
  
  if (is.matrix(x$estimate)) {
    cat("  Correlation Matrix (n = ", x$n, "):\n\n", sep = "")
    print(round(x$estimate, digits))
    
    if (!is.null(x$p.value)) {
      cat("\n  P-value Matrix:\n\n")
      print(round(x$p.value, digits))
    }
  } else {
    cat("  Estimate:  ", round(x$estimate, digits), "\n", sep = "")
    
    if (!is.null(x$statistic)) {
      cat("  Statistic: ", round(x$statistic, digits), "\n", sep = "")
    }
    
    if (!is.null(x$p.value) && !is.na(x$p.value)) {
      if (x$p.value < 2.2e-16) {
        cat("  P-value:   < 2.2e-16\n")
      } else {
        cat("  P-value:   ", format.pval(x$p.value, digits = digits), "\n", sep = "")
      }
    }
    
    cat("  Sample size (n): ", x$n, "\n", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' Summarize a moderncor object
#'
#' @param object An object of class "moderncor".
#' @param ... Additional arguments.
#' @return An object of class "summary.moderncor".
#' @export
summary.moderncor <- function(object, ...) {
  structure(object, class = "summary.moderncor")
}

#' Print a summary.moderncor object
#'
#' @param x An object of class "summary.moderncor".
#' @param digits Integer: number of decimal places to print.
#' @param ... Additional arguments.
#' @return The input object invisibly.
#' @export
print.summary.moderncor <- function(x, digits = 4, ...) {
  cat("\n========================================\n")
  cat("  moderncor Summary\n")
  cat("========================================\n")
  cat("Method:      ", x$method_label, " (", x$method, ")\n", sep = "")
  cat("Call:        ")
  print(x$call)
  cat("Sample Size: ", x$n, "\n", sep = "")
  
  if (is.matrix(x$estimate)) {
    cat("\nCorrelation Estimates:\n")
    print(round(x$estimate, digits))
    
    if (!is.null(x$p.value)) {
      cat("\nAssociated P-values:\n")
      print(round(x$p.value, digits))
    }
  } else {
    cat("Estimate:    ", round(x$estimate, digits), "\n", sep = "")
    if (!is.null(x$statistic)) {
      cat("Statistic:   ", round(x$statistic, digits), "\n", sep = "")
    }
    if (!is.null(x$p.value)) {
      cat("P-value:     ", format.pval(x$p.value, digits = digits), "\n", sep = "")
    }
  }
  cat("========================================\n\n")
  invisible(x)
}

#' Convert moderncor object to a data.frame
#'
#' @param x An object of class "moderncor".
#' @param row.names Ignored.
#' @param optional Ignored.
#' @param ... Additional arguments.
#' @return A data.frame.
#' @export
as.data.frame.moderncor <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (is.matrix(x$estimate)) {
    mat <- x$estimate
    col_names <- colnames(mat)
    
    # Generate all pairs of variables
    grid <- expand.grid(var1 = col_names, var2 = col_names, stringsAsFactors = FALSE)
    # Keep only off-diagonal entries
    grid <- grid[grid$var1 != grid$var2, ]
    
    r_vals <- sapply(seq_len(nrow(grid)), function(i) {
      mat[grid$var1[i], grid$var2[i]]
    })
    
    df <- data.frame(
      var1 = grid$var1,
      var2 = grid$var2,
      r = r_vals,
      stringsAsFactors = FALSE
    )
    
    if (!is.null(x$p.value)) {
      p_mat <- x$p.value
      p_vals <- sapply(seq_len(nrow(grid)), function(i) {
        p_mat[grid$var1[i], grid$var2[i]]
      })
      df$p.value <- p_vals
    }
    
    rownames(df) <- NULL
    df
  } else {
    df <- data.frame(
      method = x$method,
      estimate = x$estimate,
      statistic = if (is.null(x$statistic)) NA_real_ else x$statistic,
      p.value = if (is.null(x$p.value)) NA_real_ else x$p.value,
      n = x$n,
      stringsAsFactors = FALSE
    )
    rownames(df) <- NULL
    df
  }
}
