#' Compute classical and modern correlation coefficients
#'
#' This function provides a single unified interface to compute a wide range of
#' classical and modern correlation and association measures.
#'
#' @param x A numeric vector, matrix, or data.frame.
#' @param y A numeric vector, or \code{NULL} if \code{x} is a matrix or data.frame.
#' @param z A numeric vector, matrix, or data.frame representing control variables. Required for partial and semi-partial correlations.
#' @param method Character: the association method to compute. Must be one of:
#'   \itemize{
#'     \item \code{"pearson"}: Pearson product-moment correlation (linear).
#'     \item \code{"spearman"}: Spearman rank correlation (monotonic).
#'     \item \code{"kendall"}: Kendall rank correlation (monotonic).
#'     \item \code{"dcor"}: Distance correlation (general dependence).
#'     \item \code{"mic"}: Maximal Information Coefficient (general dependence).
#'     \item \code{"hsic"}: Hilbert-Schmidt Independence Criterion (general dependence).
#'     \item \code{"xi"}: Chatterjee's Xi correlation (functional dependence).
#'     \item \code{"hoeffding"}: Hoeffding's D statistic (general dependence).
#'     \item \code{"mutual_info"}: Mutual Information (information-theoretic dependence).
#'     \item \code{"biweight"}: Biweight midcorrelation (robust).
#'     \item \code{"percentage_bend"}: Percentage bend correlation (robust).
#'     \item \code{"winsorized"}: Winsorized correlation (robust).
#'     \item \code{"polychoric"}: Polychoric correlation (ordinal).
#'     \item \code{"tetrachoric"}: Tetrachoric correlation (ordinal).
#'     \item \code{"partial"}: Partial correlation controlling for variables in \code{z}.
#'     \item \code{"semi_partial"}: Semi-partial correlation controlling for variables in \code{z}.
#'     \item \code{"ball"}: Ball correlation (general dependence).
#'     \item \code{"tau_star"}: Bergsma-Dassios Tau* (general dependence).
#'   }
#' @param alternative Character: alternative hypothesis. Must be one of
#'   \code{"two.sided"}, \code{"less"}, or \code{"greater"}. Note that this is
#'   only supported for classic methods (Pearson, Spearman, Kendall). For modern
#'   and general dependence measures, it is ignored with a warning.
#' @param p_value Logical: whether to compute the p-value. Default is \code{TRUE}.
#'   For some modern methods (e.g. MIC, HSIC, Mutual Information), computing p-values
#'   can be slow because they rely on permutation tests. Set to \code{FALSE} for
#'   fast computation of estimates only.
#' @param use Character: how to handle missing values. Must be one of:
#'   \itemize{
#'     \item \code{"complete.obs"}: Remove observations with missing values (default).
#'     \item \code{"everything"}: Keep missing values (results in \code{NA} if present).
#'     \item \code{"pairwise.complete.obs"}: Compute correlations pairwise using all
#'           complete observations for each pair (only applicable for matrix/data.frame inputs).
#'   }
#' @param method_partial Character: correlation method to use for partial/semi-partial.
#'   Must be one of \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#' @param ... Additional arguments passed to the underlying compute functions.
#'   For example, \code{B} for the number of permutations in MIC or Mutual Information,
#'   or \code{R} for distance correlation.
#'
#' @return An object of class \code{"moderncor"}.
#'
#' @details
#' Most methods delegate both the estimate and the p-value to the original
#' implementing package. The biweight midcorrelation is an exception: because no
#' CRAN package provides it (the reference implementation lives in the
#' Bioconductor-only \pkg{WGCNA} package), \code{moderncor} computes the estimate
#' from the standard formula (Wilcox 2012) and approximates its p-value with a
#' Student's t statistic, \eqn{t = r\sqrt{(n - 2) / (1 - r^2)}} on \eqn{n - 2}
#' degrees of freedom (the same approximation used by \code{WGCNA::bicorAndPvalue}).
#' This p-value is therefore approximate and should be interpreted with care for
#' small samples or heavily contaminated data.
#'
#' @references
#' Wilcox, R. R. (2012). \emph{Introduction to Robust Estimation and Hypothesis
#' Testing} (3rd ed.). Academic Press.
#'
#' @export
#' @examples
#' # Generate some non-linear data (parabolic relationship)
#' set.seed(123)
#' x <- runif(100, -1, 1)
#' y <- x^2 + rnorm(100, sd = 0.1)
#'
#' # Pearson correlation (close to 0 due to non-linearity)
#' moderncor(x, y, method = "pearson")
#'
#' # Distance correlation (captures non-linear association)
#' moderncor(x, y, method = "dcor")
#'
#' # Chatterjee's Xi correlation
#' moderncor(x, y, method = "xi")
#'
#' # Compute correlation matrix for iris dataset (first 4 columns)
#' moderncor(iris[, 1:4], method = "pearson")
moderncor <- function(x, y = NULL, z = NULL,
                      method = c("pearson", "spearman", "kendall",
                                 "dcor", "mic", "hsic", "xi",
                                 "hoeffding", "mutual_info",
                                 "biweight", "percentage_bend", "winsorized",
                                 "polychoric", "tetrachoric",
                                 "partial", "semi_partial",
                                 "ball", "tau_star"),
                      alternative = c("two.sided", "less", "greater"),
                      p_value = TRUE,
                      use = c("complete.obs", "everything",
                              "pairwise.complete.obs"),
                      method_partial = c("pearson", "spearman", "kendall"),
                      ...) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  use <- match.arg(use)
  method_partial <- match.arg(method_partial)
  call <- match.call()
  
  # Validate and normalize input
  input <- validate_input(x, y, z, method, use)
  
  # Compute matrix of pairwise correlations
  if (input$type == "matrix") {
    return(compute_matrix(input$data, input$z, method, use, alternative, p_value, call, method_partial = method_partial, ...))
  }
  
  # Compute single pair correlation
  result <- compute_pair(input$x, input$y, input$z, method, alternative, p_value, method_partial = method_partial, ...)
  
  structure(
    c(result, list(n = length(input$x), call = call)),
    class = "moderncor"
  )
}
