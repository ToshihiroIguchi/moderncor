#' List all available correlation methods
#'
#' @return A data.frame with method names, labels, and package requirements.
#' @export
#' @examples
#' available_methods()
available_methods <- function() {
  methods_list <- list(
    list(method = "pearson", label = "Pearson Product-Moment Correlation", package = "stats", type = "classic"),
    list(method = "spearman", label = "Spearman Rank Correlation", package = "stats", type = "classic"),
    list(method = "kendall", label = "Kendall Rank Correlation", package = "stats", type = "classic"),
    list(method = "dcor", label = "Distance Correlation", package = "energy", type = "modern"),
    list(method = "mic", label = "Maximal Information Coefficient (MIC)", package = "minerva", type = "modern"),
    list(method = "hsic", label = "Hilbert-Schmidt Independence Criterion (HSIC)", package = "dHSIC", type = "modern"),
    list(method = "xi", label = "Chatterjee's Xi Correlation", package = "XICOR", type = "modern"),
    list(method = "hoeffding", label = "Hoeffding's D", package = "Hmisc", type = "modern"),
    list(method = "mutual_info", label = "Mutual Information", package = "infotheo", type = "information")
  )
  
  df <- do.call(rbind, lapply(methods_list, as.data.frame))
  rownames(df) <- NULL
  df
}

#' Get detailed information about a specific correlation method
#'
#' @param method Character: the method name.
#' @return A list with method details.
#' @export
#' @examples
#' method_info("pearson")
method_info <- function(method) {
  all_methods <- available_methods()
  
  # Normalize using match.arg to allow partial matching
  method <- match.arg(method, all_methods$method)
  
  info <- switch(method,
    pearson = list(
      method = "pearson",
      label = "Pearson Product-Moment Correlation",
      package = "stats",
      description = "Measures linear association between two continuous variables.",
      range = "[-1, 1]",
      assumptions = "Bivariate normality, linearity, homoscedasticity."
    ),
    spearman = list(
      method = "spearman",
      label = "Spearman Rank Correlation",
      package = "stats",
      description = "Measures monotonic association using ranks.",
      range = "[-1, 1]",
      assumptions = "Monotonic relationship, ordinal or continuous variables."
    ),
    kendall = list(
      method = "kendall",
      label = "Kendall Rank Correlation",
      package = "stats",
      description = "Measures monotonic association based on concordant and discordant pairs.",
      range = "[-1, 1]",
      assumptions = "Monotonic relationship, ordinal or continuous variables."
    ),
    dcor = list(
      method = "dcor",
      label = "Distance Correlation",
      package = "energy",
      description = "Measures both linear and nonlinear dependence. Zero if and only if independent.",
      range = "[0, 1]",
      assumptions = "Continuous variables."
    ),
    mic = list(
      method = "mic",
      label = "Maximal Information Coefficient (MIC)",
      package = "minerva",
      description = "Information-theoretic measure for general linear and nonlinear relationships.",
      range = "[0, 1]",
      assumptions = "Continuous variables."
    ),
    hsic = list(
      method = "hsic",
      label = "Hilbert-Schmidt Independence Criterion (HSIC)",
      package = "dHSIC",
      description = "Kernel-based independence test. Zero if and only if independent (with characteristic kernels).",
      range = "[0, Inf)",
      assumptions = "Continuous variables."
    ),
    xi = list(
      method = "xi",
      label = "Chatterjee's Xi Correlation",
      package = "XICOR",
      description = "Measures functional dependence. Zero implies independence, one implies Y is a function of X.",
      range = "[0, 1]",
      assumptions = "Continuous variables, no ties (or handled asymptotically)."
    ),
    hoeffding = list(
      method = "hoeffding",
      label = "Hoeffding's D",
      package = "Hmisc",
      description = "Measures general dependence, including non-monotonic (U-shaped, circular) relationships.",
      range = "[-0.5, 1]",
      assumptions = "Continuous variables."
    ),
    mutual_info = list(
      method = "mutual_info",
      label = "Mutual Information",
      package = "infotheo",
      description = "Information-theoretic measure of the mutual dependence between two variables after discretization.",
      range = "[0, Inf)",
      assumptions = "Continuous variables (discretized) or categorical."
    )
  )
  
  info
}
