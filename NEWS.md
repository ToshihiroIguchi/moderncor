# moderncor 0.2.0

## New Features

* Added robust correlation methods: biweight midcorrelation, percentage bend
  correlation (`WRS2`), and Winsorized correlation (`WRS2`).
* Added ordinal correlation methods: polychoric and tetrachoric correlation
  (`psych`).
* Added partial correlation methods: partial correlation and semi-partial
  correlation (`ppcor`), controlled via the new `z` parameter.
* Added nonparametric methods: ball correlation (`Ball`) and Bergsma-Dassios
  tau* (`TauStar`).
* Added `moderncor_cat()` for categorical association measures: Cramer's V,
  phi coefficient, Goodman-Kruskal gamma, Somers' D, contingency coefficient,
  and Tschuprow's T (all via `DescTools`).
* Added `available_methods_cat()` to list all supported categorical methods.

## Changes

* Removed dependency on `WGCNA` (Bioconductor). Biweight midcorrelation is
  now computed using a built-in implementation of the standard formula
  (Wilcox 2012), eliminating the need for `Additional_repositories`.
* Version bumped to 0.2.0 to reflect the substantial addition of Phase 2
  methods.

# moderncor 0.1.0

* Initial release of the `moderncor` package.
* Provided a single entry point `moderncor()` to compute 9 classical and
  modern association measures: Pearson, Spearman, Kendall, distance
  correlation, MIC, HSIC, Chatterjee's xi, Hoeffding's D, and mutual
  information.
* Standardized S3 class `"moderncor"` with printing, summary, and
  `as.data.frame` methods.
* Supported fast execution by allowing users to toggle p-value calculations
  (`p_value = FALSE`) to skip slow permutation tests.
* Implemented utility functions `available_methods()` and `method_info()` to
  explore supported algorithms.
* Configured graceful dependency handling for suggested packages via
  `check_suggested()`.
