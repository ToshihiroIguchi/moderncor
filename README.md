# moderncor: Unified Interface for Modern and Classical Correlation Coefficients

`moderncor` is an R package that provides a single, unified interface to compute a wide variety of classical and modern correlation and association measures. Instead of remembering different package names, function calls, and argument signatures, you only need `moderncor()` (continuous) or `moderncor_cat()` (categorical).

## Features

- **Single Entry Point**: Compute 18 continuous association measures with `moderncor(x, y, method = "...")`.
- **Categorical Association**: Compute 6 categorical measures with `moderncor_cat(x, y, method = "...")`.
- **Matrix Input**: Compute full pairwise correlation matrices from data frames or matrices.
- **Tidy Integration**: Convert results to tidy data frames with `as.data.frame()`.
- **Partial Correlations**: Control for confounders via the `z` parameter.
- **Fast Execution**: Toggle p-value computation with `p_value = FALSE`.
- **Rich Output**: Standardized S3 class with estimate, p-value, test statistic, and sample size.

## Supported Methods

### Continuous (`moderncor`)

| Category | Method Name | `method` key | Package |
|---|---|---|---|
| Classic | Pearson Product-Moment | `"pearson"` | `stats` |
| Classic | Spearman Rank | `"spearman"` | `stats` |
| Classic | Kendall Rank | `"kendall"` | `stats` |
| Modern | Distance Correlation | `"dcor"` | `energy` |
| Modern | Maximal Information Coefficient | `"mic"` | `minerva` |
| Modern | Hilbert-Schmidt Independence Criterion | `"hsic"` | `dHSIC` |
| Modern | Chatterjee's Xi | `"xi"` | `XICOR` |
| Modern | Hoeffding's D | `"hoeffding"` | `Hmisc` |
| Modern | Mutual Information | `"mutual_info"` | `infotheo` |
| Robust | Biweight Midcorrelation | `"biweight"` | built-in |
| Robust | Percentage Bend | `"percentage_bend"` | `WRS2` |
| Robust | Winsorized Correlation | `"winsorized"` | `WRS2` |
| Ordinal | Polychoric Correlation | `"polychoric"` | `psych` |
| Ordinal | Tetrachoric Correlation | `"tetrachoric"` | `psych` |
| Partial | Partial Correlation | `"partial"` | `ppcor` |
| Partial | Semi-partial Correlation | `"semi_partial"` | `ppcor` |
| Other | Ball Correlation | `"ball"` | `Ball` |
| Other | Bergsma-Dassios Tau\* | `"tau_star"` | `TauStar` |

### Categorical (`moderncor_cat`)

| Category | Method Name | `method` key | Package |
|---|---|---|---|
| Nominal | Cramer's V | `"cramers_v"` | `DescTools` |
| Nominal | Phi Coefficient | `"phi"` | `DescTools` |
| Nominal | Contingency Coefficient | `"contingency"` | `DescTools` |
| Nominal | Tschuprow's T | `"tschuprow"` | `DescTools` |
| Ordinal | Goodman-Kruskal Gamma | `"gamma"` | `DescTools` |
| Ordinal | Somers' D | `"somers_d"` | `DescTools` |

## Installation

```r
# Install from source after cloning the repository
devtools::install()
```

Optional dependencies for specific methods:

```r
install.packages(c("minerva", "Hmisc", "dHSIC", "infotheo",
                   "WRS2", "psych", "ppcor", "Ball", "TauStar", "DescTools"))
```

## Quick Start

### Basic Usage

```r
library(moderncor)

set.seed(123)
x <- runif(100, -1, 1)
y <- x^2 + rnorm(100, sd = 0.1)  # non-linear relationship

# Pearson misses the non-linear dependency
moderncor(x, y, method = "pearson")

# Distance correlation captures it
moderncor(x, y, method = "dcor")

# Chatterjee's Xi detects functional dependence
moderncor(x, y, method = "xi")
```

### Robust Correlations

```r
x_out <- c(rnorm(50), 10)  # data with an outlier
y_out <- c(rnorm(50),  0)

# Biweight midcorrelation is resistant to outliers (no extra package needed)
moderncor(x_out, y_out, method = "biweight")
```

### Partial Correlation

```r
# Partial correlation of x and y controlling for z
z <- rnorm(100)
moderncor(x, y, z = z, method = "partial")
```

### Categorical Association

```r
# Cramer's V for two nominal variables
a <- factor(sample(c("A", "B", "C"), 100, replace = TRUE))
b <- factor(sample(c("X", "Y"), 100, replace = TRUE))
moderncor_cat(a, b, method = "cramers_v")

# Goodman-Kruskal Gamma for two ordinal variables
ord1 <- factor(sample(1:4, 100, replace = TRUE), ordered = TRUE)
ord2 <- factor(sample(1:4, 100, replace = TRUE), ordered = TRUE)
moderncor_cat(ord1, ord2, method = "gamma")
```

### Pairwise Correlation Matrix

```r
# Distance correlation matrix for the iris dataset
res_mat <- moderncor(iris[, 1:4], method = "dcor")
res_mat

# Convert to tidy data frame
as.data.frame(res_mat)
```

## Helper Functions

```r
# List all continuous methods
available_methods()

# List all categorical methods
available_methods_cat()

# Get details on a specific method
method_info("dcor")
```

## License

GPL-3 (required by GPL-3 licensed dependencies).
