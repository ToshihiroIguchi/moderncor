# moderncor: Unified Interface for Modern and Classical Correlation Coefficients

`moderncor` is an R package that provides a single, unified interface to compute a wide variety of classical and modern correlation and association measures. Instead of remembering different package names, function calls, and argument signatures, you only need to use `moderncor()`.

## Features

- **Single Entry Point**: Compute 9 different association measures using `moderncor(x, y, method = "...")`.
- **Supports Matrix/Data Frame Input**: Computes full pairwise correlation matrices.
- **Tidy Integration**: S3 methods to convert results to tidy data frames using `as.data.frame()`.
- **Fast Execution Control**: Easily toggle p-value calculations with `p_value = FALSE` to skip slow permutation tests on large datasets.
- **Rich Output**: Standardized class structure returning estimates, p-values, test statistics, and sample sizes.

## Supported Methods

| Method Name | Key (`method`) | Package | R Function | Captures |
|---|---|---|---|---|
| Pearson Product-Moment | `"pearson"` | `stats` | `cor()` / `cor.test()` | Linear |
| Spearman Rank | `"spearman"` | `stats` | `cor()` / `cor.test()` | Monotonic |
| Kendall Rank | `"kendall"` | `stats` | `cor()` / `cor.test()` | Monotonic |
| Distance Correlation | `"dcor"` | `energy` | `dcor()` / `dcor.test()` | Linear & Non-linear |
| Maximal Information Coefficient | `"mic"` | `minerva` | `mine()` / Permutation | Linear & Non-linear |
| Hilbert-Schmidt Criterion | `"hsic"` | `dHSIC` | `dhsic()` / `dhsic.test()` | Linear & Non-linear |
| Chatterjee's Xi | `"xi"` | `XICOR` | `xicor()` | Functional |
| Hoeffding's D | `"hoeffding"` | `Hmisc` | `hoeffd()` | Linear & Non-linear |
| Mutual Information | `"mutual_info"`| `infotheo`| `mutinformation()` / Permutation | General Dependence |

## Installation

You can install `moderncor` locally from source:

```r
# After cloning the repository
devtools::install()
```

## Quick Start

### 1. Simple Vector Comparison

Generate synthetic data with a non-linear parabolic relationship ($y = x^2 + \epsilon$):

```r
library(moderncor)

set.seed(123)
x <- runif(100, -1, 1)
y <- x^2 + rnorm(100, sd = 0.1)

# Classical Pearson fails to capture this non-linear dependency:
moderncor(x, y, method = "pearson")
#> Estimate:  0.0385
#> P-value:   0.7042

# Distance Correlation captures it:
moderncor(x, y, method = "dcor")
#> Estimate:  0.4907
#> P-value:   0.005

# Chatterjee's Xi captures functional dependency:
moderncor(x, y, method = "xi")
#> Estimate:  0.2223
#> P-value:   0.003
```

### 2. Computing a Pairwise Correlation Matrix

```r
# Compute distance correlation matrix
res_mat <- moderncor(iris[, 1:4], method = "dcor")
res_mat
```

### 3. Converting to Tidy Data Frame

```r
# Convert matrix output to a tidy data.frame
df <- as.data.frame(res_mat)
head(df)
```

## Helper Functions

List all supported methods:
```r
available_methods()
```

Get details on a specific method:
```r
method_info("dcor")
```

## License

GPL-3 (due to dependencies on GPL-3 licensed packages).
