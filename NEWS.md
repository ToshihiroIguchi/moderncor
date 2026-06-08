# moderncor 0.1.0

- Initial release of the `moderncor` package.
- Provided a single entry point `moderncor()` to compute 9 classical and modern association measures.
- Standardized S3 class `"moderncor"` with printing, summary, and `as.data.frame` methods.
- Supported fast execution by allowing users to toggle p-value calculations (`p_value = FALSE`).
- Implemented utility functions `available_methods()` and `method_info()` to explore supported algorithms.
- Configured graceful dependency handling for suggested packages.
