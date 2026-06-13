## Test environments

- Local: R 4.6.0, Windows 11 (x86_64-w64-mingw32)
- win-builder: R-devel, R-release
- R-hub: ubuntu-latest, macos-latest

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

* checking top-level files ... NOTE
  Non-standard file/directory found at top level: '_pkgdown.yml'

  This file is the standard configuration file for the pkgdown package
  documentation site and is intentionally placed at the top level.

## Notes on dependencies

- All packages listed in `Suggests` are available on CRAN.
- No Bioconductor dependencies. The `biweight` method was originally
  implemented via WGCNA (Bioconductor); it has been self-implemented
  using the standard Wilcox (2012) formula to avoid any Bioconductor
  dependency.

## Downstream dependencies

This is a new package. There are no downstream reverse dependencies.
