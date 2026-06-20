## Test environments

- Local: R 4.6.0, Windows 11 (x86_64-w64-mingw32)
- win-builder: R-devel, R-release
- R-hub: ubuntu-latest, macos-latest

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Toshihiro Iguchi <toshihiro.iguchi.github@gmail.com>'

  New submission

  Possibly misspelled words in DESCRIPTION:
    Bergsma, Chatterjee's, Cramer's, Dassios, Hoeffding's, Somers,
    Tschuprow's, Winsorized, midcorrelation, polychoric, tetrachoric

  These are all spelled correctly. They are the surnames of statisticians
  (Bergsma, Dassios, Hoeffding, Somers, Tschuprow, Chatterjee, Cramer) and
  standard statistical terminology (polychoric, tetrachoric, midcorrelation,
  Winsorized) referring to the association measures implemented by the package.

Local `R CMD check` (without the CRAN incoming feasibility test) returns
0 errors | 0 warnings | 0 notes.

## Notes on dependencies

- All packages listed in `Suggests` are available on CRAN.
- No Bioconductor dependencies. The `biweight` method was originally
  implemented via WGCNA (Bioconductor); it has been self-implemented
  using the standard Wilcox (2012) formula to avoid any Bioconductor
  dependency.

## Downstream dependencies

This is a new package. There are no downstream reverse dependencies.
