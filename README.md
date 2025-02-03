
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->

# authoritative

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit)
[![R-CMD-check](https://github.com/epiverse-connect/authoritative/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-connect/authoritative/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-connect/authoritative/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-connect/authoritative?branch=main)
[![lifecycle-concept](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-concept.svg)](https://www.reconverse.org/lifecycle.html#concept)
<!-- badges: end -->

authoritative provides functions to clean authors names, with a focus on
the context of R package authors.

## Installation

You can install the stable version of this package from CRAN:

``` r
install.packages("authoritative")
```

or the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("epiverse-connect/authoritative")
```

## Example

These examples illustrate some of the current functionalities:

``` r
library(authoritative)

pkg_description <- system.file("DESCRIPTION", package = "authoritative")
authors_r_pkg <- read.dcf(pkg_description, "Authors@R")
parse_authors_r(authors_r_pkg)
#> [[1]]
#> [1] "Hugo Gruson <hugo@data.org> [aut, cre, cph] (<https://orcid.org/0000-0002-4094-1476>)"
```

## Development

### Lifecycle

This package is currently a *concept*, as defined by the [RECON software
lifecycle](https://www.reconverse.org/lifecycle.html). This means that
essential features and mechanisms are still being developed, and the
package is not ready for use outside of the development team.

### Contributions

Contributions are welcome via [pull
requests](https://github.com/epiverse-connect/authoritative/pulls).

### Related projects

This project is related to other existing projects in R or other
languages, but also differs from them in the following aspects:

### Code of Conduct

Please note that the authoritative project is released with a
[Contributor Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
