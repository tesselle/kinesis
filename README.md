
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kinesis <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/kinesis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tesselle/kinesis/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/tesselle/kinesis/graph/badge.svg?token=i2n7T978ex)](https://codecov.io/gh/tesselle/kinesis)

<a href="https://tesselle.r-universe.dev/kinesis"
class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/kinesis"
alt="r-universe" /></a> [![Project Status: WIP – Initial development is
in progress, but there has not yet been a stable, usable release
suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

A collection of [**shiny**](https://shiny.rstudio.com) applications that
provides graphical user interfaces for the
[tesselle](https://www.tesselle.org) packages.

This package is currently *experimental*. This means that it is
functional, but interfaces and functionalities may change over time,
testing and documentation may be lacking.

------------------------------------------------------------------------

To cite kinesis in publications use:

Frerebeau N (2024). “The tesselle Project: a Collection of R Packages
for Research and Teaching in Archaeology.” *Advances in Archaeological
Practice*. <doi:10.1017/aap.2024.10>
<https://doi.org/10.1017/aap.2024.10>.

Frerebeau N (2024). *kinesis: ‘shiny’ Applications for the ‘tesselle’
Packages*. Université Bordeaux Montaigne, Pessac, France. R package
version 0.0.0.9004, <https://packages.tesselle.org/kinesis/>.

This package is a part of the tesselle project
<https://www.tesselle.org>.

## Remote use

The applications are deployed on the [Huma-Num](https://www.huma-num.fr)
Shiny server: <https://analytics.huma-num.fr/tesselle/home/>.

## Local use

### Installation

You can install the released version of **kinesis** from [our
repository](https://tesselle.r-universe.dev) with:

``` r
options(repos = c(CRAN = "https://cloud.r-project.org",
                  tesselle = "https://tesselle.r-universe.dev"))

install.packages("kinesis")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/kinesis")
```

### Usage

``` r
## Load the package
library(kinesis)

## Run the app for matrix seriation
run_app("seriation")
```

| Keyword | Application name | Example |
|:---|:---|:---|
| `ca` | Correspondence Analysis | [ceramic data](https://analytics.huma-num.fr/tesselle/ca?data=https://raw.githubusercontent.com/tesselle/folio/refs/heads/main/data-raw/zuni.csv) |
| `pca` | Principal Components Analysis |  |
| `diversity` | Diversity Measures | [ceramic data](https://analytics.huma-num.fr/tesselle/diversity?data=https://raw.githubusercontent.com/tesselle/folio/refs/heads/main/data-raw/zuni.csv) |
| `seriation` | Matrix Seriation | [ceramic data](https://analytics.huma-num.fr/tesselle/seriation?data=https://raw.githubusercontent.com/tesselle/folio/refs/heads/main/data-raw/zuni.csv) |
| `source` | Compositional Data Analysis | [bronze data](https://analytics.huma-num.fr/tesselle/source?data=https://raw.githubusercontent.com/tesselle/folio/refs/heads/main/data-raw/bronze.csv) |
| `ternary` | Ternary Plot | [bronze data](https://analytics.huma-num.fr/tesselle/ternary?data=https://raw.githubusercontent.com/tesselle/folio/refs/heads/main/data-raw/bronze.csv) |

## Contributing

Please note that the **kinesis** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.
