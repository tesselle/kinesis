
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kinesis

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/kinesis/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/kinesis/actions)
[![codecov](https://codecov.io/gh/tesselle/kinesis/branch/master/graph/badge.svg)](https://codecov.io/gh/tesselle/kinesis)

<a href="https://tesselle.r-universe.dev/kinesis"
class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/kinesis"
alt="r-universe" /></a>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

A collection of [**shiny**](https://shiny.rstudio.com) applications that
provides graphical user interfaces for the
[tesselle](https://www.tesselle.org) packages.

    To cite kinesis in publications use:

      Frerebeau N (2023). _kinesis: 'Shiny' Applications for the 'tesselle'
      Packages_. Université Bordeaux Montaigne, Pessac, France. R package
      version 0.0.0.9003, <https://packages.tesselle.org/kinesis/>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        author = {Nicolas Frerebeau},
        title = {{kinesis: 'Shiny' Applications for the 'tesselle' Packages}},
        year = {2023},
        organization = {Université Bordeaux Montaigne},
        address = {Pessac, France},
        note = {R package version 0.0.0.9003},
        url = {https://packages.tesselle.org/kinesis/},
      }

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
install.packages("kinesis", repos = "https://tesselle.r-universe.dev")
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
run_app("kairos")
```

## Contributing

Please note that the **kinesis** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.
