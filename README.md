
<!-- README.md is generated from README.Rmd. Please edit that file -->

# janus

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/janus/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/janus/actions)
[![codecov](https://codecov.io/gh/tesselle/janus/branch/master/graph/badge.svg)](https://codecov.io/gh/tesselle/janus)

<a href="https://tesselle.r-universe.dev/janus"
class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/janus"
alt="r-universe" /></a>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

A collection of [**shiny**](https://shiny.rstudio.com) application that
provides graphical user interfaces for the
[tesselle](https://www.tesselle.org) packages.

    To cite janus in publications use:

      Frerebeau N (2023). _janus: 'Shiny' Applications for the tesselle R
      Packages_. Université Bordeaux Montaigne, Pessac, France. R package
      version 0.0.0.9002, <https://packages.tesselle.org/janus/>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        author = {Nicolas Frerebeau},
        title = {{janus: 'Shiny' Applications for the tesselle R Packages}},
        year = {2023},
        organization = {Université Bordeaux Montaigne},
        address = {Pessac, France},
        note = {R package version 0.0.0.9002},
        url = {https://packages.tesselle.org/janus/},
      }

    This package is a part of the tesselle project
    <https://www.tesselle.org>.

## Installation

You can install the latest version of **janus** from [our
repository](https://tesselle.r-universe.dev) with:

``` r
install.packages("janus", repos = "https://tesselle.r-universe.dev")
```

## Usage

``` r
## Load the package
library(janus)

## Run the app for matrix seriation
run_app("kairos")
```

## Contributing

Please note that the **janus** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.
