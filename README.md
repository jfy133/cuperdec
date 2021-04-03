

# cuperdec <img src='man/figures/cuperdec_logo.png' align='right' width=150/>
<!-- badges: start -->
[![R build status](https://github.com/jfy133/cuperdec/workflows/R-CMD-check/badge.svg)](https://github.com/jfy133/cuperdec/actions)
[![Codecov test coverage](https://codecov.io/gh/jfy133/cuperdec/branch/master/graph/badge.svg)](https://codecov.io/gh/jfy133/cuperdec?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4561901.svg)](https://doi.org/10.5281/zenodo.4561901)
[![CRAN status](https://www.r-pkg.org/badges/version/cuperdec)](https://CRAN.R-project.org/package=cuperdec)
<!-- badges: end -->

R package to generate 'Cumulative Percent Decay' curves, with optional 
filtering functions, for microbial taxonomic profiles.

![Example of Cumulative Percent Decay curves](inst/extdata/cuperdec_example_plot.svg)

These curves aim to represent the level of 'endogenous' content of microbiome 
samples, such as ancient dental calculus, to help to identify samples with low
levels of preservation that should be discarded for downstream analysis.

## Installation

This package is still in development. To install for testing, you can run
the following

```r
# install.packages("devtools")
devtools::install_github("jfy133/cuperdec")
```

## Documentation

Please see `vignettes/cuperdec-intro.Rmd`.

## Acknowledgments

Irina Velsko (@ivelsko), Zandra Fagerness (@ZandraFagernas), and Lena Semerau 
for testing and bug reports.
