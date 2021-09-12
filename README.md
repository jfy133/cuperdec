

# cuperdec <img src='man/figures/cuperdec_logo.png' align='right' width=150/>
<!-- badges: start -->
[![R build status](https://github.com/jfy133/cuperdec/workflows/R-CMD-check/badge.svg)](https://github.com/jfy133/cuperdec/actions)
[![Codecov test coverage](https://codecov.io/gh/jfy133/cuperdec/branch/master/graph/badge.svg)](https://codecov.io/gh/jfy133/cuperdec?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4561901.svg)](https://doi.org/10.5281/zenodo.4561901)
[![CRAN status](https://www.r-pkg.org/badges/version/cuperdec)](https://CRAN.R-project.org/package=cuperdec)
<!-- badges: end -->

R package to generate 'Cumulative Percent Decay' curves, with optional 
filtering functions, for microbial taxonomic profiles.

![Example of Cumulative Percent Decay curves](https://raw.githubusercontent.com/jfy133/cuperdec/master/inst/extdata/cuperdec_example_plot.svg)

These curves aim to represent the level of 'endogenous' content of microbiome 
samples, such as ancient dental calculus, to help to identify samples with low
levels of preservation that should be discarded for downstream analysis.

## Installation

`cuperdec` is on CRAN. You can install the package with the usual command

```r
install.packages("cuperdec")
```

To install the development version for testing, you can run
the following

```r
# install.packages("devtools")
devtools::install_github("jfy133/cuperdec")
```

## Documentation

Please see `vignettes/cuperdec-intro.Rmd`.

## Citation

If you use `cuperdec`, please use the following citation:

> Fellows Yates, J. A. et al. (2021) ‘The evolution and changing ecology of the African hominid oral microbiome’, Proceedings of the National Academy of Sciences of the United States of America, 118(20), p. e2021655118. doi: [10.1073/pnas.2021655118](http://dx.doi.org/10.1073/pnas.2021655118).

## Acknowledgments

Irina Velsko (@ivelsko), Zandra Fagerness (@ZandraFagernas), and Lena Semerau 
for testing and bug reports.
