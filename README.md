<!-- README.md is generated from README.Rmd. Please edit that file -->
fispdfparsr
===========

fispdfparsr is a collection of utilities for parsing PDF cross-country race results from [FIS](http://data.fis-ski.com). The focus initially is results from major events: World Cup, World Championships and Olympics.

It has seen extremely limited testing and will surely break on some PDFs, since FIS likes to monkey with their formatting pretty regularly. If you find a PDF where it breaks, please file an issue.

Installation
------------

fispdfparsr uses the [tabulizer](https://github.com/ropenscilabs/tabulizer) package to do the acutal PDF parsing, which is currently available only on GitHub, not CRAN. Follow the installation instructions there first, which may involved installing some Java dependencies.

Once you have installed tabulizer, you can install fispdfparsr via:

``` r
install.packages("devtools")
devtools::install_github("joranE/fispdfparsr")
```
