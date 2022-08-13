2022-08-14

<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="vignettes/img/logo_huh.svg" align="right" width="20%"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# huh

## What It Does

R objects can be … “shady”. There is a large variety of objects. Though
they may look similar the difference can make their handling quite
difficult. For newbies it can be quite confusing, even intimidating.

The “huh?” package wants to describe objects to make it easier to
understand what they contain and how to handle them.

## Example

``` r
library(huh)
huh(1:3)
#> name       : 1:3 
#> type       : integer 
#> class      : integer 
#> mode       : numeric 
#> dimensions : 1 
#> paradigm   : implicit
xy <- matrix(1:4, 2)
huh(xy)
#> name       : xy 
#> type       : integer 
#> class      : matrix array 
#> mode       : numeric 
#> dimensions : 2 
#> paradigm   : implicit
huh(mean)
#> name       : mean 
#> type       : closure 
#> class      : function 
#> mode       : function 
#> dimensions : 0 
#> paradigm   : implicit
```

## Installation

You can install the development version of huh from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SigurdJanson/huh")
```

<!-- `huh` distinguishes different OOP paradigms with the field `paradigm`. -->
