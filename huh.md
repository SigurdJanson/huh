huh?
================
2022-08-08

<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## What It Does

R objects can be … “shady”. There is a large variety of objects. Though
they may look similar the difference can make their handling quite
difficult.

The “huh?” package wants to describe objects to make it easier to
understand what they contain and how to handle them.

## HUH?

``` r
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

<!-- `huh` distinguishes different OOP paradigms with the field `paradigm`. -->
