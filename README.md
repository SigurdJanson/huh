2022-09-17

<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="vignettes/img/logo_huh.svg" align="right" width="20%"/>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test
coverage](https://codecov.io/gh/SigurdJanson/huh/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SigurdJanson/huh?branch=main)
<!-- badges: end -->

Learning R can be hard. R objects can be … how may we put it …
intricate? Not only is there is a large variety of objects. That is
common for well established programming languages. The issue is rather
that sometimes things may look similar but have subtle differences. All
in all handling R objects can be quite difficult for a beginner. For
newbies it can be quite confusing, even intimidating. I know it was for
me.

# huh!?

## What It Does

The “huh?” package wants to describe objects to make it easier to
understand what they are, what they contain and how to handle them. *My
success measure: How many times do you open the manual?* If “huh!?”
reduces the number of occasions you start browsing through pages of the
manual or your favourite R textbook, in that case this package is a huge
success.

It basically offers only two functions:

1.  `huh()`
2.  `how()`

… because “Huh!?” must be easy to learn. You do not need to learn many
functions and arguments by heart. You simply ask `huh(myObject)` or
`how(myObject)` in the console and get your result.

**huh**. `huh()` describes an object. It gives you the basic **type**,
the **class**, any **attributes** including the number of dimensions,
and on what **paradigm** the object is based on. The paradigm tells you
whether the object is based on S3, S4, reference or R6 classes.

**Example**.

``` r
library(huh)
# integer vector
huh(1:3)
#> name      : 1:3
#> type      : integer
#> class     : integer
#> dimensions: 1
#> paradigm  : implicit, object

# integer matrix
xy <- matrix(1:4, 2)
huh(xy)
#> name      : xy
#> type      : integer
#> class     : matrix, array
#> dimensions: 2
#> paradigm  : implicit, object

# function (i.e. a closure to be exact)
huh(mean)
#> name    : mean
#> type    : closure
#> class   : function
#> paradigm: S3, generic
```

**how**. `how()` tells you how you can access the contents of an object,
which subsetting techniques work, and what the resulting type would be.
Also for several objects `how()` provides further information that is
useful when you use the objects.

The output groups subsetting techniques by output type so that it is
quite easy what the output of a subsetting operation looks like. A
matrix returns an atomic vector when the subset is 1-dimensional unless
you add the argument `drop=FALSE`. But since `drop=TRUE` is the default
`how()` shows you this:

``` r
#> vector 
#> y[c(...)], y[c(...), ...], y[[...]]
```

The first line is the output type. The second line shows ways to subset
the matrix that will return this type. `...` means that you can only use
a single number or string as index; `c(...)` allows using a vector of
indices.

**Example**.

``` r
library(huh)
# atomic vector
x <- 1:3
how(x)
#> Subsetting 'x'
#> 
#> integer 
#> x[c(...)], x[[...]]

# 2-dimensional access
y <- matrix(1:4, 2)
how(y)
#> Subsetting 'y'
#> 
#> integer 
#> y[c(...)], y[c(...), ...], y[[...]]
#> matrix 
#> y[c(...), c(...)]
#> The result is coerced to the lowest possible dimension. I.e. ... 
#> * 1-dim results yield atomic vectors, 
#> * 2-dim results class 'matrix' 
#> Add the argument `drop=FALSE` to keep the class

# function (...well, a closure to be exact)
how(mean) # not subsettable
#> mean is not subsettable
```

That is all you need to know.

## Installation

You can install the development version of huh from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SigurdJanson/huh")
```

## Roadmap

The two big steps that are still missing.

-   **Improved formatting** to enhance readability and bring attention
    to the essential details of the output.
-   Enhanced **ease of use** of the output. It should be possible to
    just copy/paste output to use it.
-   Finish an accompanying vignette about this package and types in R,
    in general.

Take a look at the [issues of the
repository](https://github.com/SigurdJanson/huh/issues) if you want to
know details.

If you find anything suspicious or you have an idea, just let me know. I
am just a regular guy, not an R wizard and I am just trying to be a bit
helpful, here. So don’t hesitate if you notice something.

## Further Reading

Norberg, R. (2012). [Classes and Objects in
R](https://www.r-bloggers.com/2012/10/classes-and-objects-in-r/). R
Bloggers. *last access 2022-08-20*
