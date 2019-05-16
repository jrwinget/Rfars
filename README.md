# The `Rfars` Package
========
Current status: [![Build Status](https://travis-ci.org/jrwinget/Rfars.svg?branch=master)](https://travis-ci.org/jrwinget/Rfars)

This is an assignment for the **Coursera Mastering Software Development in R: Building R Packages** course.

Installation
------------

You can install the newest version of `Rfars` with:

``` r
devtools::install_github('jrwinget/Rfars')
```

Example
-------

Load the package and read the sample dataset:

``` r
library(Rfars)

file_2013 <- make_filename(2013)
df_2013 <- fars_read(file_2013) 
dim(df_2013)
## [1] 30202    50
```

The data in this package come from the National Highway Traffic Safety Administration (NHTSA) Fatality Analysis Reporting System (FARS) data.

``` r
## fars_summarize_years(df_2013)
## # A tibble: 30,202 x 2
##    MONTH  year
##    <int> <dbl>
##  1     1  2013
##  2     1  2013
##  3     1  2013
##  4     1  2013
##  5     1  2013
##  6     1  2013
##  7     1  2013
##  8     1  2013
##  9     1  2013
## 10     1  2013
## # ... with 30,192 more rows
```

Vignettes
-------

You can refer to below articles for more infoamtion about the package.

- [`Rfars` Package Description](vignettes/package-description.Rmd)
- [`Rfars` Package Usage](vignettes/package-usage.Rmd)
