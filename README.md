
<!-- README.md is generated from README.Rmd. Please edit that file -->

# farsdata

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/Noob-Programr/farsdata.svg?branch=master)](https://travis-ci.com/Noob-Programr/farsdata)
<!-- badges: end -->

The goal of farsdata is to provide functions that will be using data
from the US National Highway Traffic Safety Administration’s Fatality
Analysis Reporting System, which is a nationwide census providing the
American public yearly data regarding fatal injuries suffered in motor
vehicle traffic crashes, to plot the data on a US Map and summarize it
as well. The package uses datasets on accidents recorded in the years
2013, 2014 and 2015.

## Installation

You can install the released version of farsdata from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("farsdata")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(farsdata)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
