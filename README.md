
# morphotype

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/485943302.svg)](https://zenodo.org/badge/latestdoi/485943302)
<!-- badges: end -->

The goal of morphotype is to provide a pipeline from shape data
extraction to typological arrangement of pottery profiles.

## Installation

You can install the development version of morphotype from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kafetzakid/morphotype")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(morphotype)
drawing_img = magick::image_read('~/myRpacks/morphotype/inst/extdata/SADR010324.jpg')
print(drawing_img)
#> # A tibble: 1 × 7
#>   format width height colorspace matte filesize density
#>   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
#> 1 JPEG    1074    467 sRGB       FALSE    28592 300x300
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
img_data = morphotype::get_input(filename = 'SADR010324.jpg', trim = 10, thr = 0.9, wd = '~/myRpacks/morphotype/inst/extdata')
```
