# Remove accents and hyphens in character vector

This function will allow you to remove accents, hyphens and any strange
characters that come from the UTF-8 encoding, with the intention of
working with standardized names or texts.Review:
https://cran.r-project.org/web/packages/MazamaSpatialUtils/vignettes/MazamaSpatialUtils.html

## Usage

``` r
allpautils_names(string)
```

## Arguments

- string:

  Characters vector

## Examples

``` r
library(innovar)
string <- c("Perú", "Estados Unidos")
allpautils_names(string)
#> [1] "Peru"           "Estados Unidos"
```
