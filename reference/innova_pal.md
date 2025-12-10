# Return function to interpolate a lis color palette

Return function to interpolate a lis color palette

## Usage

``` r
innova_pal(palette = "ccvi", reverse = FALSE, ...)
```

## Arguments

- palette:

  Character name of palette in lis_palettes

- reverse:

  Boolean indicating whether the palette should be reversed

- ...:

  Additional arguments to pass to colorRampPalette()

## Examples

``` r
if (FALSE) { # \dontrun{
library(innovar)
library(scales)
pal <- innova_pal("ccvi")(9)
show_col(pal)
} # }
```
