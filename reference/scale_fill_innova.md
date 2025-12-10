# Fill scale constructor for lis colors

Fill scale constructor for lis colors

## Usage

``` r
scale_fill_innova(palette = "ccvi", discrete = TRUE, reverse = FALSE, ...)
```

## Arguments

- palette:

  Character name of palette in lis_palettes

- discrete:

  Boolean indicating whether color aesthetic is discrete or not

- reverse:

  Boolean indicating whether the palette should be reversed

- ...:

  Additional arguments passed to discrete_scale() or
  scale_fill_gradientn(), used respectively when discrete is TRUE or
  FALSE

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
library(innovar)
ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
geom_bar() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_fill_innova()
} # }
```
