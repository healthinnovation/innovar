# Color scale constructor for lis colors

Color scale constructor for lis colors

## Usage

``` r
scale_color_innova(palette = "ccvi", discrete = TRUE, reverse = FALSE, ...)
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
  scale_color_gradientn(), used respectively when discrete is TRUE or
  FALSE

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
library(innovar)
# Default discrete palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
geom_point(size = 4) +
scale_color_innova()

# Default continuous palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
geom_point(size = 4, alpha = .6) +
scale_color_innova(discrete = FALSE)
} # }
```
