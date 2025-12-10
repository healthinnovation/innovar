# Utility for dataset batch importing in multiple formats

Reference:
https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/#'

## Usage

``` r
read_batch(data_dir, ext = "csv", fun = readr::read_csv, env = TRUE, ...)
```

## Arguments

- data_dir:

  data_dir is the file path that contains the files that will be
  imported

- ext:

  extension "\*.ext" of the files that will be imported. Default "csv"

- fun:

  the import function that will be passsed to \`map\` to import
  datasets. Must be able to read files with the \`ext\` extension

- env:

  Whether the file will be imported to .GlobalEnv. If \`FALSE\` a list
  containing all the dataset will be generated

- ...:

  Arguments passed to the import function

  Import multiple files

## Examples

``` r
if (FALSE) { # \dontrun{
read_batch(data_dir = data_dir)
read_batch(data_dir = data_dir, extension = "dta", fun = haven::read_dta)
read_batch(data_dir = data_dir, extension = "dta", fun = haven::read_dta, env = F)
} # }
```
