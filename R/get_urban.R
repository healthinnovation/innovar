#' Extract urban area data of MODIS Landcover
#'
#' A function that extract a time series of the urban area of MODIS Landcover
#'
#' @param to,from is a string object,starting and final date.
#' @param region is a feature or feature collection
#'
#' @return  a tibble object with the new variables
#' @export
#' @importFrom  sf st_transform st_simplify
#' @importFrom  rgee sf_as_ee
#' @importFrom dplyr select filter
#' @examples
#' \dontrun{
#'
#' library(lis)
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#' region <- import_db("Peru_shp")
#' region_ee <- pol_as_ee(region, id = 'distr' ,simplify = 1000)
#' data <- get_climate(year = 2009, region = region)
#'
#' }
# Function for extract urban areas

get_urban <- function(to, from , region) {

  # Conditions about the times
  range <- unique(c(to:from)) %>% list()
  list_year <- ee$List(range)

  # Message of error
  if (to < 2001  | from > 2019) {
    print(sprintf("No exist data of urban area"))
  }

  list_urban <-
    list_year$
    map(
      ee_utils_pyfunc(
        function(x) {
          ee$ImageCollection("MODIS/006/MCD12Q1")$
            select(c('LC_Type2'))$
            filter(
              ee$Filter$calendarRange(
                x,
                x,
                "year")
              )$
            map(function(img) img$eq(list(13)))$
            mean()$
            multiply(
              ee$Image$pixelArea())$
            divide(100000)$
            rename('urban')
        }
      )
    )

  urban_img <- ee$ImageCollection$
    fromImages(list_urban)$
    toBands()$
    clip(region)

  data <-
    ee_sum(
      x = urban_img,
      y = region
      )

  return(data)
}
