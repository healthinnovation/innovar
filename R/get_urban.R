#' Extract urban area data of MODIS Landcover
#'
#' A function that extract a time series of the urban area of MODIS Landcover
#'
#' @param to,from it's a string object,starting and final date.
#' @param region it's a feature or feature collection
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
#' data <- get_climate(to = '2008-01-01', from = '2010-01-01', region = region)
#'
#' }
# Function for extract urban areas

get_urban <- function(to, from , region) {

  # Conditions about the times
  start_year <- substr(to, 1, 4) %>% as.numeric()
  end_year <- substr(from, 1, 4) %>% as.numeric()

  if(start_year == end_year){
    year <- unique(
      c(start_year:end_year)
    ) %>%
      list()

    year_list <- ee$List(year)
  } else {
    year <- unique(
      c(start_year:end_year)
    )
    year_list <- ee$List(year)
  }

  # Message of error
  if (to < 2001  | from > 2019) {
    print(sprintf("No exist data of urban area"))
  }

  list_urban <-
    year_list$
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
