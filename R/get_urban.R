#' Extract urban area data of MODIS Landcover
#'
#' A function that extract a time series of the urban area of MODIS Landcover (2001-01-01T00:00:00Z - 2020-01-01T00:00:00)
#'
#' @param to,from it's a string object,starting and final date.
#' @param region it's a feature or feature collection
#' @param scale A nominal scale in meters of the projection to work in.
#'
#' @return  a tibble object with the new variable in km2
#' @export
#' @importFrom  sf st_transform st_simplify
#' @importFrom  rgee sf_as_ee
#' @importFrom dplyr select filter
#' @examples
#' \dontrun{
#'
#' library(innovar)
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#' data("Peru")
#' region <- Peru
#' region_ee <- pol_as_ee(region, id = 'distr' ,simplify = 1000)
#' data <- get_urban(from = '2008-01-01', to = '2010-01-01', region = region)
#'
#' }
# Function for extract urban areas

get_urban <- function(from, to, region, scale = 1000) {

  # Conditions about the times
  start_year <- substr(from, 1, 4) %>% as.numeric()
  end_year <- substr(to, 1, 4) %>% as.numeric()

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
      y = region,
      scale = scale
      )

  return(data)
}
