#' Extract population data of WorldPop
#'
#' A function that extract a time series of the number of population
#'
#' @param year  is date format for extract the variable
#' @param region is a sf object
#'
#' @return  a sf object with the new variables
#' @export
#' @importFrom  sf st_transform st_simplify
#' @importFrom  rgee sf_as_ee
#' @importFrom dplyr select filter
#' @examples
#' @dontrun{
#' library(lis)
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#' region <- import_db("Peru_shp")[1,1]
#' data <- get_climate(year = 2009, region = region)
#' }
# Function for extract population data

get_pop <- function(year, region) {
  suppressWarnings({
    roi <- region %>%
      st_transform(crs = 4326) %>%
      st_simplify(
        preserveTopology = TRUE,
        dTolerance = 0.001
      ) %>%
      sf_as_ee()
  })
  worldpop <- ee$ImageCollection("WorldPop/GP/100m/pop")$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    select("population")$
    mosaic()$
    multiply(10000)$
    rename(sprintf("%s%s", "pob", year))

  data <- extract_value_mean(
    x = worldpop,
    y = roi
  )
  return(data)
}
