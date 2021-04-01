#' Extract urban area data of MODIS Landcover
#'
#' A function that extract a time series of the urban area of MODIS Landcover
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
#' \dontrun{
#'
#' library(lis)
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#' region <- import_db("Peru_shp")[1,1]
#' data <- get_climate(year = 2009, region = region)
#'
#' }
# Function for extract urban areas

get_urban <- function(year, region) {
  suppressWarnings({
    roi <- region %>%
      st_transform(crs = 4326) %>%
      st_simplify(
        preserveTopology = TRUE,
        dTolerance = 0.001
      ) %>%
      sf_as_ee()
  })
  img_modis <- ee$ImageCollection("MODIS/006/MCD12Q1")$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    select("LC_Type2")$
    map(function(img) img$eq(list(13)))$
    mean()
  area <- img_modis$multiply(ee$Image$pixelArea())$
    divide(100000)$
    rename(sprintf("%s%s", "Aurban", year))

  data <- ee_sum(
    x = area,
    y = roi
  )
  return(data)
}
