#' Extract Evapotranspiration data of MODIS
#'
#' A function that extract a ETP time series of MODIS
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
#' region <- import_db("Peru_shp")[1, 1]
#' data <- get_climate(year = 2009, region = region, fun = "mean")
#'
#' }
#'
# Function for extract ETP
get_etp <- function(year, region) {
  suppressWarnings({
    roi <- region %>%
      st_transform(crs = 4326) %>%
      st_simplify(
        preserveTopology = TRUE,
        dTolerance = 0.001
      ) %>%
      sf_as_ee()
  })
  etp <- ee$ImageCollection("MODIS/006/MOD16A2")$select("ET")$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    sum()$multiply(0.1) %>%
    ee$Image$rename(sprintf("%s%s", "etp", year))

  data <- ee_mean(
    etp,
    roi
  )
  return(data)
}
