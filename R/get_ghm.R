#' Extract Global Human Modification data
#'
#' A function that extract Global Human Modification data of the year 2016
#'
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
#' data <- get_climate(region = region)
#'
#' }
#  Function for extract Ghm

get_ghm <- function(region) {
  suppressWarnings({
    roi <- region %>%
      st_transform(crs = 4326) %>%
      st_simplify(
        preserveTopology = TRUE,
        dTolerance = 0.001
      ) %>%
      sf_as_ee()
  })
  ghm <- ee$ImageCollection("CSP/HM/GlobalHumanModification")$
    sum()$
    rename(sprintf("%s%s", "gHm", 2016))

  data <- extract_value_sum(
    x = ghm,
    y = roi
  )
  return(data)
}
