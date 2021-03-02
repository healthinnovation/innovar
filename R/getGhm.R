#' Extract Global Human Modification data
#'
#' A function that extract Global Human Modification data of the year 2016
#'
#' @param region is a sf object
#'
#' @return  a sf object with the new variables
#' @export
#'
#' @examples
#' \dontrun{
#' library(lis)
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#'
#' region <- st_read('../district.gpkg')
#' data <- getGhm(region=region)
#' }
# Function for extract Ghm

getGhm <- function(region) {
  suppressWarnings({roi <- region %>%
    st_transform(crs = 4326) %>%
    st_simplify(
      preserveTopology = TRUE,
      dTolerance = 0.001
    ) %>%
    sf_as_ee()})
  ghm <- ee$ImageCollection('CSP/HM/GlobalHumanModification')$
    sum()$
    rename(sprintf("%s%s",'gHm',2016))

  data <- extract_value_sum(
    x = ghm,
    y = roi)
  return(data)
}
