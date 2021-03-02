#' Extract Evapotranspiration data of MODIS
#'
#' A function that extract a ETP time series of MODIS
#'
#' @param year  is date format for extract the variable
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
#' data <- getETP(year =2009,region=region)
#' }
# Function for extract ETP

getETP <- function(year, region) {
  suppressWarnings({roi <- region %>%
    st_transform(crs = 4326) %>%
    st_simplify(
      preserveTopology = TRUE,
      dTolerance = 0.001
    ) %>%
    sf_as_ee()})
 etp <- ee$ImageCollection("MODIS/006/MOD16A2")$select('ET')$
   filter(ee$Filter$calendarRange(year, year, "year"))$
   sum()$multiply(0.1) %>%
   ee$Image$rename(sprintf("%s%s",'etp',year))

  data <- extract_value_mean(
    etp,
    roi
    )
  return(data)
}
