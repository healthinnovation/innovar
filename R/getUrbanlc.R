#' Extract urban area data of MODIS Landcover
#'
#' A function that extract a time series of the urban area of MODIS Landcover
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
#' data <- getUrbanlc(year =2009,region=region)
#' }
# Function for extract urban areas

getUrbanlc <- function(year, region) {
  suppressWarnings({roi <- region %>%
    st_transform(crs = 4326) %>%
    st_simplify(
      preserveTopology = TRUE,
      dTolerance = 0.001
    ) %>%
    sf_as_ee()})
 img_modis <- ee$ImageCollection('MODIS/006/MCD12Q1')$
      filter(ee$Filter$calendarRange(year, year, "year"))$
      select('LC_Type2')$
      map(function(img)img$eq(list(13)))$
      mean()
  area <- img_modis$multiply(ee$Image$pixelArea())$
      divide(100000)$
      rename(sprintf("%s%s",'Aurban',year))

    data <- extract_value_sum(
      x = area,
      y = roi)
  return(data)
}

