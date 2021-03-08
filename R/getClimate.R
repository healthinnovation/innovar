#' Extract climate data of TerraClimate
#'
#' A function that extract a time series of climate variables
#'
#' @param year  is date format for extract the variable
#' @param bands  is a specif variable of climate
#' @param fun   is a type of fucntion for extract statistic zonal ('mean','median','sum','count')
#' @details Name of some bands
#' \itemize{
#' \item pr: Precipitation accumulation in mm
#' \item ro: Runoff, derived using a one-dimensional soil water balance model in mm
#' \item tmmn: Minimum temperature in C°
#' \item tmmx: Maximum temperature in C°
#' }
#' @param region is a sf object
#'
#' @return  a sf object with the new variables
#' @export
#' @importFrom  sf st_transform st_simplify
#' @examples
#' \dontrun{
#' library(lis)
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#'
#' region <- st_read('../district.gpkg')
#' data <- getClimate(year =2009,bands = 'pr',region=region, fun = 'mean')
#' }
# Function for extract climate data

getClimate <- function(year,bands, region, fun = 'mean') {
  suppressWarnings({roi <- region %>%
    st_transform(crs = 4326) %>%
    st_simplify(
      preserveTopology = TRUE,
      dTolerance = 0.001
      ) %>%
    sf_as_ee()})
  temperature <- ee$ImageCollection('IDAHO_EPSCOR/TERRACLIMATE')$
    select(c(bands))$
    filter(ee$Filter$calendarRange(year,year,"year"))$
    mean()$
    multiply(0.1) %>%
    ee$Image$rename(sprintf("%s%s",'tmax',year))
  if('median' == fun){
    data <- extract_value_median(temperature, roi)
  } else if('sd' == fun){
    data <- extract_value_sd(temperature, roi)
  } else if('sum' == fun){
    data <- extract_value_sum(temperature, roi)
  } else{
    data <- extract_value_mean(temperature, roi)
  }

  return(data)
}
