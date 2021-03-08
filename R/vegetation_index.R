#' Extract of vegetation index
#'
#' A function that extract the values of vegetation
#'
#' @param year  is date format for extract the variable
#' @param region is a sf object
#' @return  a sf object with the new variables
#' @export
#' @importFrom  sf st_transform st_simplify

# NDVI
get_ndvi <- function(year , region) {

  suppressWarnings({roi <- region %>%
    st_transform(crs = 4326) %>%
    st_simplify(
      preserveTopology = TRUE,
      dTolerance = 0.001
    ) %>%
    sf_as_ee()})

  ndvi <- ee$ImageCollection('MODIS/006/MOD13Q1')$
    select('NDVI')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    mean()$multiply(0.0001)%>%
    ee$Image$rename(sprintf("%s%s",'ndvi',year))

  data <- extract_value_mean(ndvi, region)
  return(data)
}


#' Extract of vegetation index
#'
#' A function that extract the values of vegetation
#'
#' @param year  is date format for extract the variable
#' @param region is a sf object
#'
#' @return  a sf object with the new variables
#' @export
#' @importFrom  sf st_transform st_simplify
# EVI
get_evi <- function(year , region) {

  suppressWarnings({roi <- region %>%
    st_transform(crs = 4326) %>%
    st_simplify(
      preserveTopology = TRUE,
      dTolerance = 0.001
    ) %>%
    sf_as_ee()})

  ndvi <- ee$ImageCollection('MODIS/006/MOD13Q1')$
    select('EVI')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    mean()$multiply(0.0001)%>%
    ee$Image$rename(sprintf("%s%s",'evi',year))

  data <- extract_value_mean(ndvi, region)
  return(data)
}

#' Extract of vegetation index
#'
#' A function that extract the values of vegetation
#'
#' @param year  is date format for extract the variable
#' @param region is a sf object
#'
#' @return  a sf object with the new variables
#' @export
#' @importFrom  sf st_transform st_simplify
# SAVI
get_savi <- function(year, region) {

  suppressWarnings({roi <- region %>%
    st_transform(crs = 4326) %>%
    st_simplify(
      preserveTopology = TRUE,
      dTolerance = 0.001
    ) %>%
    sf_as_ee()})

  bandas <- ee$ImageCollection('MODIS/006/MOD13Q1')$
    select('sur_refl_b01','sur_refl_b02')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    map(function(x) x$multiply(0.0001))$
    mean()

  savi = bandas$expression(
    '(1 + L) * float(nir - red)/ (nir + red + L)',
    list(
      'nir'= bandas$select('sur_refl_b02'),
      'red'= bandas$select('sur_refl_b01'),
      'L'= 0.5
    ))$rename(sprintf("%s%s",'savi',year))

  data <- extract_value_mean(savi, region)
  return(data)
}
