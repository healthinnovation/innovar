#' Extract Global Human Settlement Layer data
#'
#' A function that extract Global Human Settlement Layer data from \bold{1975} to \bold{2030} each 5 years.
#'
#' @param region region and object sf.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return  a tibble object with the new variable in km².
#'
#' @importFrom  sf st_transform st_simplify
#' @importFrom  rgee sf_as_ee
#' @importFrom dplyr select filter contains
#'
#' @examples
#' \dontrun{
#'
#' library(tidyverse)
#' library(rgee)
#' library(innovar)
#' library(sf)
#' ee_Initialize()
#'
#' # 1. Reading a sf object
#' data("Peru")
#' region <- Peru
#' region_ee <- pol_as_ee(region, id = 'distr' ,simplify = 1000)
#'
#' # 2. Extracting climate information
#' data <- region_ee %>% get_ghsl()
#' }
#' @export

get_ghsl <- function(region, scale = 100) {
    # The base image collection
    img_base <- ee$ImageCollection("users/ambarja/ghsl")$
      toBands()

    ghsl_area <- img_base$multiply(ee$Image$pixelArea())$
      divide(1000000)

    data <- ee_sum(
      x = ghsl_area,
      y = region,
      scale = 100
    )
    names(data) <- str_replace(names(data),'X',replacement = 'ghsl') %>%
      gsub("_b1","",.)

    return(data)
}
