#' Polygon to earth engine
#'
#' A function that transform polygon to featurecollection.
#'
#' @param x  polygon sf object.
#' @param id the main id name in sf object.
#' @param simplify number to simplify the sf polygon  keeping its topology
#' @return feature collection
#' @importFrom  dplyr select
#' @importFrom  rgee sf_as_ee
#' @import sf
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(rgee)
#' library(innovar)
#' library(sf)
#' ee_Initialize()
#' # 1. Reading a sf object
#' data("Peru")
#' region <- Peru %>% filter(reg == 'LIMA')
#' region_ee <- pol_as_ee(region , id = 'distr' , simplify = 1000)
#' class(region_ee)
#' }
#' @export

pol_as_ee <- function(x , id, simplify = 500){

  id_names <- which(colnames(x) %in% c(id))
  sf_object <- x %>%
    select(names(x)[id_names]) %>%
    st_transform(crs = 3857) %>%
    st_simplify(preserveTopology = TRUE,
                dTolerance = simplify) %>%
    st_transform(crs = 4326) %>%
    sf_as_ee()

  return(sf_object)

}
