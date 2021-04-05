#' Point to earth engine
#'
#' A function that transform points to featurecollection.
#'
#' @param x  point sf object.
#' @param id the main id name in sf object.
#'
#' @importFrom  dplyr select
#' @importFrom  rgee sf_as_ee
#' @export

pto_to_ee <- function(x, id){

  id_names <- which(colnames(x) %in% c(id))
  sf_object <- x %>%
    select(names(x)[id_names]) %>%
    sf_as_ee()

  return(sf_object)

}


#' Polygon to earth engine
#'
#' A function that transform polygon to featurecollection.
#'
#' @param x  polygon sf object.
#' @param id the main id name in sf object.
#' @param simplify number to simplify the sf polygon  keeping its topology
#'
#' @importFrom  dplyr select
#' @importFrom  rgee sf_as_ee
#' @import sf
#' @export

pol_to_ee <- function(x , id, simplify = 500){

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
