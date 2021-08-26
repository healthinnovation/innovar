#' Extract deforestation area data from Hansen
#'
#' A function that extract deforestation area data of the year \bold{2001-2019}
#'
#' @param to,from it's a string object,starting and final date.
#' @param region region and object sf.
#' @return  a tibble object with the new variables.
#' @param scale A nominal scale in meters of the projection to work in.
#'
#' @importFrom sf st_transform st_simplify
#' @importFrom rgee sf_as_ee
#' @importFrom dplyr select filter contains
#'
#' @examples
#' \dontrun{
#'
#' library(tidyverse)
#' library(rgee)
#' library(lis)
#' library(sf)
#' ee_Initialize()
#'
#' # 1. Reading a sf object
#' region <- import_db("Peru_shp")
#' region_ee <- pol_as_ee(region, id = 'distr' ,simplify = 1000)
#'
#' # 2. Extracting deforestation area data
#' data <- region_ee %>%
#' get_def(
#'  from = '2001-01-01',
#'  to = '2005-12-31',
#'  region = region_ee,
#'  scale = 30
#' )
#' }
#' @export

get_def <- function(from, to, region, scale = 100) {

  # Conditions about the times
  start_year <- substr(from, 1, 4) %>% as.numeric()
  end_year <- substr(to, 1, 4) %>% as.numeric()

  # loss condition
  rango <- c(1:19)
  names(rango) <- 2001:2019
  anio <- rango[c(as.character(start_year:end_year))]

  # The base image collection
  img_base <- ee$Image("UMD/hansen/global_forest_change_2019_v1_7")$
    select(c('lossyear'))$
    gte(anio)

  def_area <- img_base$multiply(ee$Image$pixelArea())$
    divide(1000000)

  data <- ee_sum(
    x = def_area,
    y = region,
    scale = 30
  )

  id_names <- which(
    startsWith(
      names(data),
      prefix = 'const')
    )

  names_id <- substr(
    seq(
      as.Date(from),
      as.Date(to),
      length.out = length(id_names)
    ),
    1,4
  )

  names(data)[id_names] <- sprintf('%s%s','Adef_',names_id)
  return(data)
}
