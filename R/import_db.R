#' Loads district data for Perú
#'
#' Function 'import_db' the specified dataset.
#'
#' @param dataset is a character specifying the dataset to request.
#'
#' Current datasets available are:
#'
#' - \bold{Peru_names}: Returns the name of the 1874 districts, 196 provinces and 25 regions of Peru as they are reported by the INEI in the REDATAM platform for the 2017 CENSUS.
#'
#' - \bold{Peru_shp}: Returns the shapefile for the 1874 districts, 196 provinces and 25 regions of Peru.
#'
#' @examples
#' # Import shapefiles of Peru
#' peru_shp <- import_db('Peru_shp')
#' head(peru_shp)
#'
#' # Import names of distritcts
#' peru_names <- import_db('Peru_names')
#' head(peru_names)
#'
#' @importFrom utils read.csv
#' @import RCurl
#' @importFrom  sf st_cast
#' @export import_db

import_db <- function(dataset) {
  if (dataset == "Peru_names") {
    x <- read.csv("https://raw.githubusercontent.com/healthinnovation/lis/master/files/master_distr.csv", stringsAsFactors = F)

  } else if (dataset == "Peru_shp") {

    x <- "https://github.com/healthinnovation/lis/blob/master/files/shp_PER_adm3.Rdata?raw=true"
    x <- suppressWarnings(st_cast(readRDS(url(x)),"POLYGON"))
  }

  return(x)
}
