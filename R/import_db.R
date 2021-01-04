#' Loads district data for Perú
#'
#'Function 'import_db' the specified dataset.
#'
#' @param dataset is a character specifiying the dataset to request.
#'
#' Current datasets available are:
#'
#' - "Perú names": Returns the name of the 1874 districts, 196 provinces and 25 regions of Perú as they are reported by the INEI in the REDATAM platform for the 2017 CENSUS.
#'
#' - "Perú shp": Returns the shapefile for thethe 1874 districts, 196 provinces and 25 regions of Perú
#'
#' @examples df <- data.frame(reg=c("LIMA","CALLAO","CAJAMARCA","AMAZONAS","SAN MARTIN","HUANUCO",
#' "PASCO","JUNIN","CUSCO", "PUNO","APURIMAC","AYACUCHO",
#' "HUANCAVELICA","TUMBES","PIURA","LAMBAYEQUE","LA LIBERTAD",
#' "ANCASH","ICA","AREQUIPA","TACNA","MOQUEGUA","LORETO","UCAYALI",
#' "MADRE DE DIOS"),
#' stringsAsFactors = FALSE)
#'
#'
#' @import RCurl
#' @export import_db
import_db <- function(dataset) {


  if(dataset=="Perú names") {

    x <- getURL("https://raw.githubusercontent.com/healthinnovation/lis/master/files/master_distr.csv")
    x <- read.csv(text = x, stringsAsFactors = F)

  } else if(dataset=="Perú shp") {

    x <- #"https://github.com/thefactmachine/hex-binning-gis-data/raw/master/popDensity.RData"
    x <- readRDS(url(x))

  }

  return(x)
}
