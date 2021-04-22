#' Extract Global Human Modification data
#'
#' A function that extract Global Human Modification data of the year \bold{2016}
#'
#' @param region region and object sf.
#' @param fun function for extract statistic zonal (count, kurtosis, max, mean, median, min, mod, percentile, std, sum, variance, first).
#'
#' @return  a sf object with the new variables.
#' @importFrom  sf st_transform st_simplify
#' @importFrom  rgee sf_as_ee
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
#' # 2. Extracting climate information
#' data <- region_ee %>%
#' get_gHM(
#'  fun = "max"
#' )
#' }
#' @export

get_gHM <- function(region, fun = "count") {

  # The base image collection
  img_base <- ee$ImageCollection("CSP/HM/GlobalHumanModification")$
    select(c('gHM'))$
    mosaic()

  # Conditions
  if (fun == "count") {
    img_count <- ee_count(
      img_base,
      region
      )
    return(img_count)

  } else if (fun == "kurtosis") {
    img_kurtosis <- ee_kurstosis(
      img_base,
      region
      )
    return(img_kurtosis)

  } else if (fun == "max") {
    img_max <- ee_max(
      img_base,
      region
      )
    return(img_max)

  } else if (fun == "mean") {
    img_mean <- ee_mean(
      img_base,
      region
      )
    return(img_mean)

  } else if (fun == "median") {
    img_median <- ee_median(
      img_base,
      region
      )
    return(img_median)

  } else if (fun == "min") {
    img_min <- ee_min(
      img_base,
      region
      )
    return(img_min)

  } else if (fun == "mode") {
    img_mode <- ee_mode(
      img_base,
      region
      )
    return(img_mode)

  } else if (fun == "percentile") {
    img_percentile <- ee_percentile(
      img_base,
      region
      )
    return(img_percentile)

  } else if (fun == "std") {
    img_std <- ee_std(
      img_base,
      region
      )
    return(img_std)

  } else if (fun == "sum") {
    img_sum <- ee_sum(
      img_base,
      region
      )
    return(img_sum)

  } else if (fun == "variance") {
    img_variance <- ee_variance(
      img_base,
      region
      )
    return(img_variance)

  } else if (fun == "first") {
    img_first <- ee_first(
      img_base,
      region
      )
    return(img_first)
  }
}
