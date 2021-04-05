#' Extract of vegetation index
#'
#' A function that extract the values of vegetation.
#'
#' @param to,from the starting and final range of date.
#' @param by  two types of increment of the sequence by \bold{month} and \bold{year}.
#' @param band name of band.
#' @param region region and object sf.
#' @param fun function for extract statistic zonal ('count','kurtosis','max','mean','median','min','mode','percentile','std','sum','variance','first').
#'
#' @details Name of some bands.
#' \itemize{
#' \item \bold{NDVI:} Normalized Difference Vegetation Index.
#' \item \bold{EVI:} Enhanced Vegetation Index.
#' }
#'
#' @return  a sf object with the new variables.
#' @importFrom  sf st_transform st_simplify
#' @importFrom  rgee sf_as_ee
#' @importFrom dplyr select filter contains
#' @importFrom purrr is_empty
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
#' region_ee <- pol_as_ee(region, simplify = 1000)
#'
#' # 2. Extracting climate information
#' data <- region_ee %>% get_vegetation(
#'   to = "2001-02-01", from = "2002-12-31",
#'   by = "month", band = "tmmx", fun = "max")
#' }
#' @export

get_vegetation <- function(to, from, by, band, region, fun = "count") {

  # Conditions about the times

  start_year <- substr(to, 1, 4) %>% as.numeric()
  end_year <- substr(from, 1, 4) %>% as.numeric()
  year <- unique(c(start_year:end_year))
  year_list <- ee$List(year)


  # Factores by each bands

  multiply_factor <- c(
    NDVI = 0.0001, EVI = 0.0001
  )


  # Message of error

  if (end_year > 1999 | start_year <= 2021) {
    print(sprintf("No exist data"))
  }

  # The main functions
  if (by == "month" & fun == "count") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_count <- ee_count(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_count %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_count) %in% actual_names)
    names(img_with_value_count)[id_names] <- new_names
    return(img_with_value_count)

  } else if (by == "month" & fun == "first") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_first <- ee_first(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_first %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_first) %in% actual_names)
    names(img_with_value_first)[id_names] <- new_names
    return(img_with_value_first)

  } else if (by == "month" & fun == "kurtosis") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_kurtosis <- ee_kurtosis(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_kurtosis %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_kurtosis) %in% actual_names)
    names(img_with_value_kurtosis)[id_names] <- new_names
    return(img_with_value_kurtosis)

  } else if (by == "month" & fun == "max") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_max <- ee_max(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_max %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_max) %in% actual_names)
    names(img_with_value_max)[id_names] <- new_names
    return(img_with_value_max)

  } else if (by == "month" & fun == "mean") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_mean <- ee_mean(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_mean %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_mean) %in% actual_names)
    names(img_with_value_mean)[id_names] <- new_names
    return(img_with_value_mean)

  } else if (by == "month" & fun == "median") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_median <- ee_median(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_median %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_median) %in% actual_names)
    names(img_with_value_median)[id_names] <- new_names
    return(img_with_value_median)

  } else if (by == "month" & fun == "min") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_min <- ee_min(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_min %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_min) %in% actual_names)
    names(img_with_value_min)[id_names] <- new_names
    return(img_with_value_min)

  } else if (by == "month" & fun == "mode") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_mode <- ee_mode(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_mode %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_mode) %in% actual_names)
    names(img_with_value_mode)[id_names] <- new_names
    return(img_with_value_mode)

  } else if (by == "month" & fun == "percentile") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_percentile <- ee_percentile(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_percentile %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_percentile) %in% actual_names)
    names(img_with_value_percentile)[id_names] <- new_names
    return(img_with_value_percentile)

  } else if (by == "month" & fun == "std") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_std <- ee_std(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_std %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_std) %in% actual_names)
    names(img_with_value_std)[id_names] <- new_names
    return(img_with_value_std)

  } else if (by == "month" & fun == "sum") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_sum <- ee_sum(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_sum %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_sum) %in% actual_names)
    names(img_with_value_sum)[id_names] <- new_names
    return(img_with_value_sum)

  } else if (by == "month" & fun == "variance") {
    dataset <- ee$ImageCollection("MODIS/006/MOD13Q1")$
      select(c(band))$
      filterDate(to, from)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_with_value_variance <- ee_variance(dataset, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 month"), start = 1, stop = 7))
    actual_names <- img_with_value_variance %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_variance) %in% actual_names)
    names(img_with_value_variance)[id_names] <- new_names
    return(img_with_value_variance)
  }


  if (by == "year" & fun == "count") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$ multiply(multiply_factor[[band]])
    img_with_value_count <- ee_count(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_count %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_count) %in% actual_names)
    names(img_with_value_count)[id_names] <- new_names
    return(img_with_value_count)

  } else if (by == "year" & fun == "kurtosis") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_kurtosis <- ee_kurtosis(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_kurtosis %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_kurtosis) %in% actual_names)
    names(img_with_value_kurtosis)[id_names] <- new_names
    return(img_with_value_kurtosis)

  } else if (by == "year" & fun == "max") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_max <- ee_max(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_max %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_max) %in% actual_names)
    names(img_with_value_max)[id_names] <- new_names
    return(img_with_value_max)

  } else if (by == "year" & fun == "mean") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_mean <- ee_mean(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_mean %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_mean) %in% actual_names)
    names(img_with_value_mean)[id_names] <- new_names
    return(img_with_value_mean)

  } else if (by == "year" & fun == "median") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_median <- ee_median(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_median %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_median) %in% actual_names)
    names(img_with_value_median)[id_names] <- new_names
    return(img_with_value_median)

  } else if (by == "year" & fun == "min") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_min <- ee_min(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_min %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_min) %in% actual_names)
    names(img_with_value_min)[id_names] <- new_names
    return(img_with_value_min)

  } else if (by == "year" & fun == "mode") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_mode <- ee_mode(img_by_year, region)
    return(img_with_value_mode)

  } else if (by == "year" & fun == "percentile") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_percentile <- ee_percentile(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_percentile %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_percentile) %in% actual_names)
    names(img_with_value_percentile)[id_names] <- new_names
    return(img_with_value_percentile)

  } else if (by == "year" & fun == "std") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_std <- ee_std(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_std %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_std) %in% actual_names)
    names(img_with_value_std)[id_names] <- new_names
    return(img_with_value_std)

  } else if (by == "year" & fun == "sum") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_sum <- ee_sum(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_sum %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_sum) %in% actual_names)
    names(img_with_value_sum)[id_names] <- new_names
    return(img_with_value_sum)

  } else if (by == "year" & fun == "variance") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_variance <- ee_variance(img_by_year, region)
    new_names <- paste0(band, substr(seq(as.Date(to), as.Date(from), by = "1 year"), start = 1, stop = 4))
    actual_names <- img_with_value_variance %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_variance) %in% actual_names)
    names(img_with_value_variance)[id_names] <- new_names
    return(img_with_value_variance)

  } else if (by == "year" & fun == "first") {
    list_img_by_year <- year_list$
      map(ee_utils_pyfunc(function(x) {
        ee$ImageCollection("MODIS/006/MOD13Q1")$
          select(c(band))$
          filter(ee$Filter$calendarRange(x, x, "year"))$
          sum()
      }))

    img_by_year <- ee$ImageCollection$fromImages(list_img_by_year)$toBands()$multiply(multiply_factor[[band]])
    img_with_value_first <- ee_first(img_by_year, region)
    actual_names <- img_with_value_first %>%
      select(contains(band)) %>%
      st_set_geometry(NULL) %>%
      colnames()
    id_names <- which(colnames(img_with_value_first) %in% actual_names)
    names(img_with_value_first)[id_names] <- new_names
    return(img_with_value_first)
  }
}
