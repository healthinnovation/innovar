#' Extract climate data of TerraClimate
#'
#' A function that extract a time series of climate variables.
#'
#' @param to,from it's a string object,starting and final date.
#' @param by  two types of increment of the sequence by \bold{month} and \bold{year}.
#' @param band name of band.
#' @param region is a feature or feature collection.
#' @param fun function for extract statistic zonal (count, kurtosis, max, mean, median, min, mode, percentile, std, sum, variance, first).
#' @param scale A nominal scale in meters of the projection to work in.
#'
#' @details Name of some bands.
#' \itemize{
#' \item \bold{aet:} Actual evapotranspiration, derived using a one-dimensional soil water balance model.
#' \item \bold{def:} Climate water deficit, derived using a one-dimensional soil water balance model.
#' \item \bold{pdsi:} Palmer Drought Severity Index.
#' \item \bold{pet:} Reference evapotranspiration (ASCE Penman-Montieth).
#' \item \bold{pr:} Precipitation accumulation.
#' \item \bold{ro:} Runoff, derived using a one-dimensional soil water balance model.
#' \item \bold{soil:} Soil moisture, derived using a one-dimensional soil water balance model.
#' \item \bold{srad:} Downward surface shortwave radiation.
#' \item \bold{swe:} Snow water equivalent, derived using a one-dimensional soil water balance model.
#' \item \bold{tmmn:} Minimum temperature.
#' \item \bold{tmmx:} Maximum temperature.
#' \item \bold{vap:} Vapor pressure
#' \item \bold{vpd:} Vapor pressure deficit.
#' \item \bold{vs:} Wind-speed at 10m.
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
#' region_ee <- pol_as_ee(region , id = 'distr' , simplify = 1000)
#' # 2. Extracting climate information
#' data <- region_ee %>% get_climate(
#'   from = "2001-02-01", to = "2002-12-31",
#'   by = "month", band = "tmmx", fun = "max")
#' }
#' @export

get_climate <- function(from, to, by, band, region, fun = "count",scale = 1000) {

  # Conditions about the times

  start_year <- substr(from, 1, 4) %>% as.numeric()
  end_year <- substr(to, 1, 4) %>% as.numeric()

  if(start_year == end_year){
    year <- unique(
      c(start_year:end_year)
      ) %>%
      list()

    year_list <- ee$List(year)
  } else {
    year <- unique(
      c(start_year:end_year)
      )
    year_list <- ee$List(year)
  }

  # Factores by each bands

  multiply_factor <- c(
    aet = 0.1, def = 0.1, pdsi = 0.01, pet = 0.1,
    pr = 1, ro = 1, soil = 0.1, srad = 0.1, swe = 1,
    tmmn = 0.1, tmmx = 0.1, vap = 0.001, vpd = 0.01,
    vs = 0.01
  )

  # Message of error

  if (end_year > 2019 | start_year < 1958) {
    print(sprintf("No exist data"))
  }

  # The main functions
  if (by == "month" & fun == "count") {

    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_count <- ee_count(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_count),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
        ),
      1,7
      )

    names(img_count)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_count)

  } else if (by == "month" & fun == "kurtosis") {
    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_kurstosis <- ee_kurstosis(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_kurstosis),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_kurtosis)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_kurtosis)

  } else if (by == "month" & fun == "max") {
    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_max <- ee_max(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_max),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_max)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_max)

  } else if (by == "month" & fun == "mean") {
    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_mean <- ee_mean(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_mean),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_mean)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_mean)

  } else if (by == "month" & fun == "median") {
    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_median <- ee_median(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_median),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_median)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_median)

  } else if (by == "month" & fun == "min") {
    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_min <- ee_min(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_min),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_min)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_min)

  } else if (by == "month" & fun == "mode") {
    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_mode <- ee_mode(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_mode),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_mode)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_mode)

  } else if (by == "month" & fun == "percentile") {
    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_percentile <- ee_percentile(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_percentile),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_percentile)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_percentile)

  } else if (by == "month" & fun == "std") {
     img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_std <- ee_std(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_std),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_std)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_std)

  } else if (by == "month" & fun == "sum") {
    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_sum <- ee_sum(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_sum),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_sum)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_sum)

  } else if (by == "month" & fun == "variance") {
    img_base <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
      select(c(band))$
      filterDate(from, to)$
      toBands()$
      multiply(multiply_factor[[band]])

    img_variance <- ee_variance(
      img_base,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_variance),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
         by = '1 month'
      ),
      1,7
    )

    names(img_variance)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_variance)
  }

  if (by == "year" & fun == "count") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
              )$
            sum() }
          )
        )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
        )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_count <- ee_count(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_count),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year',
      ),
      1,4
    )

    names(img_count)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_count)


  } else if (by == "year" & fun == "kurtosis") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_kurtosis <- ee_kurstosis(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_kurtosis),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_kurtosis)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_kurtosis)

  } else if (by == "year" & fun == "max") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_max <- ee_max(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_max),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_max)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_max)

  } else if (by == "year" & fun == "mean") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_mean <- ee_mean(
      img_base,
      region,
      scale = scale)

    id_names <- which(
      endsWith(
        names(img_mean),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_mean)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_mean)

  } else if (by == "year" & fun == "median") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_median <- ee_median(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_median),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_median)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_median)

  } else if (by == "year" & fun == "min") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_min <- ee_min(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_min),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_min)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_min)

  } else if (by == "year" & fun == "mode") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_mode <- ee_mode(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_mode),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_mode)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_mode)

  } else if (by == "year" & fun == "percentile") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_percentile <- ee_percentile(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_percentile),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_percentile)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_percentile)

  } else if (by == "year" & fun == "std") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_std <- ee_std(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_std),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_std)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_std)

  } else if (by == "year" & fun == "sum") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_sum <- ee_sum(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_sum),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_sum)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_sum)

  } else if (by == "year" & fun == "variance") {
    list_img <- year_list$
      map(
        ee_utils_pyfunc(function(x) {
          ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
            select(c(band))$
            filter(
              ee$Filter$calendarRange(x, x, "year")
            )$
            sum() }
        )
      )

    img_base <- ee$ImageCollection$
      fromImages(
        list_img
      )$
      toBands()$
      multiply(multiply_factor[[band]])

    img_variance <- ee_variance(
      img_base,
      region,
      scale = scale
      )

    id_names <- which(
      endsWith(
        names(img_variance),
        suffix = band)
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 year'
      ),
      1,4
    )

    names(img_variance)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_variance)
  }
}
