#' Extract vegetation index of MODIS
#'
#' A function that extract the values of vegetation by \bold{month} (2000-02-18T00:00:00Z - 2022-04-07T00:00:00).
#'
#' @param to,from the starting and final range of date.
#' @param band name of bands.
#' @param region is a feature or feature collection.
#' @param fun function for extract statistic zonal (count, kurtosis, max, mean, median , min, mode , percentile , std, sum, variance, first).
#' @param scale A nominal scale in meters of the projection to work in.
#'
#' @details Name of some bands.
#' \itemize{
#' \item \bold{NDVI:} Normalized Difference Vegetation Index.
#' \item \bold{EVI:} Enhanced Vegetation Index.
#' \item \bold{SAVI:} Soil Adjusted Vegetation Index.
#' }
#'
#' @return  a tibble object with the new variables.
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
#' library(innovar)
#' library(sf)
#' ee_Initialize()
#'
#' # 1. Reading a sf object
#' data("Peru")
#' region <- Peru
#' region_ee <- pol_as_ee(region, id = "distr", simplify = 1000)
#'
#' # 2. Extracting climate information
#' data <- region_ee %>% get_vegetation(
#'   from = "2001-02-01", to = "2002-12-31", band = "NDVI", fun = "max"
#' )
#' }
#' @export

get_vegetation <- function(from, to, band, region, fun = "count", scale = 1000) {

  # Conditions about the times
  start_year <- substr(from, 1, 4) %>% as.numeric()
  end_year <- substr(to, 1, 4) %>% as.numeric()

  # Factors by each bands

  multiply_factor <- c(
    NDVI = 0.0001, EVI = 0.0001, SAVI = 1
  )

  # Message of error

  if (end_year < 1999 | start_year > 2024) {
    print(sprintf("No exist data"))
  }

  # NDVI - EVI

  if (band == "SAVI") {
    collection <- ee$ImageCollection("MODIS/061/MOD13A1")$
      select(c("sur_refl_b01", "sur_refl_b02", "DetailedQA"))
  } else {
    collection <- ee$ImageCollection("MODIS/061/MOD13A1")$
      select(c(band, "DetailedQA"))
  }

  # filter quality
  bitwiseExtract <- function(value, fromBit, toBit = fromBit) {
    maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
    mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
    final <- value$rightShift(fromBit)$bitwiseAnd(mask)
    return(final)
  }

  if (band == "SAVI") {
    filteApply <- function(image) {
      qa <- image$select("DetailedQA")
      ndvi <- image$select(c("sur_refl_b01", "sur_refl_b02"))
      # build filter
      filter1 <- bitwiseExtract(qa, 0, 1)
      filter2 <- bitwiseExtract(qa, 15)
      filter3 <- bitwiseExtract(qa, 14)
      # build mask
      mask <- filter1$neq(2)$And(filter2$neq(1))$And(filter3$neq(1))
      # apply mas
      ndvi$updateMask(mask) %>% return()
    }
  } else {
    filteApply <- function(image) {
      qa <- image$select("DetailedQA")
      ndvi <- image$select(c(band))
      # build filter
      filter1 <- bitwiseExtract(qa, 0, 1)
      filter2 <- bitwiseExtract(qa, 15)
      filter3 <- bitwiseExtract(qa, 14)
      # build mask
      mask <- filter1$neq(2)$And(filter2$neq(1))$And(filter3$neq(1))
      # apply mas
      ndvi$updateMask(mask) %>% return()
    }
  }

  # savi index
  savi <- function(img) {
    index <- img$
      expression(
      "(1 + L) * float(nir - red)/ (nir + red + L)",
      list(
        "nir" = img$select("sur_refl_b02"),
        "red" = img$select("sur_refl_b01"),
        "L" = 0.5
      )
    )$rename("SAVI")
  }

  # date of dataset
  months <- ee$List$sequence(1, 12)
  years <- ee$List$sequence(start_year, end_year)

  if (band == "SAVI") {
    modis <- ee$
      ImageCollection$
      fromImages(years$map(
      ee_utils_pyfunc(function(y) {
        months$map(ee_utils_pyfunc(
          function(m) {
            collection$
              filter(ee$Filter$calendarRange(y, y, "year"))$
              filter(ee$Filter$calendarRange(m, m, "month"))$
              map(filteApply)$
              map(savi)$
              max()$
              set("year", y)$
              set("month", m)
          }
        ))
      })
    )$flatten())
  } else {
    modis <- ee$
      ImageCollection$
      fromImages(years$map(
      ee_utils_pyfunc(function(y) {
        months$map(ee_utils_pyfunc(
          function(m) {
            collection$
              filter(ee$Filter$calendarRange(y, y, "year"))$
              filter(ee$Filter$calendarRange(m, m, "month"))$
              map(filteApply)$
              max()$
              set("year", y)$
              set("month", m)
          }
        ))
      })
    )$flatten())
  }

  im_base <- modis$
    filter(ee$Filter$inList("month", c(1:12)))

  if (start_year == end_year) {
    new_base <- im_base$
      filter(
      ee$Filter$inList(
        "year",
        list(
          c(
            start_year:end_year
          )
        )
      )
    )$toBands()$
      multiply(
      multiply_factor[[band]]
    )
  } else {
    new_base <- im_base$
      filter(
      ee$Filter$inList(
        "year",
        c(
          start_year:end_year
        )
      )
    )$
      toBands()$
      multiply(
      multiply_factor[[band]]
    )
  }

  # The main functions
  if (fun == "count") {
    img_count <- ee_count(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_count),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_count)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_count)
  } else if (fun == "kurtosis") {
    img_kurtosis <- ee_kurstosis(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_kurtosis),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_kurtosis)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_kurtosis)
  } else if (fun == "max") {
    img_max <- ee_max(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_max),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_max)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_max)
  } else if (fun == "mean") {
    img_mean <- ee_mean(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_mean),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_mean)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_mean)
  } else if (fun == "median") {
    img_median <- ee_median(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_median),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_median)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_median)
  } else if (fun == "min") {
    img_min <- ee_min(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_min),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_min)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_min)
  } else if (fun == "mode") {
    img_mode <- ee_mode(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_mode),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_mode)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_mode)
  } else if (fun == "percentile") {
    img_percentile <- ee_percentile(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_percentile),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_percentile)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_percentile)
  } else if (fun == "std") {
    img_std <- ee_std(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_std),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_std)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_std)
  } else if (fun == "sum") {
    img_sum <- ee_sum(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_sum),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_sum)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_sum)
  } else if (fun == "variance") {
    img_variance <- ee_variance(
      new_base,
      region,
      scale = scale
    )
    id_names <- which(
      endsWith(
        names(img_variance),
        suffix = band
      )
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        by = '1 month'
      ),
      1, 7
    )

    names(img_variance)[id_names] <- sprintf("%s%s", band, names_id)
    return(img_variance)
  }
}
