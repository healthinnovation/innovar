#' Extract evapotranspiration data of Modis
#'
#' A function that extract a ETP time series of MODIS by \bold{month} (2001-01-01T00:00:00Z - 2022-04-23T00:00:00).
#'
#' @param to,from it's a string object,starting and final date.
#' @param band name of band.
#' @param region region and object sf.
#' @param fun function for extract statistic zonal (count, kurtosis, max,mean, median , min , mode , percentile, std, sum , variance, first).
#' @param scale A nominal scale in meters of the projection to work in.
#'
#' @details Name of some bands.
#' \itemize{
#' \item \bold{ET (kg/m²):} Total evapotranspiration.
#' \item \bold{LE (J/m²):}  Average latent heat flux.
#' \item \bold{PET (kg/m²):} Total potential evapotranspiration.
#' \item \bold{PLE (j/m²):} Average potential latent heat flux.
#' \item \bold{ET_QC:} Evapotranspiration quality control flags
#' }
#'
#' @return  a tibble object with the new variables.
#'
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
#' data <- region_ee %>%
#'   get_etp(from = "2001-02-01", to = "2003-12-31", band = "ET", fun = "max")
#' }
#' @export


get_etp <- function(from, to, band, region, fun = "count", scale = 1000) {

  # Conditions about the times
  start_year <- substr(from, 1, 4) %>% as.numeric()
  end_year <- substr(to, 1, 4) %>% as.numeric()
  # Factores by each bands

  multiply_factor <- c(
    ET = 0.1, LE = 0.0001, PET = 0.1, PLE = 0.0001, ET_QC = 1
  )

  # Message of error

  if (start_year <= 2000 | end_year >= 2022) {
    stop(print(sprintf("No exist data")))
  }

  # The main functions
  collection <- ee$ImageCollection("MODIS/006/MOD16A2")
  # filter quality
  bitwiseExtract <- function(value, fromBit, toBit = fromBit) {
    maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
    mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
    final <- value$rightShift(fromBit)$bitwiseAnd(mask)
    return(final)
    }

  filteApply <- function(image) {
      qa <- image$select("ET_QC")
      etp <- image$select(c(band))
      # build filter
      filter1 <- bitwiseExtract(qa, 3, 4)
      # build mask
      mask <- filter1$neq(2)
      # apply mas
      etp$updateMask(mask) %>% return()
    }

  # date of dataset
  months <- ee$List$sequence(1, 12)
  years <- ee$List$sequence(start_year, end_year)

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
