#' Extract Aerosol index (UVAI) data of Sentinel5
#'
#' A function that extract a time series of Aerosol index variables.
#'
#' @param to,from it's a string object,starting and final date.
#' @param band name of band.
#' @param region is a feature or feature collection.
#' @param fun function for extract statistic zonal (count, kurtosis, max, mean, median, min, mode, percentile, std, sum, variance, first).
#' @param scale A nominal scale in meters of the projection to work in.
#'
#' @details Name of some bands.
#' \itemize{
#' \item \bold{absorbing_aerosol_index (mol/m²):} A measure of the prevalence of aerosols in the atmosphere, calculated by this \href{https://sentinel.esa.int/web/sentinel/technical-guides/sentinel-5p/level-2/aerosol-index}{equation} using the 354/388 wavelength pair.
#' \item \bold{sensor_altitude (mol/m²):} Altitude of the satellite with respect to the geodetic sub-satellite point (WGS84).
#' \item \bold{sensor_azimuth_angle (degrees):} Azimuth angle of the satellite at the ground pixel location (WGS84); angle measured East-of-North.
#' \item \bold{sensor_zenith_angle (degrees):} Zenith angle of the satellite at the ground pixel location (WGS84); angle measured away from the vertical.
#' \item \bold{sensor_zenith_angle (degrees):} Azimuth angle of the Sun at the ground pixel location (WGS84); angle measured East-of-North.
#' \item \bold{solar_zenith_angle (degrees):}Zenith angle of the satellite at the ground pixel location (WGS84); angle measured away from the vertical.
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
#' region_ee <- pol_as_ee(region , id = 'distr' , simplify = 1000)
#' # 2. Extracting climate information
#' data <- region_ee %>% get_aero(
#'   from = "2019-02-01", to = "2019-12-31",
#'   band = "absorbing_aerosol_index", fun = "max")
#' }
#' @export

get_aero <- function(from, to, band, region, fun = "count", scale = 1000) {

  # Conditions about the times
  start_year <- substr(from, 1, 4) %>% as.numeric()
  end_year <- substr(to, 1, 4) %>% as.numeric()

  # Message of error

  if (start_year < 2018) {
    from = "2018-07-01"
    start_year = substr(from, 1, 4) %>% as.numeric()
    print(sprintf("No exist data, aerosol is available from > 2018"))
  }


  # Dataset
  collection <- ee$ImageCollection("COPERNICUS/S5P/NRTI/L3_AER_AI")$
    select(c(band))

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
                max()$
                rename("uavi")$
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
      )$toBands()
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
      toBands()
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
        suffix = "uavi"
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

    names(img_count)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_kurtosis)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_max)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_mean)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_median)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_min)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_mode)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_percentile)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_std)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_sum)[id_names] <- sprintf("%s%s", "uavi_", names_id)
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
        suffix = "uavi"
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

    names(img_variance)[id_names] <- sprintf("%s%s", "uavi_", names_id)
    return(img_variance)
  }
}
