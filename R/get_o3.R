#' Extract Ozone data of Sentinel5
#'
#' A function that extract a time series of ozone (2018-07-10T11:02:44Z - 2022-05-15T00:00:00).
#'
#' @param to,from it's a string object,starting and final date.
#' @param band name of band.
#' @param region is a feature or feature collection.
#' @param fun function for extract statistic zonal (count, kurtosis, max, mean, median, min, mode, percentile, std, sum, variance, first).
#' @param scale A nominal scale in meters of the projection to work in.
#'
#' @details Name of some bands.
#' \itemize{
#' \item \bold{O3_column_number_density (mol/m²):} Total atmospheric column of O3 between the surface and the top of atmosphere, calculated with the DOAS algorithm.
#' \item \bold{O3_column_number_density_amf (mol/m²):} Weighted mean of cloudy and clear air mass factor (amf) weighted by intensity-weighted cloud fraction.
#' \item \bold{O3_slant_column_number_density (mol/m²):} O3 ring corrected slant column number density.
#' \item \bold{O3_effective_temperature (K):} Ozone cross section effective temperature.
#' \item \bold{cloud_fraction:} Effective cloud fraction. See the Sentinel 5P L2 Input/Output Data Definition Spec, p.220.
#' \item \bold{sensor_azimuth_angle (degrees):} 	Azimuth angle of the satellite at the ground pixel location (WGS84); angle measured East-of-North.
#' \item \bold{sensor_zenith_angle (degrees):} Zenith angle of the satellite at the ground pixel location (WGS84); angle measured away from the vertical.
#' \item \bold{solar_azimuth_angle (degrees):} Azimuth angle of the Sun at the ground pixel location (WGS84); angle measured East-of-North.
#' \item \bold{solar_zenith_angle (degrees):} Zenith angle of the satellite at the ground pixel location (WGS84); angle measured away from the vertical.
#'  }
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
#' region <- Peru
#' region_ee <- pol_as_ee(region , id = 'distr' , simplify = 1000)
#' # 2. Extracting climate information
#' data <- region_ee %>% get_o3(
#'   from = "2019-02-01", to = "2019-12-31",
#'   band = "CO_column_number_density", fun = "max")
#' }
#' @export

get_o3 <- function(from, to, band, region, fun = "max", scale = 1000) {

  # Conditions about the times
  start_year <- substr(from, 1, 4) %>% as.numeric()
  end_year <- substr(to, 1, 4) %>% as.numeric()

  # Message of error

  if (start_year < 2018) {
    from = "2018-07-01"
    start_year = substr(from, 1, 4) %>% as.numeric()
    print(sprintf("No exist data, O3 is available from > 2018"))
  }

  # Dataset
  collection <- ee$ImageCollection("COPERNICUS/S5P/NRTI/L3_O3")$
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
              rename("O3")$
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
        suffix = "O3"
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

    names(img_count)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_kurtosis)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_max)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_mean)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_median)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_min)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_mode)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_percentile)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_std)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_sum)[id_names] <- sprintf("%s%s", "O3_", names_id)
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
        suffix = "O3"
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

    names(img_variance)[id_names] <- sprintf("%s%s", "O3_", names_id)
    return(img_variance)
  }
}
