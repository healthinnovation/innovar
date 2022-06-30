#' Extract climate data of Famine Early Warning Systems Network (FEWS NET) Land Data Assimilation System ()
#'
#' A function that extract a time series of climate variables (1982-01-01T00:00:00Z–2022-04-01T00:00:00).
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
#' \item \bold{Evap_tavg (kg m-2 s-1):} Evapotranspiration.
#' \item \bold{LWdown_f_tavg (W m-2):} Downward longwave radiation flux .
#' \item \bold{Lwnet_tavg (W m-2):} Net longwave radiation flux.
#' \item \bold{Psurf_f_tavg (Pa):} Surface pressure.
#' \item \bold{Qair_f_tavg (kg kg-1):} Specific humidity.
#' \item \bold{Qg_tavg (W m-2):} Soil heat flux.
#' \item \bold{Qh_tavg (W m-2):} Sensible heat net flux.
#' \item \bold{Qle_tavg (W m-2):} Latent heat net flux.
#' \item \bold{Qs_tavg (kg m-2 s-1):} Storm surface runoff.
#' \item \bold{Qsb_tavg (kg m-2 s-1):} Baseflow-groundwater runoff.
#' \item \bold{RadT_tavg (K):} Surface radiative temperature.
#' \item \bold{Rainf_f_tavg (kg m-2 s-1):} Total precipitation rate.
#' \item \bold{SnowCover_inst :} Snow cover fraction.
#' \item \bold{SnowDepth_inst (m):} Snow depth.
#' \item \bold{Snowf_tavg (kg m-2 s-1):} Snowfall rate.
#' \item \bold{SoilMoi00_10cm_tavg (m^3 m-3):}Soil moisture (0 - 10 cm underground).
#' \item \bold{SoilMoi10_40cm_tavg (m^3 m-3):} Soil moisture (10 - 40 cm underground).
#' \item \bold{SoilMoi100_200cm_tavg (m^3 m-3):} Soil moisture (100 - 200 cm underground).
#' \item \bold{SoilMoi40_100cm_tavg (m^3 m-3):} Soil moisture (40 - 100 cm underground).
#' \item \bold{SoilTemp00_10cm_tavg (K):} Soil temperature (0 - 10 cm underground).
#' \item \bold{SoilTemp10_40cm_tavg	(K):} Soil temperature (10 - 40 cm underground).
#' \item \bold{SoilTemp100_200cm_tavg (K):} Soil temperature (100 - 200 cm underground).
#' \item \bold{SoilTemp40_100cm_tavg (K):} Soil temperature (40 - 100 cm underground).
#' \item \bold{SWdown_f_tavg (W m-2):} Surface downward shortwave radiation.
#' \item \bold{SWE_inst (kg m-2):} Snow water equivalent.
#' \item \bold{Swnet_tavg	(W m-2):} Net shortwave radiation flux.
#' \item \bold{Tair_f_tavg (K):} Near surface air temperature.
#' \item \bold{Wind_f_tavg (m s-1):} Near surface wind speed.
#'
#'
#'
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
#' data <- region_ee %>% get_fldas(
#'   from = "2001-02-01", to = "2002-12-31",
#'   by = "month", band = "Qair_f_tavg", fun = "mean")
#' }
#' @export

get_fldas <- function(from, to, by, band, region, fun = "mean",scale = 1000) {

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

  # Message of error

  if (end_year > 2023 | start_year < 1982) {
    print(sprintf("No exist data"))
  }

  # The main functions
  if (by == "month" & fun == "count") {

    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
    img_base <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
      select(c(band))$
      filterDate(from, to)$
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
          ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
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
      toBands()

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
