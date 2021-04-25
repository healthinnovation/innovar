#' Extract evapotranspiration data of Modis
#'
#' A function that extract a ETP time series of MODIS by \bold{month}.
#'
#' @param to,from it's a string object,starting and final date.
#' @param band name of band.
#' @param region region and object sf.
#' @param fun function for extract statistic zonal (count, kurtosis, max,mean, median , min , mode , percentile, std, sum , variance, first).
#' @details Name of some bands.
#' \itemize{
#' \item \bold{ET:} Total evapotranspiration.
#' \item \bold{LE:}  Average latent heat flux.
#' \item \bold{PET:} Total potential evapotranspiration.
#' \item \bold{PLE:} Average potential latent heat flux.
#' \item \bold{ET_QC:} Evapotranspiration quality control flags
#' }
#'
#' @return  a sf object with the new variables.
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
#' library(lis)
#' library(sf)
#' ee_Initialize()
#'
#' # 1. Reading a sf object
#' region <- import_db("Peru_shp")
#' region_ee <- pol_as_ee(region,  id = 'distr' , simplify = 1000)
#'
#' # 2. Extracting climate information
#' data <- region_ee %>%
#' get_etp(to = "2001-02-01", from = "2003-12-31", band = "ET", fun = "max")
#' }
#' @export


get_etp <- function(to, from, band, region, fun = "count") {

  # Conditions about the times
  start_year <- substr(to, 1, 4) %>% as.numeric()
  end_year <- substr(from, 1, 4) %>% as.numeric()
  year <- unique(c(start_year:end_year)) %>% list()
  year_list <- ee$List(year)

  # Factores by each bands

  multiply_factor <- c(
    ET = 0.1, LE = 0.0001, PET = 0.1, PLE = 0.0001, ET_QC = 1
  )

  # Message of error

    if (start_year <= 2000 | end_year >= 2022) {
    stop(print(sprintf("No exist data")))
  }

  # The main functions
  # NDVI
  collection_ndvi = ee$ImageCollection('MODIS/006/MOD16A2')$
    select(c(band))

  # date of dataset
  months = ee$List$sequence(1, 12)
  years = ee$List$sequence(start_year,end_year)

  modis = ee$ImageCollection$
    fromImages(
      years$map(
        ee_utils_pyfunc(
          function (y)
            months$map(
              ee_utils_pyfunc(
                function (m)
                  lista_collection[[band]]$
                  filter(ee$Filter$calendarRange(y, y, 'year'))$
                  filter(ee$Filter$calendarRange(m, m, 'month'))$
                  mean()$
                  set('year',y)$
                  set('month',m))
            )
        )
      )$
        flatten()
    )

  im_base <- modis$
    filter(ee$Filter$inList('month',c(1:12)))
  im_base2 <- im_base$
    filter(ee$Filter$inList('year',list(start_year:end_year)))$
    toBands()$multiply(multiply_factor[[band]])

  # The main functions
  if (fun == "count") {
    img_count <- ee_count(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_count),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_count)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_count)

  } else if (by == "month" & fun == "kurtosis") {
    img_kurtosis <- ee_kurstosis(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_kurtosis),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_kurtosis)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_kurtosis)


  } else if (by == "month" & fun == "max") {
    img_max <- ee_max(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_max),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_max)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_max)


  } else if (by == "month" & fun == "mean") {
    img_mean <- ee_mean(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_mean),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_mean)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_mean)


  } else if (by == "month" & fun == "median") {
    img_median <- ee_median(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_median),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_median)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_median)


  } else if (by == "month" & fun == "min") {
    img_min <- ee_min(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_min),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_min)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_min)


  } else if (by == "month" & fun == "mode") {
    img_mode <- ee_mode(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_mode),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_mode)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_mode)


  } else if (by == "month" & fun == "percentile") {
    img_percentile <- ee_percentile(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_percentile),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_percentile)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_percentile)

  } else if (by == "month" & fun == "std") {
    img_std <- ee_std(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_std),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_std)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_std)

  } else if (by == "month" & fun == "sum") {
    img_sum <- ee_sum(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_sum),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_sum)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_sum)

  } else if (by == "month" & fun == "variance") {
    img_variance <- ee_variance(
      im_base2,
      region
    )
    id_names <- which(
      startsWith(
        names(img_variance),
        prefix = band)
    )

    names_id <- substr(
      seq(
        as.Date(to),
        as.Date(from),
        length.out = length(id_names)
      ),
      1,7
    )

    names(img_variance)[id_names] <- sprintf('%s%s',band,names_id)
    return(img_variance)

  }
}
