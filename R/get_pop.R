#' Extract population data of WorldPop
#'
#' A function that extract a time series of the number of population by \bold{year} (2000-01-01T00:00:00Z - 2021-01-01T00:00:00).
#'
#' @param to,from it's a string object,starting and final date.
#' @param region it's a feature collection.
#' @param fun function for extract statistic zonal (\bold{count, kurtosis, max, mean, median, min, mode, percentile, std, sum, variance}).
#' @param scale A nominal scale in meters of the projection to work in.
#' @return  a tibble with the new variables.
#'
#' @importFrom sf st_transform st_simplify
#' @importFrom rgee sf_as_ee
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
#' data <- region_ee %>% get_pop(
#'   from = "2001-01-01", to = "2003-01-01",fun = "max",scale = 100)
#' }
#' @export

get_pop <- function(from, to, region, fun = "count", scale = 100) {

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
  if (to < 2000  | from > 2021) {
    print(sprintf("No exist data of worldpop"))
  }

  # The base image collection
  list_img <- year_list$
    map(
      ee_utils_pyfunc(
        function(x) {
          ee$ImageCollection("WorldPop/GP/100m/pop")$
            select(c('population'))$
            filter(ee$Filter$calendarRange(x, x, "year"))$
            mosaic()$
            rename('pop')
        }
      )
    )

  img_by_year <- ee$ImageCollection$
    fromImages(list_img)$
    toBands()

  # Conditions

  if (fun == "count") {
    img_count <- ee_count(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_count), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_count)[id_names] <- sprintf('pop%s',names_id)
    return(img_count)

  } else if (fun == "kurtosis") {
    img_kurtosis <- ee_kurstosis(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_kurtosis), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_kurtosis)[id_names] <- sprintf('pop%s',names_id)
    return(img_kurtosis)

  } else if (fun == "max") {
    img_max <- ee_max(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_max), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_max)[id_names] <- sprintf('pop%s',names_id)
    return(img_max)

  } else if (fun == "mean") {
    img_mean <- ee_mean(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_mean), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_mean)[id_names] <- sprintf('pop%s',names_id)
    return(img_mean)

  } else if (fun == "median") {
    img_median <- ee_median(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_median), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_median)[id_names] <- sprintf('pop%s',names_id)
    return(img_median)

  } else if (fun == "min") {
    img_min <- ee_min(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_min), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_min)[id_names] <- sprintf('pop%s',names_id)
    return(img_min)

  } else if (fun == "mode") {
    img_mode <- ee_mode(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_mode), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_mode)[id_names] <- sprintf('pop%s',names_id)
    return(img_mode)

  } else if (fun == "percentile") {
    img_percentile <- ee_percentile(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_percentile), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_percentile)[id_names] <- sprintf('pop%s',names_id)
    return(img_percentile)

  } else if (fun == "std") {
    img_std <- ee_std(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_std), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_std)[id_names] <- sprintf('pop%s',names_id)
    return(img_std)

  } else if (fun == "sum") {
    img_sum <- ee_sum(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_sum), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_sum)[id_names] <- sprintf('pop%s',names_id)
    return(img_sum)

  } else if (fun == "variance") {
    img_variance <- ee_variance(
      img_by_year,
      region,
      scale = scale
    )

    id_names <- which(
      endsWith(
        names(img_variance), suffix = 'pop')
    )

    names_id <- substr(
      seq(
        as.Date(from),
        as.Date(to),
        length.out = length(id_names)
      ),
      1,4
    )

    names(img_variance)[id_names] <- sprintf('pop%s',names_id)
    return(img_variance)
  }
}
