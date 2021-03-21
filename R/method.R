#' Set of function to zonal statistic
#' @param x image of type Image o Image Collection
#'
#' @param y region of type Feacture o FeatureCollection
#'
#' @param by a limit of pass
# Functions for extract the sum of pixels of a rasterdata

extract_value_sum <- function(x, y, by = 1000) {
  y_len <- y$size()$getInfo()
  for (i in seq(1, y_len, by)) {
    index <- i - 1
    print(sprintf("Extracting information [%s/%s]...", index, y_len))
    ee_value_layer <- ee$FeatureCollection(y) %>%
      ee$FeatureCollection$toList(by, index) %>%
      ee$FeatureCollection()
    if (i == 1) {
      dataset <- ee_extract(
        x = x,
        fun = ee$Reducer$sum(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$sum(),
        sf = T
      )
      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @noRd
#' @param x image of type Image o Image Collection
#'
#' @param y region of type Feacture o FeatureCollection
#'
#' @param by a limit of pass
#' mean
#' @import rgee
# Functions for extract the mean of pixels of a rasterdata

extract_value_mean <- function(x, y, by = 1000) {
  y_len <- y$size()$getInfo()
  for (i in seq(1, y_len, by)) {
    index <- i - 1
    print(sprintf("Extracting information [%s/%s]...", index, y_len))
    ee_value_layer <- ee$FeatureCollection(y) %>%
      ee$FeatureCollection$toList(by, index) %>%
      ee$FeatureCollection()
    if (i == 1) {
      dataset <- ee_extract(
        x = x,
        fun = ee$Reducer$mean(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$mean(),
        sf = T
      )
      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

#' Set of function to zonal statistic
#' @param x image of type Image o Image Collection
#'
#' @param y region of type Feacture o FeatureCollection
#'
#' @param by a limit of pass
#'
#' count
#' @import rgee
# Functions for extract the count of pixels of a rasterdata

extract_value_count <- function(x, y, by = 1000) {
  y_len <- y$size()$getInfo()
  for (i in seq(1, y_len, by)) {
    index <- i - 1
    print(sprintf("Extracting information [%s/%s]...", index, y_len))
    ee_value_layer <- ee$FeatureCollection(y) %>%
      ee$FeatureCollection$toList(by, index) %>%
      ee$FeatureCollection()
    if (i == 1) {
      dataset <- ee_extract(
        x = x,
        fun = ee$Reducer$count(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$count(),
        sf = T
      )
      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

#' Set of function to zonal statistic

#' @param x image of type Image o Image Collection
#'
#' @param y region of type Feacture o FeatureCollection
#'
#' @param by a limit of pass
#'
#' median
#' @import rgee
# Functions for extract the mean of pixels of a rasterdata

extract_value_median <- function(x, y, by = 1000) {
  y_len <- y$size()$getInfo()
  for (i in seq(1, y_len, by)) {
    index <- i - 1
    print(sprintf("Extracting information [%s/%s]...", index, y_len))
    ee_value_layer <- ee$FeatureCollection(y) %>%
      ee$FeatureCollection$toList(by, index) %>%
      ee$FeatureCollection()
    if (i == 1) {
      dataset <- ee_extract(
        x = x,
        fun = ee$Reducer$median(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$median(),
        sf = T
      )
      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

#' Set of function to zonal statistic
#' @param x image of type Image o Image Collection
#'
#' @param y region of type Feacture o FeatureCollection
#'
#' @param by a limit of pass
#'
#' median
#' @import rgee
# Functions for extract the mean of pixels of a rasterdata

extract_value_sd <- function(x, y, by = 1000) {
  y_len <- y$size()$getInfo()
  for (i in seq(1, y_len, by)) {
    index <- i - 1
    print(sprintf("Extracting information [%s/%s]...", index, y_len))
    ee_value_layer <- ee$FeatureCollection(y) %>%
      ee$FeatureCollection$toList(by, index) %>%
      ee$FeatureCollection()
    if (i == 1) {
      dataset <- ee_extract(
        x = x,
        fun = ee$Reducer$sd(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$sd(),
        sf = T
      )
      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}
