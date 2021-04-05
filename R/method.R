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
# ee.Reducer.count()

ee_count <- function(x, y, by = 1000) {
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
#' kurtosis
#' @import rgee
# Functions for extract the mean of pixels of a rasterdata
# ee.Reducer.Kurtosis()

ee_kurstosis <- function(x, y, by = 1000) {
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
        fun = ee$Reducer$kurstosis(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$kurtosis(),
        sf = T
      )
      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


# ee.Reducer.max()
#' Set of function to zonal statistic
#' @param x image of type Image o Image Collection
#'
#' @param y region of type Feacture o FeatureCollection
#'
#' @param by a limit of pass
#'
#' median
#' @import  rgee
# Functions for extract the mean of pixels of a rasterdata

ee_max <- function(x, y, by = 1000) {
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
        fun = ee$Reducer$max(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$max(),
        sf = T
      )
      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


# ee.Reducer.mean()
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

ee_mean <- function(x, y, by = 1000) {
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
#' median
#' @import rgee
# Functions for extract the mean of pixels of a rasterdata
# ee.Reducer.median

ee_median <- function(x, y, by = 1000) {
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
# ee.Reducer.min()

ee_min <- function(x, y, by = 1000) {
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
        fun = ee$Reducer$min(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$min(),
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
# ee.Reducer.mode()

ee_mode <- function(x, y, by = 1000) {
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
        fun = ee$Reducer$mode(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$mode(),
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
# ee.Reducer.percentile

ee_percentile <- function(x, y, by = 1000) {
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
        fun = ee$Reducer$percentile(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$percentile(),
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
# ee.Reducer.stdDev

ee_std <- function(x, y, by = 1000) {
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
        fun = ee$Reducer$stdDev(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$stdDev(),
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
# ee.Reducer.sum

ee_sum <- function(x, y, by = 1000) {
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
#' @param x image of type Image o Image Collection
#'
#' @param y region of type Feacture o FeatureCollection
#'
#' @param by a limit of pass
#'
#' median
#' @import rgee
# Functions for extract the mean of pixels of a rasterdata
# ee.Reducer.variance()

ee_variance <- function(x, y, by = 1000) {
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
        fun = ee$Reducer$variance(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$variance(),
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
# ee.Reducer.first()

ee_first <- function(x, y, by = 1000) {
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
        fun = ee$Reducer$first(),
        y = ee_value_layer,
        sf = T
      )
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$first(),
        sf = T
      )
      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}
