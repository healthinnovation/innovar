#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the count values.
#' @import rgee

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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$count(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feacture or featureCollection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the kurtosis values.
#' @import rgee

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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$kurtosis(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the maximum values.
#' @import rgee

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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$max(),
        sf = T
      ) %>% st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feacture o feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the mean values.
#' @import rgee

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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$mean(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

#' Set of function to zonal statistic
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the count values.
#' @import rgee

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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$median(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the minimum values.
#' @import rgee


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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$min(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

#' Set of function to zonal statistic
#' @param x image of type Image o Image Collection.
#' @param y region of type feature o feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the minimum values.
#' @import rgee


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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$mode(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}
#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the percentile values.
#' @import rgee


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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$percentile(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}
#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feacture or feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the standard deviation values.
#' @import rgee


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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$stdDev(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feature o feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the sum values.
#' @import rgee

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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$sum(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}
#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feature o feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the variance values.
#' @import rgee

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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$variance(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#'
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#'
#' @return a object sf with the first values.
#' @import rgee

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
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$first(),
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}
