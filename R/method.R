#' Set of function to zonal statistic
#' @name ee_count
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the count values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_count <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$count(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_kurtosis
#' @param x image of type Image o Image Collection.
#' @param y region of type feacture or featureCollection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the kurtosis values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_kurstosis <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$kurtosis(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_max
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the maximum values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_max <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()%>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$max(),
        scale = scale,
        sf = T
      ) %>% st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_mean
#' @param x image of type Image o Image Collection.
#' @param y region of type feacture o feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the mean values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_mean <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble()%>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$mean(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

#' Set of function to zonal statistic
#' @name ee_median
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the count values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_median <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)
    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$median(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_min
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the minimum values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_min <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$min(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_mode
#' @param x image of type Image o Image Collection.
#' @param y region of type feature o feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the minimum values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_mode <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$mode(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_percentile
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the percentile values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_percentile <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$percentile(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_std
#' @param x image of type Image o Image Collection.
#' @param y region of type feacture or feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the standard deviation values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_std <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$stdDev(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_sum
#' @param x image of type Image o Image Collection.
#' @param y region of type feature o feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the sum values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all


ee_sum <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$sum(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_variance
#' @param x image of type Image o Image Collection.
#' @param y region of type feature o feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the variance values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all

ee_variance <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$variance(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


#' Set of function to zonal statistic
#' @name ee_first
#' @param x image of type Image o Image Collection.
#' @param y region of type feature or feature collection.
#' @param by a limit of pass by polygon.
#' @param scale A nominal scale in meters of the projection to work in.
#' @return a object sf with the first values.
#' @import rgee
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all


ee_first <- function(x, y, by = 1000,scale = 1000) {
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
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$first(),
        scale = scale,
        sf = T
      ) %>%
        st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_all(replace_na,0)

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

