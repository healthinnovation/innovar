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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

    } else {
      db_local <- ee_extract(
        x = x,
        y = ee_value_layer,
        fun = ee$Reducer$max(),
        scale = scale,
        sf = T
      ) %>% st_set_geometry(NULL) %>%
        as_tibble() %>%
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}


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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

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
        mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

      dataset <- rbind(dataset, db_local)
    }
  }
  return(dataset)
}
