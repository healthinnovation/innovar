#' Multiple import
#'
#' https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/#'
#' @param argument_1
#' @param argument_2
#'
#' Import multiple files
#'
#' @examples data_dir <- "~/Dropbox/Work/COVID19/Proyectos/MINEDU/LIS-MINEDU_Shared/Indicadores/3.2. Reapertura 2021/Output data/"
#'
#' read_batch(data_dir = data_dir)
#' read_batch(data_dir = data_dir, extension = "dta", fun = haven::read_dta)
#' read_batch(data_dir = data_dir, extension = "dta", fun = haven::read_dta, env = F)
#'
#' @import fs
#' @import tidyverse
#' @export import_db
read_batch <- function(data_dir, extension = "csv", fun = readr::read_csv, env = TRUE, ...) {

  file <- fs::dir_ls(data_dir, regexp = paste0("\\.", extension, "$"))

  data_objects<-file %>%
    map(fun, ...)

  names(data_objects) <- sub('\\..*$', '', basename(file))

  if (isTRUE(env)) {
    list2env(data_objects,envir=.GlobalEnv)
  } else {
    assign("data_list", data_objects, envir=globalenv())
  }

}
