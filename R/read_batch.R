#' Utility for dataset batch importing  in multiple formats
#'
#' Reference: https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/#'
#' @param data_dir data_dir is the file path that contains the files that will be imported
#' @param ext extension "*.ext" of the files that will be imported. Default "csv"
#' @param fun the import function that will be passsed to `map` to import datasets. Must be able to read files with the `ext` extension
#' @param env Whether the file will be imported to .GlobalEnv. If `FALSE` a list containing all the dataset will be generated
#' @param ... Arguments passed to the import function
#'
#' Import multiple files
#'
#' @examples
#'\dontrun{
#' read_batch(data_dir = data_dir)
#' read_batch(data_dir = data_dir, extension = "dta", fun = haven::read_dta)
#' read_batch(data_dir = data_dir, extension = "dta", fun = haven::read_dta, env = F)
#'}
#' @importFrom purrr map
#' @importFrom fs dir_ls
#' @importFrom readr read_csv
#' @export
read_batch <- function(data_dir, ext = "csv", fun = readr::read_csv, env = TRUE, ...) {

  file <- dir_ls(data_dir, regexp = paste0("\\.", ext, "$"))

  data_objects<-file %>%
    map(fun, ...)

  names(data_objects) <- sub('\\..*$', '', basename(file))

  if (isTRUE(env)) {
    list2env(data_objects,envir=.GlobalEnv)
  } else {
    assign("data_list", data_objects, envir=as.environment())
  }

}
