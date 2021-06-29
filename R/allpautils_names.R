#' TIYLE
#'
#' Function 'allpautils_names' revisar: https://cran.r-project.org/web/packages/MazamaSpatialUtils/vignettes/MazamaSpatialUtils.html
#'
#' @param string vector of characters
#'
#' @examples df
#'
#' @export allpautils_names
allpautils_names <- function(string) {

  string <- gsub(pattern = "_",
                 replacement = " ",
                 x = gsub(pattern = "-",
                          replacement = " ",
                          x = iconv(string,
                                    from="UTF-8",
                                    to="ASCII//TRANSLIT"),
                          fixed=TRUE),
                 fixed=TRUE)

  return(string)
}
