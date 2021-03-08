#' TIYLE
#'
#' Function 'allpautils_names' revisar: https://cran.r-project.org/web/packages/MazamaSpatialUtils/vignettes/MazamaSpatialUtils.html
#'
#' @param x vector of characters
#'
#' @examples df
#'
#' @export allpautils_names
allpautils_names <- function(x) {

  x = gsub("_", " ",gsub("-", " ", iconv(x,
                                         from="UTF-8",
                                         to="ASCII//TRANSLIT"), fixed=TRUE), fixed=TRUE)

  return(x)
}
