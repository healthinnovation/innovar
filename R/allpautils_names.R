#' Remove accents and hyphens in character vector
#'
#' @description This function will allow you to remove accents, hyphens and
#'   any strange characters that come from the UTF-8 encoding, with the intention
#'   of working with standardized names or texts.Review:
#'   https://cran.r-project.org/web/packages/MazamaSpatialUtils/vignettes/MazamaSpatialUtils.html
#'
#' @param string Characters vector
#'
#' @examples
#' library(innovar)
#' string <- c("Perú", "Estados Unidos")
#' allpautils_names(string)
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
