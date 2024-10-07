#' Remove accents and hyphens in character vector
#'
#' @description This function will allow you to \bold{remove accents, hyphens and
#'   any strange characters that come from the UTF-8 encoding}, with the intention
#'   of working with standardized names or texts.
#'   For information, please refer to the following link \url{https://cran.r-project.org/web/packages/MazamaSpatialUtils/vignettes/MazamaSpatialUtils.html}
#'
#' @param string A character vector.
#'
#' @returns A character vector standardized without accents or hyphens and any strange characters.
#'
#' @examples
#' library(innovar)
#' string <- c("Perú", "Estados Unidos")
#' allpautils_names(string)
#'
#' @export allpautils_names
allpautils_names <- function(string = NULL) {

  # Check for NA values
  if (is.null(string) || length(string) == 0) {
    stop("Please insert a valid character vector.")
  }

  string <- gsub(
    pattern = "_",
    replacement = " ",
    x = gsub(
      pattern = "-",
      replacement = " ",
      x = iconv(
        string,
        from="UTF-8",
        to="ASCII//TRANSLIT"
        ),
      fixed=TRUE),
    fixed=TRUE)

  return(string)
}
