#' list_algorithms
#'
#' A function that show all the available functions of lis package.
#'
#' @param category type of algorithm
#'
#' @details category.
#' \itemize{
#' \item \bold{climate}
#' \item \bold{environment}
#' \item \bold{human_intervention}
#' \item \bold{vulnerability_index}
#' \item \bold{utils}
#' \item \bold{graph}
#' \item \bold{vulnerability_index}
#' \item \bold{admin}
#' }
#'
#' @return  a string vector
#' @examples
#' \dontrun{
#'
#' library(lis)
#' {
#' # List all available algorithms
#' lis_algorithms
#' # by category
#' list_algorithms(category = "climate")
#' }
#' @export

lis_algorithms <- function(category = "all") {
  list_fun <- ls("package:lis")

  climate <- c(
    "get_climate", "get_etp"
    )

  environmental <- c(
    "get_co", "get_no2", "get_03",
    "get_so2", "get_aero",
    "get_vegetation"
  )

  human_interv <- c(
    "get_ghm", "get_pop", "get_urban",
    "get_nlv1", "get_nlv2", "get_def"
  )

  admin <- c("gen_admin.div")

  vulnerability <- c("vulnerability_index")

  graphs <- c(
    "lis_colors", "lis_cols", "lis_pal",
    "lis_palettes", "scale_color_lis",
    "scale_fill_lis"
  )
  utils <- c(
    "allpautils_names", "read_batch", "pol_as_ee"
    )

  if (category == "all") {
    pipe_rm <- list_fun |>
    endsWith(suffix = "%>%") |>
    which()
    list_fun[-pipe_rm] |>
    print()
  }

  else if (category == "climate") {
    index <- list_fun %in% climate |>
    which()
    list_fun[index]|>
    print()
  }

  else if (category == "environment") {
    index <- list_fun %in% environmental |>
    which()
    list_fun[index]|>
    print()
  }
  else if (category == "human_intervention") {
    index <- list_fun %in% human_interv |>
    which()
    list_fun[index]|>
    print()
  } else if (category == "admin") {
    index <- list_fun %in% admin |>
    which()
    list_fun[index]|>
    print()
  } else if (category == "graph") {
    index <- list_fun %in% graphs |>
    which()
    list_fun[index]|>
    print()
  } else if (category == "utils") {
    index <- list_fun %in% utils |>
    which()
    list_fun[index]|>
    print()
  } else {
    index <- list_fun %in% vulnerability |>
    which()
    list_fun[index]|>
    print()
  }
}
