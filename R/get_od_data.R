#' Get census-like data in a origin-destination format
#'
#' Process a data frame resulting from reading a raw query on the
#' Peru census of 2017 data containing origin and destination locations in a
#' tidy long format or in a (sparse) matrix form.
#'
#' @param x A data frame resulting from reading a raw query on the
#' Peru census of 2017 data containing origin and destination locations.
#' More information on the Details section.
#' @param wide logical. Should the output be in wide format? If 'TRUE', a matrix
#' is returned with the origins in rows and the destinations in columns.
#' Defaults to 'FALSE'.
#' @param sparse logical. When the output is in wide format (i.e. \code{wide = TRUE}),
#' should a sparse matrix be returned? Defaults to 'TRUE'.
#'
#' @return Data frame (or matrix) with origin-destination format.
#' @export
#' @import dplyr
#' @importFrom zoo na.locf
#' @importFrom stringr str_remove_all str_trim str_replace
#' @importFrom tidyr separate pivot_wider
#' @importFrom stringi stri_trans_general
#' @importFrom Matrix Matrix
#'
#' @details
#' The Institute of Statistics and Informatics (INEI, by its acronym in Spanish)
#' of Peru carried out the last census on 2017. This census data can be
#' queried in the \href{https://censos2017.inei.gob.pe/redatam/}{2017 Census RADATAM platform}.
#' A query result can be downloaded as a Excel workbook (.xlsx).
#'
#' The \code{get_od_data} function aids to process the raw query results on the
#' 2017 census data involving an origin and a destination location (district
#' or province level). For example, when querying the question \emph{Distrito o país
#' donde vivía hace 5 años} (District or country where you used to live 5 years
#' ago) at a district level, one gets for every district a list with all the districts
#' or countries given as a answer for this question and their frequencies (number
#' of people living in district A that used to live in district B 5 years ago).
#'
#' The raw report obtained by querying the question \emph{Distrito o país
#' donde vivía hace 5 años} is provided in this package as an example dataset
#' under the name \code{\link{migration17raw}}. In the examples section below, we
#' show how to use the \code{get_od_data} function to process this raw dataset to
#' get different types of outputs to analyze its origin-destination information.
#'
#' @seealso \code{\link{migration17raw}}
#' @examples
#' \dontrun{
#' data("migration17raw")
#'
#' # Check that this raw data set is the result of reading an Excel file without
#' # column names
#' head(migration17raw)
#'
#' # Origin-destination data in long format (data frame)
#' od_long <- get_od_data(migration17raw)
#' head(od_long)
#'
#' # Origin-destination data in wide format (sparse matrix)
#' od_wide_sparse <- get_od_data(migration17raw, wide = TRUE)
#' od_wide_sparse[1:5, 1:5]
#' print(object.size(od_wide_sparse), units = "auto") # This is lighter
#'
#' # Origin-destination data in wide format (regular matrix)
#' od_wide <- get_od_data(migration17raw, wide = TRUE, sparse = FALSE)
#' od_wide[1:5, 1:5]
#' print(object.size(od_wide), units = "auto") # This is heavier
#' }
get_od_data <- function(x, wide = FALSE, sparse = TRUE) {
  dat_raw <- x[, -1]
  last_row <- which(dat_raw[, 1] == "RESUMEN") - 1
  dat_raw <- dat_raw[1:last_row, 1:2]

  # Filter empty rows
  dat <- dat_raw[rowSums(is.na(dat_raw)) != ncol(dat_raw), ]
  colnames(dat) <- c("origin", "cases")

  # Create destination location column
  dat$nchar <- nchar(dat$cases)
  dat$destination <- ifelse(dat$nchar >= 10, as.character(dat$cases), NA)
  dat$destination <- na.locf(dat$destination)
  dat$nchar <- NULL

  # Create destination location code column
  dat$ubigeo_des <- ifelse(
    grepl("AREA # ", dat$origin) == TRUE,
    str_remove_all(dat$origin, "AREA # "),
    NA
  )
  dat$ubigeo_des <- na.locf(dat$ubigeo_des)

  dat <- dat[
    !(
      dat$origin == "Total" | dat$origin == "No Aplica :" |
        grepl("especificado", dat$origin) == TRUE |
        grepl("Continente", dat$origin) == TRUE
    ),
  ]

  del <- which(grepl("AREA # ", dat$origin) == TRUE)
  del <- sort(c(del, del + 1))

  dat <- dat[-del, ]
  dat <- dat[!(grepl("[0-9]", dat$origin) == TRUE), ]

  # Format string columns

  od_raw <-
    dat %>%
    separate(
      origin, into = c("dept_ori", "prov_ori", "distr_ori"), sep = ",",
      fill = "right"
    ) %>%
    separate(
      destination, into = c("dept_des", "prov_des", "distr_des"), sep = ",",
      fill = "right"
    ) %>%
    mutate(
      dept_ori = str_remove_all(dept_ori, "Provincia Constitucional del "),
      dept_des = str_remove_all(dept_des, "Prov. Constitucional del ")
    ) %>%
    mutate(
      distr_ori = ifelse(dept_ori == "Callao", prov_ori, distr_ori),
      prov_ori = ifelse(dept_ori == "Callao", "Callao", prov_ori),
      distr_des = ifelse(dept_des == "Callao", prov_des, distr_des),
      prov_des = ifelse(dept_des == "Callao", "Callao", prov_des)
    ) %>%
    mutate_at(vars(distr_ori, distr_des), ~ str_remove_all(., "distrito: ")) %>%
    mutate_at(
      vars(distr_ori, prov_ori, dept_ori, distr_des, prov_des, dept_des),
      ~ str_trim(toupper(stri_trans_general(. , id = "Latin-ASCII")))
    ) %>%
    mutate_at(
      vars(distr_ori, prov_ori, dept_ori, distr_des, prov_des, dept_des),
      ~ str_remove_all(., "['~]")
    ) %>%
    mutate_at(
      vars(distr_ori, prov_ori, dept_ori, distr_des, prov_des, dept_des),
      ~ str_replace(., "[_-]", " ")
    ) %>%
    mutate(cases = as.numeric(cases))

  # Create data frame with code and names of the locations

  distr_ubigeo <-
    unique(od_raw[, c("dept_des", "prov_des", "distr_des", "ubigeo_des")])
  colnames(distr_ubigeo) <- c("dept_ori", "prov_ori", "distr_ori", "ubigeo_ori")

  # Merge to obtain the origin location codes
  od <- merge(od_raw, distr_ubigeo, by = c("dept_ori", "prov_ori", "distr_ori"))

  # Arrange columns
  od <- od[, c(
    "ubigeo_ori", "ubigeo_des", "cases", "dept_ori", "prov_ori",
    "distr_ori", "dept_des", "prov_des", "distr_des"
  )]

  # Arrange rows
  od <- arrange(od, ubigeo_ori, ubigeo_des)

  if (wide) {
    od_wide <-
      od %>%
      select(ubigeo_ori, ubigeo_des, cases) %>%
      pivot_wider(names_from = ubigeo_des, values_from = cases, values_fill = 0)

    od_wide_matrix <- as.matrix(od_wide[, -1])
    rownames(od_wide_matrix) <- od_wide$ubigeo_ori

    if (sparse) {
      od_wide_matrix_sparse <- Matrix(od_wide_matrix, sparse = TRUE)
      od_wide_matrix_sparse
    } else {
      od_wide_matrix
    }

  } else {
    od
  }
}
