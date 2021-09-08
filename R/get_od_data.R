#' Get census-like data in a origin-destination format
#'
#' @param x dataframe resulting from reading raw census data.
#' @param matrix logical. Should the output be in matrix form? Defaults to 'FALSE'.
#'
#' @return dataframe (or matrix) with origin-destination format.
#' @export
#' @import dplyr
#' @importFrom zoo na.locf
#' @importFrom stringr str_remove_all str_trim
#' @importFrom tidyr separate pivot_wider
#' @importFrom stringi stri_trans_general
#'
#' @examples
#' \dontrun{
#' library(readxl)
#' library(fs)
#' raw_path <- path("data", "test")
#' report_path <- path(raw_path, "reporte.xlsx")
#' report_raw <- read_excel(report_path, col_names = FALSE)
#'
#' od <- get_od_data(report_raw)
#' head(od)
#'
#' od_matrix <- get_od_data(report_path, matrix = TRUE)
#' dim(od_matrix)
#' }
get_od_data <- function(x, matrix = FALSE) {
  dat_raw <- x[, -1]
  last_row <- which(dat_raw[, 1] == "RESUMEN") - 1
  dat_raw <- dat_raw[1:last_row, 1:2]

  # Filter empty rows
  dat <- dat_raw[rowSums(is.na(dat_raw)) != ncol(dat_raw), ]
  colnames(dat) <- c("origin", "cases")

  # Create destiny location column
  dat$nchar <- nchar(dat$cases)
  dat$destiny <- ifelse(dat$nchar >= 10, as.character(dat$cases), NA)
  dat$destiny <- na.locf(dat$destiny)
  dat$nchar <- NULL

  # Create destiny location code column
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
      origin, into = c("reg_ori", "prov_ori", "distr_ori"), sep = ",",
      fill = "right"
    ) %>%
    separate(
      destiny, into = c("reg_des", "prov_des", "distr_des"), sep = ",",
      fill = "right"
    ) %>%
    mutate(
      reg_ori = str_remove_all(reg_ori, "Provincia Constitucional del "),
      reg_des = str_remove_all(reg_des, "Prov. Constitucional del ")
    ) %>%
    mutate(
      distr_ori = ifelse(reg_ori == "Callao", prov_ori, distr_ori),
      prov_ori = ifelse(reg_ori == "Callao", "Callao", prov_ori),
      distr_des = ifelse(reg_des == "Callao", prov_des, distr_des),
      prov_des = ifelse(reg_des == "Callao", "Callao", prov_des)
    ) %>%
    mutate_at(vars(distr_ori, distr_des), ~ str_remove_all(., "distrito: ")) %>%
    mutate_at(
      vars(distr_ori, prov_ori, reg_ori, distr_des, prov_des, reg_des),
      ~ str_trim(toupper(stri_trans_general(. , id = "Latin-ASCII")))
    ) %>%
    mutate_at(
      vars(distr_ori, prov_ori, reg_ori, distr_des, prov_des, reg_des),
      ~ str_remove_all(., "['~]")
    ) %>%
    mutate_at(
      vars(distr_ori, prov_ori, reg_ori, distr_des, prov_des, reg_des),
      ~ str_replace(., "[_-]", " ")
    ) %>%
    mutate(cases = as.numeric(cases))

  # Create data frame with code and names of the locations

  distr_ubigeo <-
    unique(od_raw[, c("reg_des", "prov_des", "distr_des", "ubigeo_des")])
  colnames(distr_ubigeo) <- c("reg_ori", "prov_ori", "distr_ori", "ubigeo_ori")

  # Merge to obtain the origin location codes
  od <- merge(od_raw, distr_ubigeo, by = c("reg_ori", "prov_ori", "distr_ori"))

  # Arrange columns
  od <- od[, c(
    "ubigeo_ori", "ubigeo_des", "cases", "reg_ori", "prov_ori",
    "distr_ori", "reg_des", "prov_des", "distr_des"
  )]

  # Arrange rows
  od <- arrange(od, ubigeo_ori, ubigeo_des)

  if (matrix) {
    od_long <-
      od %>%
      select(ubigeo_ori, ubigeo_des, cases) %>%
      pivot_wider(names_from = ubigeo_des, values_from = cases, values_fill = 0)

    od_matrix <- as.matrix(od_long[, -1])
    rownames(od_matrix) <- od_long$ubigeo_ori

    od_matrix
  } else {
    od
  }
}
