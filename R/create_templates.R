#' Create a Data Analysis Project
#'
#' Create a skeleton for a data analysis project in 2
#' formats: report, Report-centric (Rmd's); production,
#' Modularised (separate scripts and Rmd's). Use the
#' `create_dap()` function for this.
#'
#' @param dir Directory for data analysis project.
#' @param project_type The type of project is indicated.
#'   The options are "reporting" or "production".
#' @param git_activate Configure the project so that it
#'   can use git.
#' @param renv_activate Configure the project so that it
#'   can use renv.
#'
#' @note The `dir` parameter is required for the creation
#'   of the project. By default a project of type report is configured.
#'
#' @examples
#' \dontrun{
#' library(innovar)
#' create_dap("Epi_project", "report")
#' }
#' @export

create_dap <- function(dir,
                       project_type = "report",
                       git_activate = FALSE,
                       renv_activate = FALSE) {
  # Check if it's called from console
  if (missing(dir) & interactive()) {
    dir <- getwd()
  } else {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  fs::dir_create(fs::path(dir, "00_legacy"))
fs::dir_create(fs::path(dir, "01_data/processed"))
  fs::dir_create(fs::path(dir, "01_data/raw"))
  fs::dir_create(fs::path(dir, "02_output/plots"))
  fs::dir_create(fs::path(dir, "02_output/tables"))
  fs::dir_create(fs::path(dir, "02_output/reports"))
  fs::dir_create(fs::path(dir, "03_functions"))

  if (project_type == "production") {
    fs::dir_create(fs::path(dir, "04_analysis/scripts"))
    fs::dir_create(fs::path(dir, "04_analysis/notebooks"))
  }

  if (git_activate) {
    gert::git_init(fs::path(dir))
  }

  if (renv_activate) {
    renv::init(fs::path(dir), restart = FALSE)
    path_innovar <- fs::path_package("innovar")
    message_install <- paste0(
      "\nIf you want to install innovar package, use:\n",
      "renv::use('", path_innovar, "')\n\n",
      "Or if you want the latest version available:\n",
      "renv::use('healthinnovation/innovar')"
    )

    writeLines(
      paste0(
        "source('renv/activate.R')\n\n",
        "message(cat(\"", message_install, "\"))"
      ),
      ".Rprofile",
      useBytes = TRUE
    )
  }
}

data_analysis_project <- function(dir, ...) {
  params <- list(...)
  create_dap(
    dir, params$project_type,
    params$git_activate,
    params$renv_activate
  )
}

#' Use Innovar Xaringan Template
#'
#' This function populates the working directory with
#' files corresponding to the xaringan template of the
#' package.
#'
#' @param file Name of the `.Rmd` file to create.
#'
#' @examples
#' \dontrun{
#' library(innovar)
#' use_xaringan("class_01.Rmd")
#' }
#' @export

use_xaringan <- function(file) {
  if (missing(file)) {
    if (interactive()) {
      file <- readline("Enter a name for the file .Rmd: ")
    } else {
      stop("file argument must be specified", call. = FALSE)
    }
  }

  if (tolower(fs::path_ext(file)) != "rmd") {
    file <- fs::path_ext_set(fs::path_ext_remove(file), "Rmd")
  }

  rmarkdown::draft(file,
    template = "innovar-xaringan",
    package = "innovar",
    edit = FALSE
  )
}


#' Use Innovar Rmarkdown Flatly
#'
#' This function populates the working directory with
#' files corresponding to the Innovar Rmarkdown Flatly.
#'
#' @param file Name of the `.Rmd` file to create.
#'
#' @examples
#' \dontrun{
#' library(innovar)
#' use_rmd_flatly("report_01.Rmd")
#' }
#' @export

use_rmd_flatly <- function(file) {
  if (missing(file)) {
    if (interactive()) {
      file <- readline("Enter a name for the file .Rmd: ")
    } else {
      stop("file argument must be specified", call. = FALSE)
    }
  }

  if (tolower(fs::path_ext(file)) != "rmd") {
    file <- fs::path_ext_set(fs::path_ext_remove(file), "Rmd")
  }

  rmarkdown::draft(file,
                   template = "innovar-rmd-flatly",
                   package = "innovar",
                   edit = FALSE
  )
}
