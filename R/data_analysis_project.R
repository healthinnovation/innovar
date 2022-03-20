data_analysis_project <- function(path,
                                  project_type = "A",
                                  git_activate = FALSE,
                                  renv_activate = FALSE) {

  # ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  fs::dir_create(fs::path(path, "00_legacy"))
  fs::dir_create(fs::path(path, "01_data/proccessed"))
  fs::dir_create(fs::path(path, "01_data/raw"))
  fs::dir_create(fs::path(path, "02_output/plots"))
  fs::dir_create(fs::path(path, "02_output/tables"))
  fs::dir_create(fs::path(path, "02_output/reports"))
  fs::dir_create(fs::path(path, "03_functions"))

  if (project_type == "B") {
    fs::dir_create(fs::path(path, "04_analysis/scripts"))
    fs::dir_create(fs::path(path, "04_analysis/notebooks"))
  }

  if (git_activate) {
    gert::git_init(fs::path(path))
  }

  if (renv_activate) {
    renv::init(fs::path(path), restart = FALSE)
    path_innovar <- fs::path_package("innovar")
    message_install <- paste0("\nIf you want to install innovar package, use:\n",
                              "renv::use('", path_innovar, "')\n\n",
                              "Or if you want the latest version available:\n",
                              "renv::use('healthinnovation/innovar')")

    writeLines(paste0("source('renv/activate.R')\n\n",
                      "message(cat(\"", message_install, "\"))"),
               ".Rprofile",
               useBytes = TRUE)
  }

}

