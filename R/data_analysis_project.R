data_analysis_project <- function(path,
                                  project_type = project_types(),
                                  git_activate = FALSE,
                                  renv_activate = FALSE) {

  project_type = match.arg(project_type)

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
    renv::init(fs::path(path))
  }

}

project_types <- function() {
  c("A", "B")
}
