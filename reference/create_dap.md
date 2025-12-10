# Create a Data Analysis Project

Create a skeleton for a data analysis project in 2 formats: report,
Report-centric (Rmd's); production, Modularised (separate scripts and
Rmd's). Use the \`create_dap()\` function for this.

## Usage

``` r
create_dap(
  dir,
  project_type = "report",
  git_activate = FALSE,
  renv_activate = FALSE
)
```

## Arguments

- dir:

  Directory for data analysis project.

- project_type:

  The type of project is indicated. The options are "reporting" or
  "production".

- git_activate:

  Configure the project so that it can use git.

- renv_activate:

  Configure the project so that it can use renv.

## Note

The \`dir\` parameter is required for the creation of the project. By
default a project of type report is configured.

## Examples

``` r
if (FALSE) { # \dontrun{
library(innovar)
create_dap("Epi_project", "report")
} # }
```
