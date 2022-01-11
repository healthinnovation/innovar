#' Export CSV file to MongoDB
#'
#' This a Python wrapper of the \code{insert_many()} method of package pymongo to insert a list of documents as a collection in a database in MongoDB.
#'
#' @param filepath CSV file path.
#' @param dbname Database name.
#' @param user User name.
#' @param password Password.
#' @param interpreter Path of the Python interpreter (optional)
#'
#' @return Confirmation message if exporting was successful. Else an error.
#' @importFrom reticulate use_python source_python
#'
#' @examples
#' \dontrun{
#' library(innovar)
#' data("cars")
#' write.csv(cars, "cars.csv", row.names = FALSE)
#' dbname = "<database-name>"
#' user <- Sys.getenv("USER")
#' password <- Sys.getenv("PASSWORD")
#' export("cars.csv", dbname, user, password)
#' }
#' @export
export <- function(filepath, dbname, user, password, interpreter = NULL) {
  if (is.null(filepath)) stop("File path (filepath) is required.")

  if (is.null(dbname)) stop("Database names (dbname) is required.")

  if (is.null(user)) stop("User (user) is required.")

  if (is.null(password)) stop("Password (password) is required.")

  if (!is.null(interpreter)) use_python(interpreter, required = TRUE)

  source_python("./py/export.py")

  arg <- list(filepath, dbname, user, password)
  status <- do.call(export, arg)

  if (status) {
    print("Collection was exported succesfully.")
  } else {
    print("Error: Exporting failed.")
  }
}
