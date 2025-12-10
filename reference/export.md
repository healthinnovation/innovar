# Export CSV file to MongoDB

This a Python wrapper of the `insert_many()` method of package pymongo
to insert a list of documents as a collection in a database in MongoDB.

## Usage

``` r
export(filepath, dbname, user, password, interpreter = NULL)
```

## Arguments

- filepath:

  CSV file path.

- dbname:

  Database name.

- user:

  User name.

- password:

  Password.

- interpreter:

  Path of the Python interpreter (optional)

## Value

Confirmation message if exporting was successful. Else an error.

## Examples

``` r
if (FALSE) { # \dontrun{
library(innovar)
data("cars")
write.csv(cars, "cars.csv", row.names = FALSE)
dbname = "<database-name>"
user <- Sys.getenv("USER")
password <- Sys.getenv("PASSWORD")
export("cars.csv", dbname, user, password)
} # }
```
