% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_od_data.R
\name{get_od_data}
\alias{get_od_data}
\title{Get census-like data in a origin-destination format}
\usage{
get_od_data(x, wide = FALSE, sparse = TRUE)
}
\arguments{
\item{x}{A data frame resulting from reading a raw query on the
Peru census of 2017 data containing origin and destination locations.
More information on the Details section.}

\item{wide}{logical. Should the output be in wide format? If 'TRUE', a matrix
is returned with the origins in rows and the destinations in columns.
Defaults to 'FALSE'.}

\item{sparse}{logical. When the output is in wide format (i.e. \code{wide = TRUE}),
should a sparse matrix be returned? Defaults to 'TRUE'.}
}
\value{
Data frame (or matrix) with origin-destination format.
}
\description{
Process a data frame resulting from reading a raw query on the
Peru census of 2017 data containing origin and destination locations in a
tidy long format or in a (sparse) matrix form.
}
\details{
The Institute of Statistics and Informatics (INEI, by its acronym in Spanish)
of Peru carried out the last census on 2017. This census data can be
queried in the \href{https://censos2017.inei.gob.pe/redatam/}{2017 Census RADATAM platform}.
A query result can be downloaded as a Excel workbook (.xlsx).

The \code{get_od_data} function aids to process the raw query results on the
2017 census data involving an origin and a destination location (district
or province level). For example, when querying the question \emph{Distrito o país
donde vivía hace 5 años} (District or country where you used to live 5 years
ago) at a district level, one gets for every district a list with all the districts
or countries given as a answer for this question and their frequencies (number
of people living in district A that used to live in district B 5 years ago).

The raw report obtained by querying the question \emph{Distrito o país
donde vivía hace 5 años} is provided in this package as an example dataset
under the name \code{\link{migration17raw}}. In the examples section below, we
show how to use the \code{get_od_data} function to process this raw dataset to
get different types of outputs to analyze its origin-destination information.
}
\examples{
\dontrun{
data("migration17raw")

# Check that this raw data set is the result of reading an Excel file without
# column names
head(migration17raw)

# Origin-destination data in long format (data frame)
od_long <- get_od_data(migration17raw)
head(od_long)

# Origin-destination data in wide format (sparse matrix)
od_wide_sparse <- get_od_data(migration17raw, wide = TRUE)
od_wide_sparse[1:5, 1:5]
print(object.size(od_wide_sparse), units = "auto") # This is lighter

# Origin-destination data in wide format (regular matrix)
od_wide <- get_od_data(migration17raw, wide = TRUE, sparse = FALSE)
od_wide[1:5, 1:5]
print(object.size(od_wide), units = "auto") # This is heavier
}
}
\seealso{
\code{\link{migration17raw}}
}
