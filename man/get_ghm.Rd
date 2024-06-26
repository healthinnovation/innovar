% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ghm.R
\name{get_ghm}
\alias{get_ghm}
\title{Extract Global Human Modification data}
\usage{
get_ghm(region, fun = "count", scale = 1000)
}
\arguments{
\item{region}{region and object sf.}

\item{fun}{function for extract statistic zonal (\bold{count, kurtosis, max, mean, median, min, mod, percentile, std, sum, variance, first}).}

\item{scale}{A nominal scale in meters of the projection to work in.}
}
\value{
a tibble object with the new variable in km².
}
\description{
A function that extract Global Human Modification data of the year \bold{2016}
}
\examples{
\dontrun{

library(tidyverse)
library(rgee)
library(innovar)
library(sf)
ee_Initialize()

# 1. Reading a sf object
data("Peru")
region <- Peru
region_ee <- pol_as_ee(region, id = 'distr' ,simplify = 1000)

# 2. Extracting climate information
data <- region_ee \%>\%
get_ghm(
 fun = "max"
)
}
}
