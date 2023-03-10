% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hill_index.R
\name{hill_index}
\alias{hill_index}
\title{Hill numbers}
\usage{
hill_index(x, id = "sample")
}
\arguments{
\item{x}{a species matrix of observation/reads counts (samples as row,
OTU/ASV/species as columns)}

\item{id}{name to give to the output column containing the rownames}
}
\value{
A data.frame with the sample names and the three indices,  each
in a column.
}
\description{
Wrapper for three alpha diversity indices in form of Hill numbers with
exponent 0, 1 and 2 corresponding to species richness,  exponential
of the Shannon index and inverse Simpson index
}
