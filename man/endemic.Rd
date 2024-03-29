% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/endemic.R
\name{endemic}
\alias{endemic}
\title{Endemic species}
\usage{
endemic(x, geo, splits = F, singleocc = T)
}
\arguments{
\item{x}{an occurrence or abundance matrix}

\item{geo}{a vector or factor of geographic units grouping the samples of the matrix}

\item{splits}{return a long data-frame with the list of endemic
species/OTUs, there geographic group and the number of sites in
which they occur}

\item{singleocc}{keep single occurrence species/OTUs, turn to FALSE to
remove them, or give a numeric to set the minimum occurrence}
}
\value{
a named vector of the number of species/OTUs of each sample uniquely found in
the geographic unit the sample is coming from
}
\description{
Identify endemic species in a community matrix based on geographic
grouping of samples
}
