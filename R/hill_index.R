#' Hill numbers
#'
#' Wrapper for three alpha diversity indices in form of Hill numbers with
#' exponent 0, 1 and 2 corresponding to species richness,  exponential
#' of the Shannon index and inverse Simpson index
#'
#' @param x a species matrix of observation/reads counts (samples as row,
#'      OTU/ASV/species as columns)
#' @param id name to give to the output column containing the rownames
#' @return A data.frame with the sample names and the three indices,  each
#'      in a column.
#' @export
#'
hill_index<-function(x, id="sample") {
  . <- NULL
  data.frame(
    hill0=vegan::specnumber(x),
    hill1=exp(vegan::diversity(x)),
    hill2=vegan::diversity(x, index="invsimpson") %>% replace(which(is.infinite(.)), 0)
  ) %>% tibble::rownames_to_column(id)
}
