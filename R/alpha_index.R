#' Compute standard alpha indices
#'
#' Wrapper for species richness, Shannon and Simpson diversity indices formatted to a data-frame
#'
#' @param x a species matrix of observation/reads counts (samples as row,
#'      OTU/ASV/species as columns)
#' @param id name to give to the output column containing the rownames
#' @return A data.frame with the sample names and the three indices, each in a column.
#' @export
#'
alpha_index<-function (x, id="sample") {
  cbind.data.frame(
    richness=vegan::specnumber(x),
    shannon=vegan::diversity(x),
    simpson=vegan::diversity(x,index="simpson")
  ) %>% tibble::rownames_to_column(id)
}
