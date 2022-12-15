#' Phylogenetic alpha diversity indices
#'
#' Wrapper for three diversity metrics PD,  PSV and PSE for a species
#' matrix (species as columns) and a tree
#'
#' @param x a species matrix of observation/reads counts (samples as row,
#'      OTU/ASV/species as columns)
#' @param id name to give to the output column containing the rownames
#' @param tre phylogenetic tree (i.e. an object of class "phylo")
#' @return A data.frame with the sample names and the three phylogenetic
#'      indices,  each in a column.
#' @export
#'
phylo_index<-function(x, id="sample",  tre){
  cbind.data.frame(
    PD=picante::pd(x, tre)$PD,
    PSV=picante::psv(x, tre, compute.var=F)$PSVs,
    PSE=picante::pse(x, tre)$PSEs
  ) %>%
    tibble::rownames_to_column(id)
}
