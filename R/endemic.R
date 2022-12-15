#' Endemic species
#'
#' Identify endemic species in a community matrix based on geographic
#' grouping of samples
#'
#' @param x an occurrence or abundance matrix
#' @param geo a vector or factor grouping the samples of the matrix
#' @param splits return a long data-frame with the list of endemic
#'      species/OTUs, there geographic group and the number of sites in
#'      which they occur
#' @param singleocc keep single occurrence species/OTUs, turn to FALSE to
#'      remove them, or give a numeric to set the minimum occurrence
#' @return the number of species/OTUs of each sample uniquely found in
#'      each geographic group
#' @import dplyr
#' @export
#'
endemic<-function(x, geo, splits=F, singleocc=T) {
  tmp<-data.frame(x, check.names=F) %>%
    tibble::rownames_to_column("sample") %>%
    tidyr::pivot_longer(-sample, names_to="sp", values_to="n") %>%
    filter(n>0) %>%
    left_join(data.frame(sample=rownames(x), geo=as.character(geo)), by="sample") %>%
    count(.data$sp, geo) %>%
    group_by(.data$sp) %>%
    filter(n()==1) %>%
    data.frame()
  if(is.integer(singleocc)){
    tmp<-filter(tmp, n>=singleocc)
  } else if(is.logical(singleocc)) {
    if(!singleocc) {
      tmp<-filter(tmp, n>1)
    }
  } else {
    stop(paste("unsupported value for singleocc option:", deparse(substitute(singleocc))))
  }
  if(splits){
    return(tmp)
  } else {
    rowSums(x[, tmp$sp]>0*1)
  }
}
