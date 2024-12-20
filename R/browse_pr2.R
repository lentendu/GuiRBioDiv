#' Browse PR2 database taxonomy
#'
#' Determine the taxonomic rank of the provided clade name in the PR2 database
#'
#' @param x a clade name
#' @return a rank name or NA if not found
#'
#' @references
#' Guillou, L., Bachar, D., Audic, S., Bass, D., Berney, C., Bittner, L., Boutte, C. et al. 2013. The Protist Ribosomal Reference database (PR2): a catalog of unicellular eukaryote Small Sub-Unit rRNA sequences with curated taxonomy. Nucleic Acids Res. 41:D597–604.
#'
#' @export
#'
browse_pr2<- function(x) {
  for (i in colnames(pr2database::pr2_taxonomy())[1:9]) {
    if(length(grep(x,getElement(pr2database::pr2_taxonomy(),i)))){
      return(i)
      break
    }
  }
  return(NA)
}
