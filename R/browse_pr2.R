#' Browse PR2 database taxonomy
#'
#' Determine the taxonomic rank of the provided clade name in the PR2 database
#'
#' @param x a clade name
#' @param partial allow partial match, TRUE by default
#' @return a rank name for exact match, a named list for partial match(es) or NA if not found
#'
#' @references
#' Guillou, L., Bachar, D., Audic, S., Bass, D., Berney, C., Bittner, L., Boutte, C. et al. 2013. The Protist Ribosomal Reference database (PR2): a catalog of unicellular eukaryote Small Sub-Unit rRNA sequences with curated taxonomy. Nucleic Acids Res. 41:D597–604.
#'
#' @export
#'
browse_pr2<- function(x, partial=TRUE) {
  tmp_names<-colnames(pr2database::pr2_taxonomy())[1:9]
  # exact match fist
  for (i in tmp_names) {
    if(any(which(x %in% getElement(pr2database::pr2_taxonomy(),i)))){
      return(i)
      break
    }
  }
  # test partial match
  if (partial) {
    tmp_par<-lapply(tmp_names, function(i) {
      tmp<-getElement(pr2database::pr2_taxonomy(),i)
      unique(tmp[stringr::str_detect(tmp,x)])
    })
    names(tmp_par)<-tmp_names
    tmp_par<-tmp_par[which(sapply(tmp_par,length)>0)]
    if( length(tmp_par)>0 ) {
      tmp_par
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}
