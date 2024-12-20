#' Browse PR2 database taxonomy
#'
#' Determine the taxonomic rank of the provided clade name in the PR2 database
#'
#' @param x a clade name
#' @param clade output full clade for exact match, off by default
#' @param partial allow partial match, TRUE by default
#' @return a character string corresponding to the rank name or a named vector for exact match, a named list for partial match(es) or NA if not found
#'
#' @references
#' Guillou, L., Bachar, D., Audic, S., Bass, D., Berney, C., Bittner, L., Boutte, C. et al. 2013. The Protist Ribosomal Reference database (PR2): a catalog of unicellular eukaryote Small Sub-Unit rRNA sequences with curated taxonomy. Nucleic Acids Res. 41:D597–604.
#'
#' @export
#'
browse_pr2<- function( x, clade = FALSE , partial = TRUE ) {
  tmp_names<-colnames(pr2database::pr2_taxonomy())[1:9]
  # exact match fist
  for (i in tmp_names) {
    tmpt<-which(getElement(pr2database::pr2_taxonomy(),i) %in% x)
    if(any(tmpt)){
      if ( clade ) {
        return(unlist(unique(pr2database::pr2_taxonomy()[tmpt,1:which(tmp_names==i)])))
      } else {
        return(i)
      }
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
