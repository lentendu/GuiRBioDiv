#' Linear model correction of sequencing depth bias for alpha diversity indices
#'
#' Compute residuals of linear model between alpha diversity indices (hill numbers
#' or phylogenetic alpha diversity) and sample counts from a species/OTU matrix
#'
#' @param x is a species matrix of observation/reads counts (samples as row,
#'      OTU/ASV/species as columns)
#' @param counts are untransformed per sample observation counts provided in a
#'      named vector (same names as rownames of x). This parameter is optional,
#'      if not provided, row sums of input matrix will be used.
#' @param index either "hill" calling \code{\link{hill_index}} or "phylo" calling
#'      \code{\link{phylo_index}}
#' @param id name to give to the output column containing the rownames of x
#' @param trans define the desired read count's transformation. Either raw read
#'      counts (trans="no"), square-root transformed (trans="sqrt") or log
#'      transformed (trans="log"). Default is automatic selection for the best
#'      fit transformation (trans="auto").
#' @param scale the type of scaling to apply on residuals, either "no", "scale" for
#'      center-scale with \code{\link[base:scale]{base::scale()}}, or "range" for
#'      rescaling inside a fix range (-1:1) with \code{\link[scales:rescale]{scales::rescale()}}
#' @param inform if set to TRUE will print anova summary result of linear models
#'      for each diversity index
#' @param tree is needed for "phylo" index
#' @return A data.frame in a long format with sample names, the transformed per
#'      sample count of reads, the type of transformation used, the name of the
#'      diversity index, the raw value of the index, and the scaled residual value
#'      after linear model fitting against the transformed count of reads. If inform
#'      is turn to TRUE, the summary of the anova of best linear model for each
#'      index is printed in the terminal.
#' @importFrom stats lm residuals
#' @importFrom plyr ddply dlply laply ldply llply
#' @importFrom rlang .data
#' @import dplyr
#' @export
#' @seealso
#' [hill_index()] to compute Hill's numbers without correction
#' [phylo_index()] to compute phylogenetic alpha diversity indices without correction
#'
alpha_residual<-function(x, counts, id="sample", index="hill", trans="auto", tree=NULL, scale="scale", inform=F) {
  if (!is.null(tree) & index=="hill") {
    warning("phylogenetic tree not needed for Hill numbers")
  }
  if (! trans %in% c("no","sqrt","log","auto")) {
    stop("unknown trans option: ", trans)
  }
  if ( scale=="no") {
    my_scale<-function(x) x
  } else if ( scale=="scale" ) {
    my_scale<-function(x) base::scale(x)[,1]
  } else if ( scale=="range" ) {
    my_scale<-function(x) scales::rescale(x, to=c(-1,1))
  } else {
    stop("uknown scale option: ", scale)
  }
  if (index=="hill") {
    z=hill_index(x, id=id)
  } else if (index=="phylo") {
    if (is.null(tree))
      stop ("need a tree to compute phylo index")
    else
      z=phylo_index(x, tree, id=id)
  } else {
    stop ("unknown index option: ", index)
  }
  if (trans=="auto") {
    if (missing(counts)) {
      counts <- rowSums(x)
    } else if ( !all(sort(names(counts)) == sort(rownames(x))) ) {
      stop ("Mismatches between matrix rownames and count names")
    }
    counts <- list("no"=counts, "sqrt"=sqrt(counts), "log"=log(counts)) %>%
      ldply(function(i) {
        data.frame(counts=i) %>%
          tibble::rownames_to_column(id)
      }, .id="trans")
    tmp<-tidyr::gather(z, "index", "value", -all_of(id) ) %>%
      mutate(index=factor(index, levels=unique(index))) %>%
      left_join(counts, by=id)
    tmp_lm<-dlply(tmp, "index", function(i) {
      dlply(i, "trans", function(j) {
        lm(value ~ counts, data=j)
      })
    })
    tmp_best<-llply(tmp_lm, function(i) {
      if (any(laply(i, function(j) summary(j)$coef[2, 4])<0.05) ) {
        names(sort(laply(i, function(j) summary(j)$adj.r.square), decreasing=T)[1])
      } else {
        "no_lm"
      }
    })
    ddply(tmp, "index", function(i){
      tmpi<-unique(i$index)
      tmpb<-tmp_best[tmpi]
      if ( tmpb == "no_lm" ) {
        if ( inform ) {
          print( paste0( "index ", tmpi, " is not linearly correlated to counts, neither to log or sqrt transformed counts" ) )
        }
        filter( i, trans=="no" ) %>%
          select( -"trans" ) %>%
          mutate( residual=my_scale(.data$value) )
      } else {
        if ( inform ) {
          print( paste0( "index ", tmpi,
                         " best linear correlation to counts is with ", tmpb,
                         " transformation: Adj. R2 = ",
                         round(summary(tmp_lm[[tmpi]][[unlist(tmpb)]])$adj.r.squared, digits=3),
                         " ;  p.value = ",
                         scales::scientific(summary(tmp_lm[[tmpi]][[unlist(tmpb)]])$coef[2, 4], digits = 3)))
        }
        filter(i, trans==tmpb) %>%
          mutate(residual=my_scale( residuals( lm( .data$value ~ .data$counts ) ) ) ) %>%
          select(-trans)
      }
    })
  } else {
    if (missing(counts)) {
      counts <- rowSums(x)
    } else if ( !all(sort(names(counts)) == sort(rownames(x))) ) {
      stop("Mismatches between matrix rownames and count names")
    }
    if (trans=="sqrt") {
      counts <- sqrt(counts)
    } else if (trans=="log") {
      counts <- log(counts)
    }
    tmp<-tidyr::gather(z, "index", "value", -all_of(id) ) %>%
      mutate(index=factor(index, levels=unique(index))) %>%
      left_join(tibble::rownames_to_column(data.frame(counts=counts), id), by=id)
    ddply(tmp, "index", function(i){
      tmpi<-unique(i$index)
      tmp_lm<-summary(lm(value ~ counts, data=i))
      if (tmp_lm$coefficients[2, 4]<0.05 & tmp_lm$adj.r.squared>0.1) {
        if ( inform ) {
          print( paste0( "index ", tmpi,
                         " linear correlation to counts with ", trans,
                         " transformation is: Adj. R2 = ",
                         round(tmp_lm$adj.r.squared, digits=3),
                         " ;  p.value = ",
                         scales::scientific(tmp_lm$coef[2, 4], digits = 3)))
        }
        i %>% mutate(residual=my_scale( residuals( lm( .data$value ~ .data$counts ) ) ) )
      } else {
        if ( inform ) {
          if (trans == "no") {
            print( paste0( "index ", tmpi, " is not linearly correlated to counts" ) )
          } else {
            print( paste0( "index ", tmpi, " is not linearly correlated to ", trans, " transformed counts" ) )
          }
        }
        i %>% mutate(residual=my_scale(.data$value), counts=rowSums(x))
      }
    })
  }
}
