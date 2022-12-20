#' Format PERMANOVA results
#'
#' Transform PERMANOVA aov table as produced by \code{\link[vegan:adonis2]{vegan::adonis2()}}
#' into a tidy data-frame
#'
#' @param x an object of class anova.cca as produced by \code{\link[vegan:adonis2]{vegan::adonis2()}}
#' @param digits number of digits to round R^2 values
#' @return A data.frame with the parameters the rounded R^2 and
#'      stars denoting p-value significance (* for p<0.05, ** for
#'      p<0.01, *** for p<0.001)
#' @importFrom rlang .data
#' @import dplyr
#' @export
#' @examples
#' x<-data.frame(group=c("B","B","A","A","A","B","B","A","B","A"))
#' tmp<-vegan::adonis2(mat_exa ~ group, data = x)
#' permtab(tmp)
#'
permtab<-function(x,digits=2) {
  suppressWarnings(broom::tidy(x)) %>%
    rename(pval="p.value") %>%
    select("term","R2","pval") %>%
    filter("term"!="Total") %>%
    mutate(R2=round(.data$R2,digits=digits),
           pval=ifelse(is.na(.data$pval),"",
                       ifelse(.data$pval<=0.001,"***",
                              ifelse(.data$pval<0.01,"**",
                                     ifelse(.data$pval<0.05,"*","")))))
}
