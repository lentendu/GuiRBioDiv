#' Tukey's Ladder of Powers transformation
#'
#' Transform a numeric vector with Tukey's Ladder of Powers (also know
#' as Box-Cox transformation).
#' Code adapted from \code{\link[rcompanion:transformTukey]{rcompanion::transformTukey()}}
#'
#' @param x a numeric vector to transform
#' @param lambda a numeric vector of exponents to screen in order
#'      to find the transformation wit best normality
#' @param onlynn if TRUE, only perform the transformation if the
#'      input vector is not already normal, otherwise return x
#' @param naomit if TRUE, perform the transformation on non NA values,
#'      else return an error
#' @return a list with four elements:
#' \itemize{
#'   \item val - a vector of transformed values
#'   \item trans - the transformation applied
#'   \item W - the value of the Shapiro-Wilk statistic of the transformed values
#'   \item p - the p-value of the Shapiro-Wilk test of normality of the transformed values
#' }
#' @references http://rcompanion.org/handbook/I_12.html
#' @importFrom stats shapiro.test
#' @importFrom R.utils insert
#' @export
#' @examples
#' set.seed(2)
#' transformTukey2(rnorm(10))
#'
#' # avoid transformation if already normal
#' set.seed(2)
#' transformTukey2(rnorm(10), onlynn = TRUE)
#'
transformTukey2<-function(x, lambda=seq(-2,2,0.05), onlynn = FALSE, naomit = FALSE) {
  # test if NA
  if(any(is.na(x))) {
    if (naomit) {
      nax<-which(is.na(x))
      x<-x[!is.na(x)]
    } else {
      stop("data contains NA")
    }
  }
  # test normality and continue with transformation only if non-normal or return untransformed values otherwise
  if(onlynn) {
    tmp<-shapiro.test(x)
    if(tmp$p.value>0.05) {
      return(c(list(val=x, trans="x", W=signif(tmp$statistic, digits=4), p=signif(tmp$p.value, digits=4))))
    }
  }
  ladder<-plyr::ldply(lambda, function(y) {
    if(y==0) {
      if(any(x<=0)) {trans<-log(x+abs(min(x))+1e-5)} else {trans<-log(x)} ; label<-"log(x)"
    }
    else if(y==1) {trans<-x;label<-"x"}
    else {
      if(any(x<=0)) {trans<-sign(x)*abs(x)^y} else {trans<-x^y} ; label<-paste0("x^",signif(y))
    }
    tmp<-shapiro.test(trans)
    return(cbind.data.frame(trans=label, W=signif(tmp$statistic,digits=4), p=signif(tmp$p.value, digits=4)))
  })
  TRANS<-dplyr::mutate(dplyr::arrange(ladder, desc(.data$W), desc(.data$p)), trans=as.character(.data$trans))[1,]
  # apply transformation formula
  if(any(x<=0)) {
    out<-c(list(val=eval(parse(text=sub("x","sign(x)*abs(x)", TRANS$trans)))), TRANS)
  } else {
    out<-c(list(val=eval(parse(text=TRANS$trans))), TRANS)
  }
  # integrate back NA
  if (exists("nax")) {
    for(i in nax) {out$val<-insert(out$val,i,NA)}
  }
  return(out)
}
