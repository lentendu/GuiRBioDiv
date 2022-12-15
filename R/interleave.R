#' Interleave
#'
#' Interleave elements of one or two vectors
#'
#' @name interleave
#' @param v1 a vector to interleave the first half with the second half,
#'      or interleave with v2 if provided
#' @param v2 a vector to interleave with v2
#' @param rev reverse the order of v2
#' @return a vector with interleaved elements
#' @export
#'
interleave <- function(v1, v2, rev=F) {
  if(missing(v2)) {
    ord1 <- seq(1, length(v1), 2)
    ord2 <- seq(2, length(v1), 2)
    if(rev) ord2<-rev(ord2)
    v1[order(c(ord1, ord2))]
  } else {
    if(rev) v2<-rev(v2)
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1, v2)[order(c(ord1, ord2))]
  }
}
