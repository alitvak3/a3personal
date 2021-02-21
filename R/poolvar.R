#' Pooled Standard Deviation and Variance
#' 
#' @describeIn poolvar Pooled Variance
#' @export
pool.var <- function(n1, s1, n2, s2){
  numer <- (n1-1)*s1^2 + (n2-1)*s2^2
  denom <- n1+n2-2
  return(numer/denom)
}
#' @describeIn poolvar Pooled Standard Deviation
#' @export
pool.sd <- function(n1, s1, n2, s2){
  sqrt(pool.var(n1, s1, n2, s2))
}