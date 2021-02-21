#' Probability Between
#'
#' This function calculates probability between two qauntiles.
#' @export
#' @describeIn pbetween Normal
pbetween.norm <- function(q1, q2, mean = 0, sd = 1){
     pnorm(q2, mean, sd)-pnorm(q1, mean, sd)
}
#' @export
#' @describeIn pbetween Poisson
pbetween.pois <- function(q1, q2, l){
     ppois(q2, l)-ppois(q1, l)
}
#' @export
#' @describeIn pbetween Student's t
pbetween.t <- function(q1, q2, df){
     pt(q2, df)-pbinom(q1, df)
}
#' @export
#' @describeIn pbetween Binomial
pbetween.binom <- function(q1, q2, size, prob){
     pbinom(q2, size, prob)-pbinom(q1, size, prob)
}
