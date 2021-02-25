#' Score
#'
#' Calculates t-score or z-score
#' @param x value
#' @param mu mean
#' @param sd standard deviation
#' @param n number of obervations
#' @export
score <- function(x, mu, sd, n){
        (x-mu) / (sd/sqrt(n))
}
