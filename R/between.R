#' Probability between
#'
#' This fucntion calculates probability between two qauntiles.
#' Method can be set to "norm" for normal, "pois" for Poisson, "binom" for binomial, or "t" for t.
pbetween <- function(q1, q2, m = 0, s = 1, l, size, p, df, method = "nomral"){
     if(method == "norm"){
          pnorm(q2, m, s)-pnorm(q1, m, s)
     } else if(method == "pois"){
          ppois(q2, l)-ppois(q1, l)
     } else if(method =="binom"){
          pbinom(q2, size, p)-pbinom(q1, size, p)
     } else if(method =="t"){
          pt(q2, df)-pbinom(q1, df)
     }
}
