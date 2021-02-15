#' Confidence interval
#'
#' This function creates a confidence interval.
#' Methods include "t" or "z".
CI <- function(point, SE, df, level = .95, method = "t"){
     if(method == "t"){
          precision <- qt(level+(1-level)/2, df) * SE
     } else if (method == "z"){
          precision <- qnorm(level+(1-level)/2) * SE
     }
     lower <- point - precision
     upper <- point + precision
     int <- c(point, precision, lower, upper)
     names(int) <- c("point", "precision", "lower", "upper")
     return(int)
}
