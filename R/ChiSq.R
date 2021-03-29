#' Chi Square
#'
#' This function produces a table and runs a Chi Square test on counts of binned data or Poisson Ditrsibutions.
#'
#' @param bin.bounds A numeric vector for the boundaries of the bins. -Inf and positive Inf will automatically be added to each end.
#' @param Obs.Freq The counts in order of the intervals
#' @param mean The sample x-value  mean of the data
#' @param sd The standard deviation
#' @param NullDist The expected distribution under the null hypothesis. Currently supports normal and uniform
#' @param df Degrees of freedom
#' @export
#' @describeIn ChiSq Chi-Square for Binned Data
ChiSq.Bins <- function(bin.bounds,
                       ObsFreq,
                       mean,
                       sd,
                       NullDist = c("normal", "uniform"),
                       df = length(ObsFreq) - 1
                       ){
        if(length(ObsFreq) == length(bin.bounds) - 2)ObsFreq <- c(0, ObsFreq, 0)
        match.arg(NullDist)
        Lower <- c(-Inf, bin.bounds)
        Upper <- lead(Lower, 1)
        Upper[length(Lower)] <- Inf
        if(NullDist == "normal"){
                rExp <- pbetween.norm(Lower, Upper, mean, sd)
        }
        if(NullDist == "uniform"){
                rExp <- 1/length(ObsFreq)
        }
        nExp <- rExp * sum(ObsFreq)
        ChiSq <- (ObsFreq - nExp)^2/nExp
        results <- list(
                Table = tibble(
                        Lower,
                        Upper,
                        ObsFreq,
                        ExpRelFreq = rExp,
                        ExpFreq = nExp, ChiSq
                        ),
                ChiSqStat = sum(ChiSq),
                p.value = pchisq(sum(ChiSq), df, lower.tail = FALSE)
                )
        results
}
#' @param counts A numeric vector of the frequencies of occurence within the time unit.
#' @param Obs.Freq A numeric vector of the time units for each count.
#' @param lambda The parameter of the Poission distribution; will be calculated if not provided.
#' @param df Degrees of freedom
#' @export
#' @describeIn ChiSq Chi-Square for Binned Data
ChiSq.pois <- function(counts,
                       ObsFreq,
                       lambda = counts * ObsFreq / ObsFreq,
                       boundary = c("neither", "greater", "less"),
                       df = length(ObsFreq) - 1){
     boundary <- match.arg(boundary)
     rExp <- dpois(counts, lambda)
     if(boundary == "greater"){
          rExp[length(rExp)] <- ppois(counts[length(counts)-1],
                                      lambda,
                                      lower.tail = FALSE)
     }
     else if(boundary == "less"){
          rExp[1] <- ppois(counts[1],
                           lambda)
     }
     nExp <- rExp * sum(ObsFreq)
     ChiSq <- (ObsFreq - nExp)^2/nExp
     results <- list(
          Table = tibble(
               Lower,
               Upper,
               ObsFreq,
               ExpRelFreq = rExp,
               ExpFreq = nExp, ChiSq
          ),
          ChiSqStat = sum(ChiSq),
          p.value = pchisq(sum(ChiSq), df, lower.tail = FALSE)
     )
     results
}
