#' Power Functions
#'
#' These functions create a tibble showing the power at different true means.
#' @param mus a list or vector of true means or differences to be tested
#' @param mu.0 the hypothesized mean or difference
#' @param sd standard deviation
#' @param n number of observations
#' @param samples number of samples (1 or 2)
#' @param alternative "two.sided", "less", or "greater", abbreviations accepted
#' @describeIn power creates a power function table based on the z method
#' @export
power_func.z <-
  function(mus,
           mu.0,
           sd,
           n,
           conf.level = 0.95,
           alternative = c("two.sided", "less",
                           "greater")) {
    alt<- match.arg(alternative)
    SEM <- sd / sqrt(n)
    alpha <- 1 - conf.level

    if (is.null(df) == TRUE) {
      df <- n - 1
    }

    if (alt == "two.sided") {
      C1 <- qnorm(alpha / 2, mu.0, SEM)
      C2 <- qnorm(1 - alpha / 2, mu.0, SEM)
      pwr.l <- sapply(mus[mus <= mu.0], function(x) {
          pnorm(C1, mean = x, sd = SEM)
      })
      pwr.h <- sapply(mus[mus > mu.0], function(x) {
          pnorm(C2,
                mean = x,
                sd = SEM,
                lower.tail = FALSE)
      })
      pwr <- c(pwr.l, pwr.h)
        }
   if (alt == "less") {
        C1 <- qnorm(alpha, mu.0, SEM)
        pwr <- sapply(mus, function(x) {
            pnorm(C1, mean = x, sd = SEM)
          })
   }
  if (alt == "greater") {
        C2 <- qnorm(1-alpha, mu.0, SEM)
        pwr <- sapply(mus, function(x) {
          pnorm(C2, mean = x, sd = SEM, lower.tail = FALSE)
          })
  }
  return(tibble(`test mu` = mus, power = pwr))

  }

#' @describeIn power creates a power function table based on the t method
#' @export
power_func.t <-
  function(mus,
           mu.0,
           sd,
           n,
           conf.level = 0.95,
           samples = c(1, 2),
           alternative = c("two.sided", "less",
                           "greater")) {
    alt <- match.arg(alternative)
    SEM <- sd / sqrt(n)
    alpha <- 1 - conf.level

    df <- n - samples

    if (alt == "two.sided") {
      C1 <- mu.0 + SEM*qt(alpha / 2,  df)
      C2 <- mu.0 + SEM*qt(1 - alpha / 2, df)
      pwr.l <- sapply(mus[mus <= mu.0], function(x) {
        pt(score(C1, x, sd, n), df)
      })
      pwr.l <- unlist(pwr.l)
      pwr.h <- sapply(mus[mus > mu.0], function(x) {
        pt(score(C2, x, sd, n),
              df = df,
              lower.tail = FALSE)
      })
      pwr.h <- unlist(pwr.h)
      pwr <- c(pwr.l, pwr.h)
    }
if (alt == "less") {
  C1 <- mu.0 + SEM*qt(alpha, SEM)
  pwr <- sapply(mus, function(x) {
    pt(score(C1, x, sd, n),  df)
  })
}
if (alt == "greater") {
  C2 <- mu.0 + SEM*qt(1-alpha, df)
  pwr <- sapply(mus, function(x) {
    pt(score(C2, x, sd, n), df, lower.tail = FALSE)
  })}

return(tibble(`test mu` = mus, power = pwr))
  }
