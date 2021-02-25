power_func.z <-
  function(mus,
           mu.0,
           sd,
           n,
           alpha,
           alternative = c("two.sided", "less",
                           "greater")) {
    alt<- match.arg(alternative)
    SEM <- sd / sqrt(n)
    
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
power_func.t <-
  function(mus,
           mu.0,
           sd,
           n,
           alpha,
           samples = 1,
           alternative = c("two.sided", "less",
                           "greater")) {
    alt <- match.arg(alternative)
    SEM <- sd / sqrt(n)
    
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