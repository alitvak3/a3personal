#' t-Test from Summary Statistics
#' 
#' This is the t.test fucntion from the stats package adapted to work from summary statistics instead of vectors.
#' @export
#' @param m1 mean of x
#' @param m2 mean of y
#' @param s1 standard deviation of x
#' @param s2 standard deviation of y
#' @param n1 number of observations in x
#' @param n2 number of observations in y
t_from_summ <- function (m1, m2, s1, s2, n1, n2, alternative = c("two.sided", "less", 
                                       "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, 
          ...) 
{
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu))) 
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 || 
                               !is.finite(conf.level) || conf.level < 0 || conf.level > 
                               1)) 
    stop("'conf.level' must be a single number between 0 and 1")
 
  nx <- n1
  mx <- m1
  vx <- s1^2
  if (is.null(m2)) {
    if (nx < 2) 
      stop("not enough 'x' observations")
    df <- nx - 1
    stderr <- sqrt(vx/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx)) 
      stop("data are essentially constant")
    tstat <- (mx - mu)/stderr
    method <- if (paired) 
      "Paired t-test"
    else "One Sample t-test"
    estimate <- setNames(mx, if (paired) 
      "mean of the differences"
      else "mean of x")
  }
  else {
    ny <- n2
    if (nx < 1 || (!var.equal && nx < 2)) 
      stop("not enough 'x' observations")
    if (ny < 1 || (!var.equal && ny < 2)) 
      stop("not enough 'y' observations")
    if (var.equal && nx + ny < 3) 
      stop("not enough observations")
    my <- m2
    vy <- s2^2
    method <- paste(if (!var.equal) 
      "Welch", "Two Sample t-test")
    estimate <- c(mx, my)
    names(estimate) <- c("mean of x", "mean of y")
    if (var.equal) {
      df <- nx + ny - 2
      v <- 0
      if (nx > 1) 
        v <- v + (nx - 1) * vx
      if (ny > 1) 
        v <- v + (ny - 1) * vy
      v <- v/df
      stderr <- sqrt(v * (1/nx + 1/ny))
    }
    else {
      stderrx <- sqrt(vx/nx)
      stderry <- sqrt(vy/ny)
      stderr <- sqrt(stderrx^2 + stderry^2)
      df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 
                                                        1))
    }
    if (stderr < 10 * .Machine$double.eps * max(abs(mx), 
                                                abs(my))) 
      stop("data are essentially constant")
    tstat <- (mx - my - mu)/stderr
  }
  if (alternative == "less") {
    pval <- pt(tstat, df)
    cint <- c(-Inf, tstat + qt(conf.level, df))
  }
  else if (alternative == "greater") {
    pval <- pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - qt(conf.level, df), Inf)
  }
  else {
    pval <- 2 * pt(-abs(tstat), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if (paired || !is.null(m2)) 
    "difference in means"
  else "mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = pval, 
               conf.int = cint, estimate = estimate, null.value = mu, 
               stderr = stderr, alternative = alternative, method = method)
  class(rval) <- "htest"
  rval
}