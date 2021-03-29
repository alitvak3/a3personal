#' T-score of a Pearson correlation coefficient
#' @param r Pearson correlation coefficient
#' @param n number of observations
#' @export
t_of_r <- function(r, n){
        t <- r*sqrt(n-2)/sqrt(1-r^2)
        p.2t <- 2 * min(
                c(
                pt(t, n-2),
                pt(t, n-2, lower.tail = FALSE)
                )
        )
        res <- c(t, p.2t)
        names(res) <- c("t", "p.2t")
        res
}