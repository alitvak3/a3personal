score <- function(x, mu, sd, n){
        (x-mu) / (sd/sqrt(n))
}