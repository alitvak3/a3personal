#' pH approximator
#'
#' This function approximates the pH of a solution whre loss of base is negligible.
pH.approx <- function(Ka.base, Ka.exp, conc) {
     H <- sqrt(conc*Ka.base*10^Ka.exp)
     -log10(H)
}
