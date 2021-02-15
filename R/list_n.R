#' List with Names
#'
#' This function converts a vector into a list with names eqaul to the values.
#' Useful for lapply and sapply operations.
#' @export
list_n <- function(x = 1:100){
     l <- as.list(x)
     names(l) <- factor(x)
}
