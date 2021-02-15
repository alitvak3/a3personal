#'CSV to Tibble
#'
#'This function takes a .csv file and assigns it as a tibble.
#'@param x .csv file
#'@param n desired object variable
#'@export

csv_as_tibble <- function(x, n){
     assign(n, as_tibble(read_csv(x)), envir = .GlobalEnv)
}