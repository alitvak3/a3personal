#' Working Directory Navigators
#' 
#' These functions allow the working directory to be moved up or down in the file path.
#' 
#' @describeIn wd_nav navigate wd up
#' @param n_lev levels to navigate up, default 1
#' @export
wd_up <- function(n_lev = 1){
     extend <- str_split(getwd(), "/")[[1]]
     trim <- head(extend, -n_lev)
     compress <- paste(trim, collapse = "/")
     setwd(compress)
     return(getwd())
}
#' @describeIn wd_nav navigates wd down
#' @param x character string for the downward file path
#' @export
wd_down <- function(x){
     setwd(paste(getwd(), x, sep = "/"))
     return(getwd())
}
#' @describeIn wd_nav navigates wd up, then down
#' @param up levels to navigate up, default 0
#' @param down character string for the downward file path
#' @export
wd_nav <- function(up = 0, down){
     if(up == 0){wd_down(down)}
     else if(missing(down) == TRUE){wd_up(up)}
     else{
          wd_up(up)
          wd_down(down)
     }
}
     