#' Constants
#'
#' This function loads a constant.
#' Constants include:
#' h (J*m)
#' c (m/s)
#' e
#' R (J/(k*m))
#' An
#' F (C/mol)
constants <- function(x){
     if(x == "h"){6.62607015*10^-34} #J*m
     else if(x == "c"){299792458} #m/s
     else if(x == "e"){2.71828182845904523536028747135266249775724709369995}
     else if(x == "R"){8.31446261815324} #J/(k*m)
     else if(x == "An"){6.02214076}
     else if(x == "F"){96485.33212} #C/mol
}
