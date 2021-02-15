#' Constants
#'
#' This function loads a constant.
#' @param x name of constant
#' @section Constants:
#' \itemize{
#' \item{"h"}{ --- Planck constant (J\*m)}
#' \item{"c"}{ --- speed of light in a vaccum (m/s)}
#' \item{"e"}{ --- Euler number}
#' \item{"R"}{ --- ideal gas constant (J/(k\*m))}
#' \item{"me"}{ --- mass of an electron (kg)}
#' \item{"F"}{ --- Faraday constant (C/mol)}
#' \item{"An"}{ --- Avogadro number}
#' \item{"Rh"}{ --- Rydbherg for hjydrogen (1/m)}
#' \item{"Cw"}{ --- molar heat capacity of water (J/(mol*K))}
#' }
#' @export
constants <- function(x){
     if(x == "h"){6.62607015*10^-34} #J*m
     else if(x == "c"){299792458} #m/s
     else if(x == "e"){2.71828182845904523536028747135266249775724709369995}
     else if(x == "R"){8.31446261815324} #J/(k*m)
     else if(x == "An"){6.02214076*10^23}
     else if(x == "F"){96485.33212} #C/mol
     else if (x == "me"){9.1093837015*10^-31} #kg
     else if (x == "Rh"){1.09678810*10^7} #m^-1
     else if(x == "Cw"){75.385} #J/(mol*K)
}
