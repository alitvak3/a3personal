boundExpand <- function(bounds){
        Lower <- c(-Inf, bounds)
        Upper <- lead(Lower, 1)
        Upper[length(Lower)] <- Inf
        return(list(Lower = Lower, Upper = Upper))
}