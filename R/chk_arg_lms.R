#' Check inputs for `lms_calc()`
#'
#' Checks the input arguments for `lms_calc()`
#'
#' @inheritParams lms_calc
#'
#' @return Nothing if checks are successful, otherwise warnings and/or errors.
#'
#' @noRd
chk_arg_lms <- function(z = NA, l = NA, m = NA, s = NA){
  
  if(inherits(z, "numeric") == FALSE){
    stop("Error: z must be numerical.")
  }
  
  if(inherits(l, "numeric") == FALSE){
    stop("Error: l must be numerical.")
  }
  
  if(inherits(m, "numeric") == FALSE){
    stop("Error: m must be numerical.")
  }
  
  if(inherits(s, "numeric") == FALSE){
    stop("Error: s must be numerical.")
  }
  
  if(any(is.na(z)) == TRUE){
    stop("Error: z cannot contain NA.")
  }
  
  if(any(is.na(l)) == TRUE){
    stop("Error: l cannot contain NA.")
  }
  
  if(any(is.na(m)) == TRUE){
    stop("Error: m cannot contain NA.")
  }
  
  if(any(is.na(s)) == TRUE){
    stop("Error: s cannot contain NA.")
  }
  
  if(
    length(l) != length(m) || 
    length(l) != length(s) || 
    length(m) != length(s)
  ){
    stop("Error: l, m, and s must all be the same length.")
  }
  
  if(
    (length(l) > 1 | length(z) > 1) &
    (length(z) != length(l) & length(z) != 1 & length(l) != 1)
  ){
    stop(paste0(
      "Error: If l, m, and s are vectors of length > 1, ",
      "then z must either be the same length as l, m, and s or length 1. ",
      "If z is a vector of length > 1, ",
      "then l, m, and s must eithe be the same length as z or length 1."
    ))
  }
}