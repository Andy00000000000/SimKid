#' Check inputs for `calc_bmi_bsa()`
#'
#' Checks the input arguments for `calc_bmi_bsa()`
#'
#' @inheritParams calc_bmi_bsa
#'
#' @return Nothing if checks are successful, otherwise warnings and/or errors.
#'
#' @noRd
chk_arg_bodysize <- function(data = NULL){
  
  if(inherits(data, "data.frame") == FALSE){
    stop("Error: data must be a data frame.")
  }
  
  if(all(c("WTKG","HTCM") %in% colnames(data)) == FALSE){
    stop("Error: data must have columns of HTCM and WTKG.")
  }
}