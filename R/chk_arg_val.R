#' Check inputs for `validate_kid()`
#'
#' Checks the input arguments for `validate_kid()`
#'
#' @inheritParams validate_kid
#'
#' @return Nothing if checks are successful, otherwise warnings and/or errors.
#'
#' @noRd
chk_arg_val <- function(age0isbirth = NULL, overlay_percentile = NULL){
  
  if((!is.na(overlay_percentile) & inherits(overlay_percentile,"numeric") == FALSE) | length(overlay_percentile) != 1L){
    stop("Error: overlay_percentile must be either NA or a numeric of length one ranging between 0 and 1.")
  }
  
  if(!is.na(overlay_percentile) && inherits(overlay_percentile,"numeric") == TRUE && (overlay_percentile <= 0 | overlay_percentile >= 1)){
    stop("Error: overlay_percentile must be either NA or a numeric of length one ranging between 0 and 1.")
  }
  
  if(length(age0isbirth) != 1L || !is.logical(age0isbirth) || is.na(age0isbirth)){stop("age0isbirth must be logical of length one (\"TRUE\" or \"FALSE\")")}
}