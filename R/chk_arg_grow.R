#' Check inputs for `grow_kid()`
#'
#' Checks the input arguments for `grow_kid()`
#'
#' @inheritParams grow_kid
#'
#' @return Nothing if checks are successful, otherwise warnings and/or errors.
#'
#' @noRd
chk_arg_grow <- function(data = NULL, grow_time = NULL, tstep = NULL, age0isbirth = NULL){
  
  if(inherits(data,"data.frame") == FALSE){
    stop("Error: data must be a data frame.")
  }
  
  if(inherits(grow_time,"numeric") == FALSE || length(grow_time) != 1 || grow_time < 0){
    stop("Error: grow_time must be a non-negative numeric of length one.")
  }
  
  if(inherits(tstep,"numeric") == FALSE || length(tstep) != 1 || tstep <= 0){
    stop("Error: tstep must be a positive numeric of length one.")
  }
  
  if(all(c("ID","SEXF","AGEMO","ZWTKG","ZHTCM","CHART") %in% colnames(data)) == FALSE){
    stop("Error: data must have columns of ID, SEXF, AGEMO, ZWTKG, ZHTCM, and CHART.")
  }
  
  if(length(age0isbirth) != 1L || !is.logical(age0isbirth) || is.na(age0isbirth)){stop("age0isbirth must be logical of length one (\"TRUE\" or \"FALSE\")")}
}