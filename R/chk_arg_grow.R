#' Check inputs for `grow_kid()`
#'
#' Checks the input arguments for `grow_kid()`
#'
#' @inheritParams grow_kid
#'
#' @return Nothing if checks are successful, otherwise warnings and/or errors.
#'
#' @noRd
chk_arg_grow <- function(data = NULL, grow_time = NULL, tstep = NULL, tunit = NULL){
  
  if(inherits(data,"data.frame") == FALSE){
    stop("Error: data must be a data frame.")
  }
  
  if(inherits(grow_time,"numeric") == FALSE || length(grow_time) != 1 || grow_time < 0){
    stop("Error: grow_time must be a non-negative numeric of length one.")
  }
  
  if(inherits(tstep,"numeric") == FALSE || length(tstep) != 1 || tstep <= 0){
    stop("Error: tstep must be a positive numeric of length one.")
  }
  
  if(inherits(tunit,"character") == FALSE || length(tunit) != 1 || !(tunit %in% c("month","year"))){
    stop("Error: tunit must either be \"month\" or \"year\".")
  }
  
  if(all(c("ID","SEXF","AGEMO","ZWTKG","ZHTCM","CHART") %in% colnames(data)) == FALSE){
    stop("Error: data must have columns of ID, SEXF, AGEMO, ZWTKG, ZHTCM, and CHART.")
  }
  
}