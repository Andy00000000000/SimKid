#' Check output for `sim_kid()`
#'
#' Checks the output data frame for `sim_kid()`
#'
#' @param demo A data frame of `sim_kid()` output.
#' @param num A positive integer specifying the correct number of rows.
#'
#' @return Nothing if checks are successful, otherwise warnings and/or errors.
#'
#' @noRd
chk_out <- function(demo = NULL, num = NULL){
  
  if(inherits(demo, "data.frame") == FALSE){
    stop(paste0(
      "Error: The output class is not data.frame. Please check that ",
      "input arguments are correctly specified."
    ))
  }
  
  if(nrow(demo) != num){
    stop(paste0(
      "Error: The number of output rows is incorrect. Please check that ",
      "input arguments are correctly specified. If correct, ",
      "please reach out for support."
    ))
  }

  tmp_correct <- as.data.frame(matrix(0,nrow = 1, ncol = 16))
  colnames(tmp_correct) <- c(
    "ID","SEXF","AGEMO","AGE","GAWK","WTKG","HTCM",
    "BMI","BSA1","BSA2","BSA3",
    "ZWTKG","ZHTCM","PWTKG","PHTCM","CHART"
  )
  tmp_test <- paste0(colnames(demo), collapse = ".")
  tmp_correct <- paste0(colnames(tmp_correct), collapse = ".")
  if(tmp_test != tmp_correct){
    stop(paste0(
      "Error: The output columns are incorrect. Please check that ",
      "input arguments are correctly specified. If correct, ",
      "please reach out for support."
    ))
  }

  if(any(is.na(demo$SEXF)) == TRUE){
    stop(paste0(
      "Error: NA occurred for SEXF output. Please check that ",
      "input arguments are correctly specified."
    ))
  }

  if(any(is.na(demo$AGE)) == TRUE){
    stop(paste0(
      "Error: NA occurred for AGE output. Please check that ",
      "input arguments are correctly specified."
    ))
  }

  if(any(is.na(demo$WTKG)) == TRUE){
    stop(paste0(
      "Error: NA occurred for WTKG output. Please check that ",
      "input arguments are correctly specified."
    ))
  }
}