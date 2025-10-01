#' Calculate BMI and BSA
#' 
#' @description
#' BMI and BSA are calculated for a data frame that minimally has columns of 
#' HTCM and WTKG. Output columns match the definitions given by `sim_kid()`.
#'
#' @param data A data frame with columns of `HTCM` and `WTKG`.
#'
#' @return A data frame with columns of `BMI`, `BSA1`, `BSA2`, and `BSA3` added 
#' to `data`:
#'    * `BMI`: Body mass index in kilograms per meter squared, rounded to 1 
#'    decimal place.
#'    * `BSA1`: Body surface area in meters squared, rounded to 2 decimal 
#'    places; calculated using the Mosteller equation.
#'    * `BSA2`: Body surface area in meters squared, rounded to 2 decimal 
#'    places; calculated using the Gehan and George equation.
#'    * `BSA3`: Body surface area in meters squared, rounded to 2 decimal 
#'    places; calculated using the DuBois equation.
#' @export
#'
#' @examples
#' demo0 <- sim_kid()
#' demo <- calc_bmi_bsa(data = demo0)
calc_bmi_bsa <- function(data = NULL){
  
  chk_arg_bodysize(data = data)
  
  data %>%
    dplyr::mutate(
      BMI = ifelse(
        !is.na(.data$HTCM), 
        round(.data$WTKG/((.data$HTCM/100)^2),1), 
        NA
      )
    )%>%
    dplyr::mutate(
      BSA1 = ifelse(
        !is.na(.data$HTCM), 
        round(sqrt(.data$WTKG*.data$HTCM/3600),2), 
        NA
      ), # Mosteller
      BSA2 = ifelse(
        !is.na(.data$HTCM), 
        round(0.0235*(.data$WTKG^0.51456)*(.data$HTCM^0.42246),2), 
        NA
      ), # Gehan
      BSA3 = ifelse(
        !is.na(.data$HTCM), 
        round(0.007184*(.data$WTKG^0.425)*(.data$HTCM^0.725),2), 
        NA
      ) # DuBois
    )
}