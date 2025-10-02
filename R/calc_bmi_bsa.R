#' Calculate BMI and BSA
#' 
#' @description
#' BMI and BSA are calculated for a data frame that minimally has columns of 
#' HTCM and WTKG. Output columns match the definitions given by `sim_kid()`.
#'
#' @details Equations and methods involved during the creation of virtual 
#' subjects.
#' 
#' @section Calculation of body mass index:
#' 
#' The equation for body mass index in kilograms per meter squared is 
#' `BMI = WTKG/((HTCM/100)^2)`.
#' 
#' @section Calculation of body surface area:
#' 
#' The Mosteller equation (1) for body surface area in meters squared is 
#' `BSA1 = sqrt(WTKG*HTCM/3600)`.
#' 
#' The Gehan and George equation (2) for body surface area in meters squared is 
#' `BSA2 = 0.0235*(WTKG^0.51456)*(HTCM^0.42246)`.
#' 
#' The DuBois equation (3) for body surface area in meters squared is 
#' `BSA3 = 0.007184*(WTKG^0.425)*(HTCM^0.725)`.
#' 
#' (1) Mosteller RD. Simplified calculation of body-surface area. N Engl J Med. 
#' 1987 Oct 22;317(17):1098. <doi: 10.1056/NEJM198710223171717.> PMID: 3657876.
#' (2) Gehan EA, George SL. Estimation of human body surface area from height 
#' and weight. Cancer Chemother Rep. 1970 Aug;54(4):225-35. PMID: 5527019.
#' (3) Du Bois D, Du Bois EF. A formula to estimate the approximate surface 
#' area if height and weight be known. 1916. Nutrition. 1989 
#' Sep-Oct;5(5):303-11; discussion 312-3. PMID: 2520314.
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