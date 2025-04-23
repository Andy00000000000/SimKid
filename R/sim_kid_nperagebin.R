#' Create body size metrics for virtual subjects using anthropometric growth chart data and an equal number of subjects per age bin
#'
#' @description
#' Body size metrics (height, weight, BMI, and BSA) are created for a population of virtual subjects.
#' The body size metrics reflect the anthropometric growth chart distribution(s) and correlations (ex. height vs weight) according to virtual subject age and sex.
#' The probability that a given subject is female are specified by the user. The simulation produces and equal number of virtual subjects for each anthropometric age bin.
#' For ages greater than 2 years, CDC growth charts are used. 
#' For ages birth to 2 years, either CDC (the default) or WHO growth charts can be used. Note that while CDC growth charts are used to prevent a jump discontinuity at 2 years, WHO growth charts are recommended for ages 0 to 2 years.
#' For birth only (postnatal age of zero), Fenton growth charts for preterm can be used according to a distribution of gestational age.
#' Note that when using Fenton growth charts, only body weight will be simulated.
#'
#' @inheritParams sim_kid
#' @param num_per_age A positive integer that specifies the number of subjects to simulate for each age bin. Defaults to a single subject for each age bin.
#' 
#' @return A data frame with the number of rows equal to `num_per_age` multiplied by the number of anthropometric growth chart age bins and columns of:
#'    * `ID`: An integer ranging from `1` to `num` that serves as a virtual subject identifier.
#'    * `SEXF`: An integer of value `0` for male or `1` for female.
#'    * `AGEMO`: Postnatal age in months.
#'    * `AGE`: Postnatal age in years.
#'    * `GAWK`: Gestational age in weeks.
#'    * `WTKG`: Body weight in kilograms, rounded to 2 decimal places.
#'    * `HTCM`: Body height in centimeters, rounded to the nearest centimeter.
#'    * `BMI`: Body mass index in kilograms per meter squared, rounded to 1 decimal place.
#'    * `BSA1`: Body surface area in meters squared, rounded to 2 decimal places; calculated using the Mosteller equation.
#'    * `BSA2`: Body surface area in meters squared, rounded to 2 decimal places; calculated using the Gehan and George equation.
#'    * `BSA3`: Body surface area in meters squared, rounded to 2 decimal places; calculated using the DuBois equation.
#'    * `ZWTKG`: The z-score of weight-for-height for ages 0 to 2 years, weight-for-age for ages greater than 2 years, and weight-for-gestational-age for newborns when using Fenton growth charts.
#'    * `ZHTCM`: The z-score of height-for-age.
#'    * `PWTKG`: The percentile of weight corresponding to the respective z-score.
#'    * `PHTCM`: The percentile of height corresponding to the respective z-score.
#'    A warning will be returned if the simulation to create virtual subjects fails.
#' @export
#'
#' @seealso [sim_kid()] to simulate virtual subjects using a distribution of age.
#'
#' @examples
#' # Simulate 1 subject per age bin for ages 0 to 20 years using
#' #   CDC growth charts and a 50% probability of female sex.
#' df_kids <- sim_kid_nperagebin()
sim_kid_nperagebin <- function(
    num_per_age = 1,
    prob_female = 0.5,
    age0isbirth = FALSE,
    age0to2yr_growthchart = "CDC",
    age2to20yr_correlate_htwt = TRUE,
    htwt_percentile_min = NULL,
    htwt_percentile_max = NULL,
    masterseed = NULL
){
  
}