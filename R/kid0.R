#' Master Combined Dataset of Growth Charts of Weight, Height, and BMI for Age
#' 
#' cdc0, who0, and fent0 were combined into a single dataset
#' 
#' @format ## `kid0`
#' A data frame with 1,536 rows and 16 columns:
#' \describe{
#'   \item{CHART}{Growth chart label}
#'   \item{VAR}{Demographic variable (WTKG is weight in kg, HTCM is height in cm, BMI is body mass index in kg/m^2)}
#'   \item{SEXF}{Female sex indicator (0 is male; 1 is female)}
#'   \item{AGEGRP}{Age group}
#'   \item{L}{Power in the Box-Cox transformation (calculation of VAR using age)}
#'   \item{M}{Median (calculation of VAR using age)}
#'   \item{S}{Generalized coefficient of variation (calculation of VAR using age)}
#'   \item{P3}{3rd percentile of the given VAR}
#'   \item{P5}{5th percentile of the given VAR}
#'   \item{P10}{10th percentile of the given VAR}
#'   \item{P25}{25th percentile of the given VAR}
#'   \item{P50}{50th percentile of the given VAR}
#'   \item{P75}{75th percentile of the given VAR}
#'   \item{P90}{90th percentile of the given VAR}
#'   \item{P95}{95th percentile of the given VAR}
#'   \item{P97}{97th percentile of the given VAR}
#' }
#' @source data-raw/cdc0.csv, data-raw/who0.csv, data-raw/fent0.csv
"kid0"