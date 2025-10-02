#' Fenton Growth Charts of Weight for Age in Preterm Infants
#' 
#' Fenton growth charts for male and female weight vs. age were digitized up 
#' to 40 weeks (for full-term) from the literature (1-3) using 
#' 'PinPoint Digitizer' (4).
#' Fitting of weight LMS parameters by age and sex was done in R using the 
#' optimize function (5) and the sum of squares statistic between digitized and 
#' predicted weight percentiles.
#' (1) <https://ucalgary.ca/resource/preterm-growth-chart/preterm-growth-chart>
#' (2) Fenton, T.R., Kim, J.H. A systematic review and meta-analysis to revise 
#' the Fenton growth chart for preterm infants. BMC Pediatr 13, 59 (2013). 
#' <doi:10.1186/1471-2431-13-59>
#' (3) Fenton, T.R., Nasser, R., Eliasziw, M. et al. Validating the weight gain 
#' of preterm infants between the reference growth curve of the fetus and the 
#' term infant. BMC Pediatr 13, 92 (2013). 
#' <doi:10.1186/1471-2431-13-92>
#' (4) <https://mhismail.github.io/PinPoint-Landing/>
#' (5) <https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/
#' optimize>
#' 
#' @format ## `fent0`
#' A data frame with 38 rows and 16 columns:
#' \describe{
#'   \item{CHART}{Growth chart label}
#'   \item{VAR}{Demographic variable (WTKG is weight in kg)}
#'   \item{SEXF}{Female sex indicator (0 is male; 1 is female)}
#'   \item{AGEGRP}{Newborn age group bucket in weeks (PNA is postnatal age; 
#'   GA is gestational age)}
#'   \item{L}{Power in the Box-Cox transformation (calculation of VAR using 
#'   age)}
#'   \item{M}{Median (calculation of VAR using age)}
#'   \item{S}{Generalized coefficient of variation (calculation of VAR using 
#'   age)}
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
#' @source 
#' <https://ucalgary.ca/resource/preterm-growth-chart/preterm-growth-chart>
"fent0"