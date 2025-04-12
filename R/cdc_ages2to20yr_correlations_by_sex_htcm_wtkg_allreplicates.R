#' Optimized Correlations between Z-scores of Weight and Height for Ages 2 to 20 years using U.S. Centers for Disease Control and Prevention (CDC) Growth Charts
#' 
#' Methods of dataset creation:
#' 1. Virtual subject age is created: 1000 males and 1000 females per each month of age ranging from 25 to 239 months.
#' 2. Virtual subject height and weight are created using the CDC growth charts (i.e., LMS parameters) and BMI is calculated.
#' 3. The height and weight distributions by age are constrained between the 0.1 and 99.9 percentiles (i.e., z-scores from -3 to 3).
#' 4. The correlations between z-score of height and z-score of weight are optimized separately by sex and age using a 1-year age bucket (ex. correlation for ages 2-3 yr, 3-4 yr, etc.).
#' 5. Percentiles of BMI (3rd, 10th, 25th, 50th, 75th, 90th, 97th) for the virtual population is compared to matching percentiles of observed BMI (i.e. the CDC growth chart of BMI vs age using the lower end of the 1-year age bucket) to calculate sum of squares.
#' 6. R optimize function is used to minimize the sum of squares, providing the optimal correlation between z-scores of height and weight per sex and year of age.
#' 7. This process is repeated 10x.
#' 
#' @format ## `cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates`
#' A data frame with 360 rows and 4 columns:
#' \describe{
#'   \item{ITER}{Iteration of the repeated optimization procedure}
#'   \item{SEXF}{Female sex indicator (0 is male; 1 is female)}
#'   \item{AGEGRP}{Age group in months}
#'   \item{HTWT_COR}{Optimized correlation between z-score of weight and z-score of height}
#' }
#' @source data-raw/cdc0.csv (subset of data-raw/kid0.csv) and data-raw/htwt0.csv
"cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates"