#' Mean Correlations across the Ten Replicates of Optimization between Z-scores 
#' of Weight and Height for Ages 2 to 20 years using U.S. Centers for Disease 
#' Control and Prevention (CDC) Growth Charts
#' 
#' The mean correlations over the 10x replicates of optimization is calculated 
#' for use in the 'SimKid' package. In the 'SimKid' package the optimized 
#' correlations are validated by simulating virtual populations, calculating 
#' BMI statistics, and overlaying with the respective CDC BMI vs. age growth 
#' charts.
#' 
#' @format ## `cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized`
#' A data frame with 36 rows and 5 columns:
#' \describe{
#'   \item{SEXF}{Female sex indicator (0 is male; 1 is female)}
#'   \item{AGEGRP}{Age group in months}
#'   \item{NITER}{Number of iterations (i.e., replicates) that contributes to 
#'   the MEAN_HTWT_COR calculation}
#'   \item{NSUBJ_AGEMO_SEXF}{Number of virtual subjects, in each replicate of 
#'   the optimization procedure, per month of age and per sex that contributes 
#'   to the MEAN_HTWT_COR calculation}
#'   \item{MEAN_HTWT_COR}{Mean across the replicates of optimized correlation 
#'   between z-score of weight and z-score of height}
#' }
#' @source 
#' data-raw/cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates.csv
"cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized"