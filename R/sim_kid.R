#' Create body size metrics for virtual subjects using anthropometric growth chart data and a distribution of age
#' 
#' @description
#' Body size metrics (height, weight, BMI, and BSA) are created for a population of virtual subjects.
#' The body size metrics reflect the anthropometric growth chart distribution(s) and correlations (ex. height vs weight) according to virtual subject age and sex.
#' The assumed distribution of age (uniform or truncated normal) and probability that a given subject is female are specified by the user.
#' For ages greater than 2 years, CDC growth charts are used. 
#' For ages birth to 2 years, either CDC (the default) or WHO growth charts can be used. Note that while CDC growth charts are used to prevent a jump discontinuity at 2 years, WHO growth charts are recommended for ages 0 to 2 years.
#' For birth only (postnatal age of zero), Fenton growth charts for preterm can be used according to a distribution of gestational age.
#' Note that when using Fenton growth charts, only body weight will be simulated.
#'
#' @details Equations and methods involved during the creation of virtual subjects.
#' 
#' @section Calculation of simulated body height and weight: 
#' 
#' The equation for simulated body height in cm (`HTCM`) or weight in kg (`WTKG`) is:
#' if L (rounded to 6 decimal places) is equal to 0, then `= M*exp(S*Z)`;
#' otherwise `= M*(1+L*S*Z)^(1/L))`.
#' 
#' Where L, M, and S are obtained, using the independent variables of sex (`SEXF`) and age bucket (`AGEGRP`), from identical internal-systems-data versions of the combined anthropometric growth chart datasets ([kid0] and [htwt0] located within the `data` folder).
#' And where Z, the z-score respective to either the height or weight distribution, is randomly sampled for each virtual subject.
#' 
#' @section Simulation of z-scores for variability in height and weight: 
#' 
#' For ages 0 to 2 years, correlations between height and weight are always implemented. This is done by simulating height using length-for-age growth charts (see [kid0] located within the `data` folder) and then simulating weight using weight-for-height growth charts (see [htwt0] located within the `data` folder).
#' For ages greater than 2 years, correlations between height and weight were repeatedly optimized (see [cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates] located within the `data` folder) and then summarized to the mean (see [cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized] located within the `data` folder).
#' For ages greater than 2 years, the user can override the default behavior that includes correlations (as per an identical internal-systems-data version of [cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized]) between simulated height and weight using the `age2to20yr_correlate_htwt` input.
#' 
#' For ages 0 to 2 years and for ages greater than 2 years when simulating without correlations between height and weight:
#' The z-scores are obtained independently for height and weight and for each virtual subject via random sampling from a truncated standard normal distribution using `msm::rtnorm([num], 0, 1, zmin, zmax)`.
#' 
#' For ages greater than 2 years when simulating with correlations between height and weight:
#' The z-scores are obtained simultaneously for height and weight and for each virtual subject via random sampling from a truncated multivariate standard normal distribution using `tmvtnorm::rtmvnorm(num, rep(0, 2), matrix(c(1,htwt_cor,htwt_cor,1), ncol = 2), lower = rep(zmin,2), upper = rep(zmax,2))`.
#' 
#' Where `zmin` and `zmax` are the maximum allowable z-scores, calculated as `qnorm([htwt_percentile_min])` and `qnorm([htwt_percentile_max])`, respectively.
#' And where htwt_cor is the mean optimized correlation, by sex and 1-year bin of age, between height and weight, as listed in an identical internal-systems-data version of [cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized] located within the `data` folder.
#' 
#' @section Calculation of body mass index:
#' 
#' The equation for body mass index in kilograms per meter squared is `BMI = WTKG/((HTCM/100)^2)`.
#' 
#' @section Calculation of body surface area:
#' 
#' The Mosteller equation for body surface area in meters squared is `BSA1 = sqrt(WTKG*HTCM/3600)`.
#' The Gehan and George equation for body surface area in meters squared is `BSA2 = 0.0235*(WTKG^0.51456)*(HTCM^0.42246)`.
#' The DuBois equation for body surface area in meters squared is `BSA3 = 0.007184*(WTKG^0.425)*(HTCM^0.725)`.
#' 
#' @param num A positive integer that specifies the number of subjects to simulate. Defaults to a single subject.
#' @param agedistr A string that specifies the distribution used to create virtual subject age.
#'   * `unif` (the default): A uniform distribution of age with a range from `agemin` to `agemax`.
#'   * `norm`: A truncated normal distribution of age with a mean of `agemean`, a standard deviation of `agesd`, and a range from `agemin` to `agemax`.
#' @param agemean A positive numeric greater than or equal to `agemin` and less than or equal to `agemax` that specifies the mean age when `agedistr = "norm"` is specified.
#'   * Not used for `agedistr = "unif"`.
#'   * Units of postnatal age in months for `age0to2yr_growthchart = "CDC"` or `age0to2yr_growthchart = "WHO"`.
#'   * Units of gestational age in weeks for `age0to2yr_growthchart = "FENTON"`.
#' @param agesd A numeric greater than or equal to `0` that specifies the standard deviation of age when `agedistr = "norm"` is specified.
#'   * Not used for `agedistr = "unif"`.
#'   * Units of postnatal age in months for `age0to2yr_growthchart = "CDC"` or `age0to2yr_growthchart = "WHO"`.
#'   * Units of gestational age in weeks for `age0to2yr_growthchart = "FENTON"`.
#' @param agemin A numeric that specifies the lower range of age. Defaults to the maximum allowable range if missing.
#'   * Must be greater than or equal to `0` months of postnatal age for `age0to2yr_growthchart = "CDC"` or `age0to2yr_growthchart = "WHO"`.
#'   * Must be greater than or equal to `22` weeks of gestational age for `age0to2yr_growthchart = "FENTON"`.
#'   * Must be less than `agemax`.
#' @param agemax A numeric that specifies the upper range of age. Defaults to the maximum allowable range if missing.
#'   * Must be less than `240` months of postnatal age for `age0to2yr_growthchart = "CDC"` or `age0to2yr_growthchart = "WHO"`.
#'   * Must be less than `41` weeks of gestational age for `age0to2yr_growthchart = "FENTON"`.
#'   * Must be greater than `agemin`.
#' @param prob_female A numeric value with an inclusive range of `0` to `1` that specifies the probability that a given virtual subject is female. Defaults to `0.5`.
#' @param age0isbirth A logical that specifies whether age equal to zero denotes birth.
#'   * `TRUE`: Age of `0` is birth.
#'   * `FALSE` (the default): Age of `0` is ages from birth to less than one month.
#'   * Not applicable nor used for `age0to2yr_growthchart = "FENTON"`, for which postnatal age is always zero.
#' @param age0to2yr_growthchart A string that specifies which anthropometric growth charts are used for ages less than or equal to 2 years old.
#'   * `"CDC"` (the default): United States Centers for Disease Control and Prevention growth charts are used.
#'   * `"WHO"`: World Health Organization growth charts are used.
#'   * `"FENTON"`: Fenton growth charts for preterm newborns are used. This option is only available when simulating virtual subjects at birth (postnatal age = 0).
#' @param age2to20yr_correlate_htwt A logical that specifies whether correlations, by sex and year of age, are implemented between simulated height and simulated weight for ages greater than or equal to 2 years old.
#'   * `TRUE` (the default): Correlations are implemented between simulated height and simulated weight according to an identical internal-systems-data version of [cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized] located within the `data` folder.
#'   * `FALSE`: Height and weight are simulated independently without any correlation(s). Note that this will likely result in unrealistic virtual subjects.
#'   * Not applicable nor used for `age0to2yr_growthchart = "FENTON"`, for which height is not simulated.
#' @param htwt_percentile_min A numeric value that specifies the minimum allowed percentile of simulated height and weight, expressed as a decimal.
#'    * Must be greater than or equal to `0.001`.
#'    * Must be less than `htwt_percentile_max`.
#'    * Defaults to `0.001` when `age0to2yr_growthchart = "CDC"` or `age0to2yr_growthchart = "WHO"`.
#'    * Defaults to `0.01` when `age0to2yr_growthchart = "FENTON"` to avoid non-viable birth weights.
#' @param htwt_percentile_max A numeric value that specifies the maximum allowed percentile of simulated height and weight, expressed as a decimal.
#'    * Must be less than or equal to `0.999`.
#'    * Must be greater than `htwt_percentile_min`.
#'    * Defaults to `0.999` when `age0to2yr_growthchart = "CDC"` or `age0to2yr_growthchart = "WHO"`.
#'    * Defaults to `0.99` when `age0to2yr_growthchart = "FENTON"` to avoid non-viable birth weights.
#' @param masterseed An integer ranging from `1` to `.Machine$integer.max` that sets an overall seed for the simulation to ensure reproducibility of the results. Defaults to no seed.
#'
#' @return A data frame with the number of rows equal to `num` and columns of:
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
#'    An error will be returned if the simulation fails.
#' @export
#'
#' @seealso [sim_kid_nperagebin()] to simulate the same number of virtual subjects per age bucket reported by the anthropometric growth charts.
#'
#' @examples
#' # Simulate 1 subject with an age randomly sampled from a uniform distribution of ages ranging
#' #    from 0 to 20 years using CDC growth charts.
#' df_kids <- sim_kid()
#' 
#' # Simulate 10 female 3 year old subjects with a seed set for reproducibility.
#' df_kids <- sim_kid(
#'   num = 10,
#'   agedistr = "norm", agemean = 36, agesd = 0,
#'   prob_female = 1, masterseed = 513
#' )
#' 
#' # Simulate 10 subjects (approximately 50% female) with ages ranging from 1 year to 2 years
#' #   according to a uniform distribution of age using WHO growth charts.
#' df_kids <- sim_kid(
#'   num = 10,
#'   agedistr = "unif", agemin = 12, agemax = 24,
#'   age0to2yr_growthchart = "WHO"
#' )
sim_kid <- function(
  num = 1,
  agedistr = "unif",
  agemean = NULL,
  agesd = NULL,
  agemin = NULL,
  agemax = NULL,
  prob_female = 0.5,
  age0isbirth = FALSE,
  age0to2yr_growthchart = "CDC",
  age2to20yr_correlate_htwt = TRUE,
  htwt_percentile_min = NULL,
  htwt_percentile_max = NULL,
  masterseed = NULL
){
  ## CHECK ARGUMENTS ####
  
  chk_arg(
    num, agedistr, agemean, agesd, agemin, agemax, prob_female, 
    age0isbirth, age0to2yr_growthchart, age2to20yr_correlate_htwt, 
    htwt_percentile_min, htwt_percentile_max, masterseed
  )
  
  ## INITIALIZE DEFAULT REACTIVE ARGUMENTS ####
  
  initargs <- init_arg(
    age0to2yr_growthchart = toupper(age0to2yr_growthchart),
    agemin = agemin, agemax = agemax,
    htwt_percentile_min = htwt_percentile_min, htwt_percentile_max = htwt_percentile_max
  )
  
  agemin <- initargs$agemin
  agemax <- initargs$agemax
  htwt_percentile_min <- initargs$htwt_percentile_min
  htwt_percentile_max <- initargs$htwt_percentile_max
  
  ## MANIP INPUT ARGUMENTS ####
  
  age0to2yr_growthchart <- toupper(age0to2yr_growthchart)
  zscore_min <- stats::qnorm(htwt_percentile_min)
  zscore_max <- stats::qnorm(htwt_percentile_max)
  
  ## GET SEEDS IF MASTERSEED != NULL ####
  
  seedl <- get_seeds(masterseed)
  
  ## INITIALIZE OUTPUT DATA FRAME ####
  
  
  
  ## END ####
}