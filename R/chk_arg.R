#' Check inputs for `sim_kid()`
#'
#' Checks the input arguments for `sim_kid()`
#'
#' @inheritParams sim_kid
#'
#' @return Nothing if checks are successful, otherwise warnings and/or errors.
#'
#' @noRd
chk_arg <- function( # same defaults as sim_kid() to facilitate testthat unit tests
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
  ## age0to2yr_growthchart ####
  
  if(length(age0to2yr_growthchart) != 1L || !is.character(age0to2yr_growthchart) || !(toupper(age0to2yr_growthchart) %in% c("CDC","WHO","FENTON"))){stop("age0to2yr_growthchart must be either \"CDC\", \"WHO\", or \"FENTON\"")}
  
  ## num ####
  
  if(length(num) != 1L || !is.numeric(num) || num%%1!=0 || num <= 0){stop("num must be a positive integer of length one")}
  
  ## agedistr ####
  
  if(length(agedistr) != 1L || !is.character(agedistr) || !(agedistr %in% c("unif","norm","nperage"))){stop("agedistr must be either \"unif\" or \"norm\" or \"nperage\"")}
  
  ## agemin and agemax ####
  
  if(!is.null(agemin) && (length(agemin) != 1L || !is.numeric(agemin))){stop("agemin must be a numeric of length one")}
  if(!is.null(agemax) && (length(agemax) != 1L || !is.numeric(agemax))){stop("agemin must be a numeric of length one")}
  if(toupper(age0to2yr_growthchart) %in% c("CDC","WHO") && !is.null(agemin) && agemin < 0){stop("agemin must be >= 0 months when age0to2yr_growthchart is either \"CDC\" or \"WHO\"")}
  if(toupper(age0to2yr_growthchart) == "FENTON" && !is.null(agemin) && agemin < 22){stop("agemin must be >= 22 weeks of gestational age when age0to2yr_growthchart is \"FENTON\"")}
  if(toupper(age0to2yr_growthchart) %in% c("CDC","WHO") && !is.null(agemax) && agemax >= 240){stop("agemax must be < 240 months when age0to2yr_growthchart is either \"CDC\" or \"WHO\"")}
  if(toupper(age0to2yr_growthchart) == "FENTON" && !is.null(agemax) && agemax >= 41){stop("agemax must be < 41 weeks of gestational age when age0to2yr_growthchart is \"FENTON\"")}
  if(!is.null(agemax) && !is.null(agemin) && agemax < agemin){stop("agemax must be > agemin")}
  
  ## agemean and agesd ####
  
  if((!is.null(agemean)) && agedistr != "norm"){warning("agemean is only used for agedistr = \"norm\"")}
  if((!is.null(agesd)) && agedistr != "norm"){warning("agesd is only used for agedistr = \"norm\"")}
  if(agedistr == "norm" && (is.null(agemean) || is.null(agesd))){stop("agemean and agesd must be specified when agedistr = \"norm\"")}
  if(agedistr == "norm" && (length(agemean) != 1L || !is.numeric(agemean))){stop("agemean must be numeric of length one")}
  if(agedistr == "norm" && (length(agesd) != 1L || !is.numeric(agesd) || agesd < 0)){stop("agesd must be numeric of length one and greater than or equal to 0")}
  if(agedistr == "norm" && ((!is.null(agemin) && agemean < agemin) || (!is.null(agemax) && agemean > agemax))){stop("agemean must satisty agemin <= agemean <= agemax")}
  if(agedistr == "norm" && is.null(agemin) && (toupper(age0to2yr_growthchart) %in% c("CDC","WHO") && agemean < 0)){stop("agemean must satisfy 0 <= agemean < 240 months when age0to2yr_growthchart is either \"CDC\" or \"WHO\"")}
  if(agedistr == "norm" && is.null(agemax) && (toupper(age0to2yr_growthchart) %in% c("CDC","WHO") && agemean >= 240)){stop("agemean must satisfy 0 <= agemean < 240 months when age0to2yr_growthchart is either \"CDC\" or \"WHO\"")}
  if(agedistr == "norm" && is.null(agemin) && (toupper(age0to2yr_growthchart) == "FENTON" && agemean < 22)){stop("agemean must satisfy 22 <= agemean < 41 weeks of gestational age when age0to2yr_growthchart is \"FENTON\"")}
  if(agedistr == "norm" && is.null(agemax) && (toupper(age0to2yr_growthchart) == "FENTON" && agemean >= 41)){stop("agemean must satisfy 22 <= agemean < 41 weeks of gestational age when age0to2yr_growthchart is \"FENTON\"")}
  
  ## prob_female ####
  
  if(length(prob_female) != 1L || !is.numeric(prob_female) || prob_female < 0 || prob_female > 1){stop("prob_female must be numeric of length one and between 0 to 1, inclusively")}
  
  ## age0isbirth ####
  
  if(length(age0isbirth) != 1L || !is.logical(age0isbirth) || is.na(age0isbirth)){stop("age0isbirth must be logical of length one (\"TRUE\" or \"FALSE\")")}
  if(toupper(age0to2yr_growthchart) == "FENTON" && age0isbirth == TRUE){warning("age0isbirth is not used age0to2yr_growthchart = \"FENTON\" since PNA is always zero and does not need to be specified")} # opposite of default
  
  ## age2to20yr_correlate_htwt ####
  
  if(length(age2to20yr_correlate_htwt) != 1L || !is.logical(age2to20yr_correlate_htwt) || is.na(age2to20yr_correlate_htwt)){stop("age2to20yr_correlate_htwt must be logical of length one (\"TRUE\" or \"FALSE\")")}
  if(toupper(age0to2yr_growthchart) == "FENTON" && age2to20yr_correlate_htwt == FALSE){warning("age2to20yr_correlate_htwt is not used age0to2yr_growthchart = \"FENTON\" since height is not simulated")} # opposite of default
  
  ## htwt_percentile_min and htwt_percentile_max ####
  
  if(!is.null(htwt_percentile_min) && (length(htwt_percentile_min) != 1L || !is.numeric(htwt_percentile_min) || htwt_percentile_min < 0.001 || htwt_percentile_min > 0.998)){stop("htwt_percentile_min must be numeric of length one and between 0.001 to 0.998, inclusively")}
  if(!is.null(htwt_percentile_max) && (length(htwt_percentile_max) != 1L || !is.numeric(htwt_percentile_max) || htwt_percentile_max < 0.002 || htwt_percentile_max > 0.999)){stop("htwt_percentile_max must be numeric of length one and between 0.002 to 0.999, inclusively")}
  if(!is.null(htwt_percentile_min) && !is.null(htwt_percentile_max) && (htwt_percentile_min >= htwt_percentile_max)){stop("htwt_percentile_min must be less than htwt_percentile_max")}
  if(toupper(age0to2yr_growthchart) == "FENTON" && !is.null(htwt_percentile_min) && htwt_percentile_min < 0.01){warning("htwt_percentile_min < 0.01 for age0to2yr_growthchart = \"FENTON\" may produce nonviable or unrealistic birth weights")}
  if(toupper(age0to2yr_growthchart) == "FENTON" && !is.null(htwt_percentile_max) && htwt_percentile_max > 0.99){warning("htwt_percentile_max > 0.99 for age0to2yr_growthchart = \"FENTON\" may produce nonviable or unrealistic birth weights")}
  
  ## masterseed ####
  
  if(!is.null(masterseed) && (length(masterseed) != 1L || !is.numeric(masterseed) || masterseed%%1!=0 || masterseed <= 0)){stop("masterseed must be a positive integer of length one or \"NULL\" for no seed")}
  
  ## end ####
}