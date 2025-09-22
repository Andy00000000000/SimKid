#' Helper function for `sim_kid()` to simulate height weight per CDC or WHO Growth Charts for Ages 2 to 20 year
#'
#' @param demo0 Input data frame of demographics with columns of `SEXF` and `GAWK`.
#' @param seedl Vector of integers specifying seeds for reproducibility.
#' @param seedindex Integer specifying the seed index to begin with.
#' @param zscore_min Minimum allowed Z score for weight.
#' @param zscore_max Maximum allowed Z score for weight.
#' @param age2to20yr_correlate_htwt A logical that specifies whether correlations, by sex and year of age, are implemented between simulated height and simulated weight for ages greater than or equal to 2 years old.
#'   * `TRUE` (the default): Correlations are implemented between simulated height and simulated weight according to an identical internal-systems-data version of [cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized] located within the `data` folder.
#'   * `FALSE`: Height and weight are simulated independently without any correlation(s). Note that this will likely result in unrealistic virtual subjects.
#'
#' @return A data frame with rows and columns matching `demo0`.
#'
#' @noRd
helper_kid_2to20yr <- function(
    demo0 = NULL,
    seedl = NULL,
    seedindex = NULL,
    zscore_min = NULL,
    zscore_max = NULL,
    age2to20yr_correlate_htwt = NULL
){
  ## MANIP TO READY ####
  
  demo <- demo0 %>% dplyr::filter(.data$AGEMO > 24)
  
  ped0 <- internal_kid0 %>%
    dplyr::mutate(AGEMO = substr(.data$AGEGRP, 2, (nchar(.data$AGEGRP)-1)))%>%
    dplyr::mutate(AGEMO = gsub(",.*", "", .data$AGEMO))%>%
    dplyr::mutate(AGEMO = suppressWarnings(as.numeric(.data$AGEMO)))%>%
    dplyr::filter(.data$AGEMO > 24) %>%
    dplyr::filter(.data$CHART == "CDC")
  
  digits_htcm <- 0
  
  ## GENERATE VARIABILITY ####
  
  if(nrow(demo)>0){
    if(age2to20yr_correlate_htwt == FALSE){
      
      withr::with_seed(seedl[seedindex], demo$ZHTCM <- msm::rtnorm(nrow(demo), 0, 1, zscore_min, zscore_max))
      seedindex <- seedindex + 1
      
      withr::with_seed(seedl[seedindex], demo$ZWTKG <- msm::rtnorm(nrow(demo), 0, 1, zscore_min, zscore_max))
      seedindex <- seedindex + 1
      
    }else{
      
      demo <- demo %>%
        dplyr::mutate(TMPAGEGRP = ifelse(
          .data$AGEMO >= 25 & .data$AGEMO < 36, 
          "[25,36)", 
          paste0("[",12*floor(.data$AGE),",",12*(floor(.data$AGE)+1),")"))
        )
      
      xi_list <- demo %>% dplyr::distinct(.data$SEXF,.data$TMPAGEGRP)
      for(xi in 1:nrow(xi_list)){
        
        cov_in <- internal_cdc_cor[which(internal_cdc_cor$AGEGRP == xi_list[xi,"TMPAGEGRP"] & internal_cdc_cor$SEXF == xi_list[xi,"SEXF"]), "MEAN_HTWT_COR"] # covariance and correlation are equal since SD=1 (Z score distributions)
        id_index <- which(demo$TMPAGEGRP == xi_list[xi,"TMPAGEGRP"] & demo$SEXF==xi_list[xi,"SEXF"])
        
        withr::with_seed(seedl[seedindex], tmpz <- tmvtnorm::rtmvnorm(length(id_index), rep(0, 2), matrix(c(1,cov_in,cov_in,1), ncol = 2), lower = rep(zscore_min,2), upper = rep(zscore_max,2)))
        seedindex <- seedindex + 1
        
        demo[id_index,"ZHTCM"] <- tmpz[,1]
        demo[id_index,"ZWTKG"] <- tmpz[,2]
      }
      
      demo <- demo %>%
        dplyr::select(-.data$TMPAGEGRP)
    }
  }
  
  ## CALCULATE HTCM ####
  
  demo <- demo %>%
    dplyr::mutate(VAR = "HTCM")%>%
    dplyr::left_join(ped0, by = c("VAR","SEXF","AGEMO"))%>%
    dplyr::mutate(HTCM = ifelse(round(.data$L,1E-6) == 0, .data$M*exp(.data$S*.data$ZHTCM), .data$M*(1+.data$L*.data$S*.data$ZHTCM)^(1/.data$L)))%>%
    dplyr::mutate(HTCM_DIGITS = digits_htcm)%>%
    dplyr::mutate(HTCM = ifelse(.data$HTCM_DIGITS == 1, 0.5*round(2*.data$HTCM), round(.data$HTCM))) # nearest 0.5
  
  demo <- demo[,which(colnames(demo) %in% colnames(demo0))]
  
  ## CALCULATE WTKG ####
  
  demo <- demo %>%
    dplyr::mutate(VAR = "WTKG")%>%
    dplyr::left_join(ped0, by = c("VAR","SEXF","AGEMO"))%>%
    dplyr::mutate(WTKG = ifelse(round(.data$L,1E-6) == 0, .data$M*exp(.data$S*.data$ZWTKG), .data$M*(1+.data$L*.data$S*.data$ZWTKG)^(1/.data$L)))
  
  demo <- demo[,which(colnames(demo) %in% colnames(demo0))]
  
  ## RETURN ####
  
  return(list(demo = demo, seedindex = seedindex))
  
  ## END ####
}