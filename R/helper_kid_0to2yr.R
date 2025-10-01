#' Helper function for `sim_kid()` to simulate height and weight per CDC or WHO 
#' Growth Charts for Ages 0 to 2 year
#'
#' @param demo0 Input data frame of demographics with columns of `SEXF` and 
#' `GAWK`.
#' @param seedl Vector of integers specifying seeds for reproducibility.
#' @param seedindex Integer specifying the seed index to begin with.
#' @param zscore_min Minimum allowed Z score for weight.
#' @param zscore_max Maximum allowed Z score for weight.
#' @param age0isbirth A logical that specifies whether age equal to zero 
#' denotes birth.
#'   * `TRUE`: Age of `0` is birth.
#'   * `FALSE` (the default): Age of `0` is ages from birth to less than one 
#'   month.
#' @param age0to2yr_growthchart A string that specifies which anthropometric 
#' growth charts are used for ages less than or equal to 2 years old.
#'   * `"CDC"` (the default): United States Centers for Disease Control and 
#'   Prevention growth charts are used.
#'   * `"WHO"`: World Health Organization growth charts are used.
#' @param sim_z A logical that specifies whether to simulate Z score 
#' variability.
#'   * `TRUE` (the default).
#'   * `FALSE`.
#'
#' @return A data frame with rows and columns matching `demo0`.
#'
#' @noRd
helper_kid_0to2yr <- function(
    demo0 = NULL,
    seedl = NULL,
    seedindex = NULL,
    zscore_min = NULL,
    zscore_max = NULL,
    age0isbirth = NULL,
    age0to2yr_growthchart = NULL,
    sim_z = TRUE
){
  ## MANIP TO READY ####
  
  demo <- demo0 %>% dplyr::filter(.data$AGEMO <= 24)
  
  ped0 <- internal_kid0 %>%
    dplyr::filter(.data$CHART == age0to2yr_growthchart) %>%
    dplyr::mutate(AGEMO = substr(.data$AGEGRP, 2, (nchar(.data$AGEGRP)-1)))%>%
    dplyr::mutate(AGEMO = gsub(",.*", "", .data$AGEMO))%>%
    dplyr::mutate(AGEMO = suppressWarnings(as.numeric(.data$AGEMO)))%>%
    dplyr::filter(.data$AGEMO <= 24)
  
  if(age0isbirth == TRUE){
    ped0 <- ped0 %>% dplyr::filter(.data$AGEGRP != "(0,1)")
  }else{
    ped0 <- ped0 %>% dplyr::filter(.data$AGEGRP != "[0,0]")
  }
  
  htwt0 <- internal_htwt0 %>%
    dplyr::filter(.data$CHART == age0to2yr_growthchart) %>%
    dplyr::filter(.data$VAR == "HTWT")%>%
    dplyr::mutate(HTCM = substr(.data$HTCMGRP, 2, (nchar(.data$HTCMGRP)-1)))%>%
    dplyr::mutate(HTCM = gsub(",.*", "", .data$HTCM))%>%
    dplyr::mutate(HTCM = as.numeric(.data$HTCM))%>%
    dplyr::filter(.data$HTCMGRP != "[77,77]", .data$HTCMGRP != "[45,45]")
  
  digits_htcm <- ifelse(age0to2yr_growthchart == "CDC", 0, 1)
  
  ## SIMULATE VARIABILITY ####
  
  if(sim_z == TRUE){
    
    withr::with_seed(
      seedl[seedindex], 
      demo$ZHTCM <- msm::rtnorm(nrow(demo), 0, 1, zscore_min, zscore_max)
    )
    seedindex <- seedindex + 1
    
    withr::with_seed(
      seedl[seedindex], 
      demo$ZWTKG <- msm::rtnorm(nrow(demo), 0, 1, zscore_min, zscore_max)
    )
    seedindex <- seedindex + 1
  }
  
  ## CALCULATE HTCM ####
  
  demo <- demo %>%
    dplyr::mutate(VAR = "HTCM")%>%
    dplyr::left_join(ped0, by = c("VAR","SEXF","AGEMO"))%>%
    dplyr::mutate(
      HTCM = lms_calc(z = .data$ZHTCM, l = .data$L, m = .data$M, s = .data$S)
    )%>%
    dplyr::mutate(HTCM_DIGITS = digits_htcm)%>%
    dplyr::mutate(
      HTCM = ifelse(
        .data$HTCM_DIGITS == 1, 
        0.5*round(2*.data$HTCM), 
        round(.data$HTCM)
      )
    ) # nearest 0.5
  
  demo <- demo[,which(colnames(demo) %in% colnames(demo0))]
  
  ## CALCULATE WTKG ####
  
  demo <- demo %>% # ZZZ lower limit of weight vs height chart
    dplyr::mutate(HTCM = ifelse(.data$HTCM < 45, 45, .data$HTCM)) %>%
    dplyr::left_join(htwt0, by = c("SEXF","HTCM"))%>%
    dplyr::mutate(
      WTKG = lms_calc(z = .data$ZWTKG, l = .data$L, m = .data$M, s = .data$S)
    )
  
  demo <- demo[,which(colnames(demo) %in% colnames(demo0))]
  
  ## COMBINE WITH DEMO0 ####
  
  demo <- suppressWarnings(
    demo %>%
      dplyr::select(
        .data$ID, .data$AGEMO, .data$WTKG, 
        .data$ZWTKG, .data$HTCM, .data$ZHTCM, .data$CHART
      )%>%
      dplyr::rename(
        WTKG1 = .data$WTKG, ZWTKG1 = .data$ZWTKG, 
        HTCM1 = .data$HTCM, ZHTCM1 = .data$ZHTCM, 
        CHART1 = .data$CHART
      )
  )
  
  demo <- suppressWarnings(
    demo0 %>%
      dplyr::left_join(demo, by = c("ID","AGEMO"))%>%
      dplyr::mutate(
        WTKG = ifelse(is.na(.data$WTKG), .data$WTKG1, .data$WTKG)
      )%>%
      dplyr::mutate(
        ZWTKG = ifelse(is.na(.data$ZWTKG), .data$ZWTKG1, .data$ZWTKG)
      )%>%
      dplyr::mutate(
        HTCM = ifelse(is.na(.data$HTCM), .data$HTCM1, .data$HTCM)
      )%>%
      dplyr::mutate(
        ZHTCM = ifelse(is.na(.data$ZHTCM), .data$ZHTCM1, .data$ZHTCM)
      )%>%
      dplyr::mutate(
        CHART = ifelse(is.na(.data$CHART), .data$CHART1, .data$CHART)
      )%>%
      dplyr::select(
        -.data$WTKG1, -.data$ZWTKG1, 
        -.data$HTCM1, -.data$ZHTCM1, 
        -.data$CHART1
      )
  )
  
  ## RETURN ####
  
  list(demo = demo, seedindex = seedindex)
  
  ## END ####
}