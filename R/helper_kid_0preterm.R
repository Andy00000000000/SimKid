#' Helper function for `sim_kid()` to simulate weight per Fenton Growth Chart 
#' for preterm newborns
#'
#' @param demo0 Input data frame of demographics with columns of `SEXF` and 
#' `GAWK`.
#' @param seedl Vector of integers specifying seeds for reproducibility.
#' @param seedindex Integer specifying the seed index to begin with.
#' @param zscore_min Minimum allowed Z score for weight.
#' @param zscore_max Maximum allowed Z score for weight.
#'
#' @return A data frame with rows and columns matching `demo0`.
#'
#' @noRd
helper_kid_0preterm <- function(
    demo0 = NULL,
    seedl = NULL,
    seedindex = NULL,
    zscore_min = NULL,
    zscore_max = NULL
){
  ## MANIP TO READY ####
  
  demo <- demo0 %>% dplyr::filter(.data$AGEMO == 0)
  
  ped0 <- internal_kid0 %>%
    dplyr::filter(.data$CHART == "FENTON")%>%
    dplyr::mutate(GAWK = substr(.data$AGEGRP,17,(nchar(.data$AGEGRP)-1)))%>%
    dplyr::mutate(GAWK = gsub(",.*", "", .data$GAWK))%>%
    dplyr::mutate(GAWK = as.numeric(.data$GAWK))
  
  ## CALCULATE WTKG ####
  
  withr::with_seed(
    seedl[seedindex], 
    demo$ZWTKG <- msm::rtnorm(nrow(demo), 0, 1, zscore_min, zscore_max)
  )
  seedindex <- seedindex + 1
  
  demo <- demo %>%
    dplyr::mutate(VAR = "WTKG")%>%
    dplyr::left_join(ped0, by = c("VAR","SEXF","GAWK"))%>%
    dplyr::mutate(
      WTKG = lms_calc(z = .data$ZWTKG, l = .data$L, m = .data$M, s = .data$S)
    )
  
  demo <- demo[,which(colnames(demo) %in% colnames(demo0))]
  
  ## RETURN ####
  
  list(demo = demo, seedindex = seedindex)
  
  ## END ####
}
