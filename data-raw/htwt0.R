## code to prepare `htwt0` dataset goes here

# htwt0.csv in data-raw and htwt0.rda in data was prepared on April 11, 2025

## DOWNLOAD GROWTH CHARTS ####

if (!file.exists("data-raw/wtstat.csv")) {
  download.file("https://www.cdc.gov/growthcharts/data/zscore/wtstat.csv", "data-raw/wtstat.csv")
}

if (!file.exists("data-raw/wtleninf.csv")) {
  download.file("https://www.cdc.gov/growthcharts/data/zscore/wtleninf.csv", "data-raw/wtleninf.csv")
}

if (!file.exists("data-raw/WHO-Boys-Weight-for-length-Percentiles.csv")) {
  download.file("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/growthcharts/WHO-Boys-Weight-for-length-Percentiles.csv", "data-raw/WHO-Boys-Weight-for-length-Percentiles.csv")
}

if (!file.exists("data-raw/WHO-Girls-Weight-for-length-Percentiles.csv")) {
  download.file("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/growthcharts/WHO-Girls-Weight-for-length-Percentiles.csv", "data-raw/WHO-Girls-Weight-for-length-Percentiles.csv")
}

## CREATE HTWT0 (MANIPULATED DATASET) FROM ORIGINAL RAW CDC AND WHO GROWTH CHARTS ####

`%>%` <- magrittr::`%>%`

load_htwt <- function(dir = "data-raw"){
  
  cdc_htwt_inf <- read.csv(paste0(dir,"/wtleninf.csv"), stringsAsFactors = F)%>%
    dplyr::mutate_if(is.character,as.numeric)%>%
    dplyr::filter(!is.na(Length))%>%
    dplyr::mutate(VAR="HTWT")%>%
    dplyr::mutate(SEXF = ifelse(Sex == 1, 0, ifelse(Sex == 2, 1, NA)))%>%
    dplyr::rename(HTCM = Length)%>%
    dplyr::mutate(CHART = "CDC")%>%
    dplyr::arrange(dplyr::desc(VAR),SEXF,HTCM)%>%
    dplyr::group_by(VAR,SEXF)%>%
    dplyr::mutate(LSHTCM = dplyr::lag(HTCM))%>%
    dplyr::mutate(NXHTCM = dplyr::lead(HTCM))%>%
    dplyr::ungroup()%>%
    as.data.frame()%>%
    dplyr::mutate(LOHTCM = ifelse(is.na(NXHTCM), HTCM-(HTCM-LSHTCM)/2, HTCM - (NXHTCM-HTCM)/2))%>%
    dplyr::mutate(UPHTCM = ifelse(is.na(NXHTCM), HTCM+(HTCM-LOHTCM), NXHTCM - (NXHTCM-HTCM)/2))%>%
    dplyr::mutate(LOHTCM = ifelse(HTCM == 45, HTCM, LOHTCM))%>%
    dplyr::mutate(UPHTCM = ifelse(HTCM == 45, HTCM, UPHTCM))%>%
    dplyr::mutate(HTCMGRP = paste0("[",LOHTCM,",",UPHTCM,ifelse(LOHTCM!=UPHTCM, ")", "]")))%>%
    dplyr::mutate(HTCMGRP = ifelse(HTCMGRP == "[45,46)", "(45,46)", HTCMGRP))%>%
    dplyr::select(CHART,VAR,SEXF,HTCMGRP,L,M,S,P3,P5,P10,P25,P50,P75,P90,P95,P97)
  
  cdc_htwt <- read.csv(paste0(dir,"/wtstat.csv"), stringsAsFactors = F)%>%
    dplyr::mutate_if(is.character,as.numeric)%>%
    dplyr::filter(!is.na(Height))%>%
    dplyr::mutate(VAR="HTWT_GT2YR")%>%
    dplyr::mutate(SEXF = ifelse(Sex == 1, 0, ifelse(Sex == 2, 1, NA)))%>%
    dplyr::rename(HTCM = Height)%>%
    dplyr::mutate(CHART = "CDC")%>%
    dplyr::arrange(dplyr::desc(VAR),SEXF,HTCM)%>%
    dplyr::group_by(VAR,SEXF)%>%
    dplyr::mutate(LSHTCM = dplyr::lag(HTCM))%>%
    dplyr::mutate(NXHTCM = dplyr::lead(HTCM))%>%
    dplyr::ungroup()%>%
    as.data.frame()%>%
    dplyr::mutate(LOHTCM = ifelse(is.na(NXHTCM), HTCM-(HTCM-LSHTCM)/2, HTCM - (NXHTCM-HTCM)/2))%>%
    dplyr::mutate(UPHTCM = ifelse(is.na(NXHTCM), HTCM+(HTCM-LOHTCM), NXHTCM - (NXHTCM-HTCM)/2))%>%
    dplyr::mutate(LOHTCM = ifelse(HTCM == 77, HTCM, LOHTCM))%>%
    dplyr::mutate(UPHTCM = ifelse(HTCM == 77, HTCM, UPHTCM))%>%
    dplyr::mutate(HTCMGRP = paste0("[",LOHTCM,",",UPHTCM,ifelse(LOHTCM!=UPHTCM, ")", "]")))%>%
    dplyr::mutate(HTCMGRP = ifelse(HTCMGRP == "[77,78)", "(77,78)", HTCMGRP))%>%
    dplyr::select(CHART,VAR,SEXF,HTCMGRP,L,M,S,P3,P5,P10,P25,P50,P75,P90,P95,P97)
  
  who_htwt_male <- read.csv(paste0(dir,"/WHO-Boys-Weight-for-length-Percentiles.csv"), stringsAsFactors = F)%>%
    dplyr::mutate(SEXF=0)%>%
    dplyr::mutate(VAR="HTWT")
  
  who_htwt_female <- read.csv(paste0(dir,"/WHO-Girls-Weight-for-length-Percentiles.csv"), stringsAsFactors = F)%>%
    dplyr::mutate(SEXF=1)%>%
    dplyr::mutate(VAR="HTWT")
  
  who_htwt <- who_htwt_male %>%
    dplyr::bind_rows(who_htwt_female)%>%
    dplyr::rename(P3=X2nd..2.3rd., P5=X5th, P10=X10th, P25=X25th, P50=X50th, P75=X75th, P90=X90th, P95=X95th, P97=X98th..97.7th.)%>%
    dplyr::rename(HTCM = Length)%>%
    dplyr::mutate(CHART = "WHO")%>%
    dplyr::arrange(dplyr::desc(VAR),SEXF,HTCM)%>%
    dplyr::group_by(VAR,SEXF)%>%
    dplyr::mutate(LSHTCM = dplyr::lag(HTCM))%>%
    dplyr::mutate(NXHTCM = dplyr::lead(HTCM))%>%
    dplyr::ungroup()%>%
    as.data.frame()%>%
    dplyr::mutate(LOHTCM = HTCM)%>%
    dplyr::mutate(UPHTCM = ifelse(is.na(NXHTCM), HTCM+0.5, NXHTCM))%>%
    dplyr::mutate(HTCMGRP = paste0("[",LOHTCM,",",UPHTCM,ifelse(LOHTCM!=UPHTCM, ")", "]")))%>%
    dplyr::select(CHART,VAR,SEXF,HTCMGRP,L,M,S,P3,P5,P10,P25,P50,P75,P90,P95,P97)
  
  htwt <- cdc_htwt_inf %>%
    dplyr::bind_rows(cdc_htwt)%>%
    dplyr::bind_rows(who_htwt)
  
  return(htwt)
}

htwt0 <- suppressWarnings(load_htwt())

## WRITE HTWT0 ####

write.csv(htwt0, "data-raw/htwt0.csv", row.names = F, quote = T, na = "")
usethis::use_data(htwt0, overwrite = TRUE)

## END ####
