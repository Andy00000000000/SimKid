## code to prepare `who0` dataset goes here

# who0.csv in data-raw and who0.rda in data was prepared on April 11, 2025

## DOWNLOAD WHO GROWTH CHARTS ####

### Birth to 36 Months ####

if (!file.exists("data-raw/WHO-Boys-Weight-for-age-Percentiles.csv")) {
  download.file(
    paste0(
      "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/growthcharts/",
      "WHO-Boys-Weight-for-age-Percentiles.csv"
    ), 
    "data-raw/WHO-Boys-Weight-for-age-Percentiles.csv"
  )
}

if (!file.exists("data-raw/WHO-Girls-Weight-for-age-Percentiles.csv")) {
  download.file(
    paste0(
      "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/growthcharts/",
      "WHO-Girls-Weight-for-age%20Percentiles.csv"
    ), 
    "data-raw/WHO-Girls-Weight-for-age-Percentiles.csv"
  )
}

if (!file.exists("data-raw/WHO-Boys-Length-for-age-Percentiles.csv")) {
  download.file(
    paste0(
      "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/growthcharts/",
      "WHO-Boys-Length-for-age-Percentiles.csv"
    ),
    "data-raw/WHO-Boys-Length-for-age-Percentiles.csv"
  )
}

if (!file.exists("data-raw/WHO-Girls-Length-for-age-Percentiles.csv")) {
  download.file(
    paste0(
      "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/growthcharts/",
      "WHO-Girls-Length-for-age-Percentiles.csv"
    ), 
    "data-raw/WHO-Girls-Length-for-age-Percentiles.csv"
  )
}

## CREATE WHO0 (MANIPULATED DATASET) FROM ORIGINAL RAW WHO GROWTH CHARTS ####

`%>%` <- magrittr::`%>%`

load_who <- function(dir = "data-raw"){
  
  who_wtageinf_male <- read.csv(
    paste0(dir,"/WHO-Boys-Weight-for-age-Percentiles.csv"), 
    stringsAsFactors = FALSE
  )%>%
    dplyr::mutate(SEXF=0)%>%
    dplyr::mutate(VAR="WTKG")
  
  who_wtageinf_female <- read.csv(
    paste0(dir,"/WHO-Girls-Weight-for-age-Percentiles.csv"), 
    stringsAsFactors = FALSE
  )%>%
    dplyr::mutate(SEXF=1)%>%
    dplyr::mutate(VAR="WTKG")
  
  who_htageinf_male <- read.csv(
    paste0(dir,"/WHO-Boys-Length-for-age-Percentiles.csv"), 
    stringsAsFactors = FALSE
  )%>%
    dplyr::mutate(SEXF=0)%>%
    dplyr::mutate(VAR="HTCM")
  
  who_htageinf_female <- read.csv(
    paste0(dir,"/WHO-Girls-Length-for-age-Percentiles.csv"), 
    stringsAsFactors = FALSE
  )%>%
    dplyr::mutate(SEXF=1)%>%
    dplyr::mutate(VAR="HTCM")
  
  who0 <- who_wtageinf_male %>%
    dplyr::bind_rows(who_wtageinf_female)%>%
    dplyr::bind_rows(who_htageinf_male)%>%
    dplyr::bind_rows(who_htageinf_female)%>%
    dplyr::rename(
      AGEMO = Month,P3=X2nd..2.3rd., P5=X5th, P10=X10th, P25=X25th, P50=X50th,
      P75=X75th, P90=X90th, P95=X95th, P97=X98th..97.7th.
    )%>%
    dplyr::mutate(CHART = "WHO")%>%
    dplyr::arrange(dplyr::desc(VAR),SEXF,AGEMO)%>%
    dplyr::group_by(VAR,SEXF)%>%
    dplyr::mutate(LSAGEMO = dplyr::lag(AGEMO))%>%
    dplyr::mutate(NXAGEMO = dplyr::lead(AGEMO))%>%
    dplyr::ungroup()%>%
    as.data.frame()%>%
    dplyr::mutate(LOAGEMO = AGEMO)%>%
    dplyr::mutate(UPAGEMO = ifelse(is.na(NXAGEMO), AGEMO, NXAGEMO))%>%
    dplyr::mutate(
      AGEGRP = paste0(
        "[",
        LOAGEMO,
        ",",
        UPAGEMO,
        ifelse(LOAGEMO!=UPAGEMO, ")", "]")
      )
    )%>%
    dplyr::select(CHART,VAR,SEXF,AGEGRP,L,M,S,P3,P5,P10,P25,P50,P75,P90,P95,P97)
}

who0 <- suppressWarnings(load_who())

## WRITE WHO0 ####

write.csv(who0, "data-raw/who0.csv", row.names = F, quote = T, na = "")
usethis::use_data(who0, overwrite = TRUE)

## END ####
