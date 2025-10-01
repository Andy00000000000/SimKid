## code to prepare `cdc0` dataset goes here

# cdc0.csv in data-raw and cdc0.rda in data was prepared on April 11, 2025

## DOWNLOAD CDC GROWTH CHARTS ####

### 2 to 20 Years ####

if (!file.exists("data-raw/bmiagerev.csv")) {
  download.file(
    "https://www.cdc.gov/growthcharts/data/zscore/bmiagerev.csv", 
    "data-raw/bmiagerev.csv"
  )
}

if (!file.exists("data-raw/statage.csv")) {
  download.file(
    "https://www.cdc.gov/growthcharts/data/zscore/statage.csv", 
    "data-raw/statage.csv"
  )
}

if (!file.exists("data-raw/wtage.csv")) {
  download.file(
    "https://www.cdc.gov/growthcharts/data/zscore/wtage.csv", 
    "data-raw/wtage.csv"
  )
}

### Birth to 36 Months ####

if (!file.exists("data-raw/wtageinf.csv")) {
  download.file(
    "https://www.cdc.gov/growthcharts/data/zscore/wtageinf.csv", 
    "data-raw/wtageinf.csv"
  )
}

if (!file.exists("data-raw/lenageinf.csv")) {
  download.file(
    "https://www.cdc.gov/growthcharts/data/zscore/lenageinf.csv", 
    "data-raw/lenageinf.csv"
  )
}

## CREATE CDC0 (MANIPULATED DATASET) FROM ORIGINAL RAW CDC GROWTH CHARTS ####

`%>%` <- magrittr::`%>%`

load_cdc <- function(dir = "data-raw"){
  
  cdc_wtage <- read.csv(paste0(dir,"/wtage.csv"),stringsAsFactors = F)%>%
    dplyr::mutate_if(is.character,as.numeric)%>%
    dplyr::filter(!is.na(Agemos))%>%
    dplyr::mutate(VAR="WTKG")
  
  cdc_htage <- read.csv(paste0(dir,"/statage.csv"),stringsAsFactors = F)%>%
    dplyr::mutate_if(is.character,as.numeric)%>%
    dplyr::filter(!is.na(Agemos))%>%
    dplyr::mutate(VAR="HTCM")
  
  cdc_bmiage <- read.csv(paste0(dir,"/bmiagerev.csv"),stringsAsFactors = F)%>%
    dplyr::mutate_if(is.character,as.numeric)%>%
    dplyr::filter(!is.na(Agemos))%>%
    dplyr::select(-P85)%>%
    dplyr::mutate(VAR="BMI")
  
  cdc_wtageinf <- read.csv(paste0(dir,"/wtageinf.csv"),stringsAsFactors = F)%>%
    dplyr::mutate_if(is.character,as.numeric)%>%
    dplyr::filter(!is.na(Agemos))%>%
    dplyr::mutate(VAR="WTKG")
  
  cdc_htageinf <- read.csv(paste0(dir,"/lenageinf.csv"),stringsAsFactors = F)%>%
    dplyr::mutate_if(is.character,as.numeric)%>%
    dplyr::filter(!is.na(Agemos))%>%
    dplyr::mutate(VAR="HTCM")
  
  # cdc_wthtinf <- read.csv(paste0(dir,"/wtstat.csv"),stringsAsFactors = F)%>%
  #   dplyr::mutate_if(is.character,as.numeric)%>%
  #   dplyr::filter(!is.na(Height))%>%
  #   dplyr::mutate(VAR="WTHT")
  
  cdc0 <- cdc_wtageinf %>% dplyr::filter(Agemos <= 24) %>%
    dplyr::bind_rows(cdc_wtage)%>%
    dplyr::bind_rows(cdc_htageinf %>% dplyr::filter(Agemos <= 24))%>%
    dplyr::bind_rows(cdc_htage)%>%
    dplyr::bind_rows(cdc_bmiage)%>%
    dplyr::distinct()%>%
    dplyr::mutate(SEXF = ifelse(Sex == 1, 0, ifelse(Sex == 2, 1, NA)))%>%
    dplyr::rename(AGEMO = Agemos)%>%
    dplyr::select(-Sex)%>%
    dplyr::arrange(dplyr::desc(VAR),SEXF,AGEMO)%>%
    dplyr::filter(AGEMO != 24, AGEMO != 240)%>%
    dplyr::group_by(VAR,SEXF)%>%
    dplyr::mutate(LSAGEMO = dplyr::lag(AGEMO))%>%
    dplyr::mutate(NXAGEMO = dplyr::lead(AGEMO))%>%
    dplyr::ungroup()%>%
    as.data.frame()%>%
    dplyr::mutate(
      LOAGEMO = ifelse(
        is.na(NXAGEMO), 
        AGEMO-(AGEMO-LSAGEMO)/2, 
        AGEMO - (NXAGEMO-AGEMO)/2
      )
    )%>%
    dplyr::mutate(
      UPAGEMO = ifelse(
        is.na(NXAGEMO), 
        AGEMO+(AGEMO-LOAGEMO), 
        NXAGEMO - (NXAGEMO-AGEMO)/2
      )
    )%>%
    dplyr::mutate(LOAGEMO = ifelse(AGEMO == 0, AGEMO, LOAGEMO))%>%
    dplyr::mutate(UPAGEMO = ifelse(AGEMO == 0, AGEMO, UPAGEMO))%>%
    # dplyr::mutate(UPAGEMO = ifelse(UPAGEMO == 241, 240, UPAGEMO))%>%
    dplyr::mutate(
      AGEGRP = paste0(
        "[", LOAGEMO, ",", UPAGEMO, ifelse(LOAGEMO!=UPAGEMO, ")", "]")
      )
    )%>%
    dplyr::mutate(AGEGRP = ifelse(AGEGRP == "[0,1)", "(0,1)", AGEGRP))%>%
    dplyr::distinct(VAR,AGEMO,SEXF,.keep_all = T)%>%
    dplyr::select(-LSAGEMO,-NXAGEMO)%>%
    dplyr::mutate(CHART = "CDC")%>%
    dplyr::select(CHART,VAR,SEXF,AGEGRP,L,M,S,P3,P5,P10,P25,P50,P75,P90,P95,P97)
}

cdc0 <- suppressWarnings(load_cdc())

## WRITE CDC0 ####

write.csv(cdc0, "data-raw/cdc0.csv", row.names = F, quote = T, na = "")
usethis::use_data(cdc0, overwrite = TRUE)

## END ####
