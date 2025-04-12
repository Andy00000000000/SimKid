## code to prepare `sysdata` dataset goes here

internal_kid0 <- read.csv("data-raw/kid0.csv",stringsAsFactors = F)
internal_htwt0 <- read.csv("data-raw/htwt0.csv",stringsAsFactors = F)
internal_cdc_cor <- read.csv("data-raw/cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized.csv",stringsAsFactors = F)

usethis::use_data(internal_kid0,internal_htwt0,internal_cdc_cor, overwrite = TRUE, internal = TRUE)

## END ####
