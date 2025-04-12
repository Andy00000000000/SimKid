## code to prepare `kid0` dataset goes here

# kid0.csv in data-raw and kid0.rda in data was prepared on April 11, 2025
# cdc0.csv, who0.csv, and fent0.csv must be created first

`%>%` <- magrittr::`%>%`

cdc0 <- read.csv("data-raw/cdc0.csv",stringsAsFactors = F)
who0 <- read.csv("data-raw/who0.csv",stringsAsFactors = F)
fent0 <- read.csv("data-raw/fent0.csv",stringsAsFactors = F)

kid0 <- cdc0 %>% dplyr::bind_rows(who0) %>% dplyr::bind_rows(fent0)

write.csv(kid0,"data-raw/kid0.csv", row.names = F, quote = T,na = "")
usethis::use_data(kid0, overwrite = TRUE)

## END ####
