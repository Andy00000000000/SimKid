# code to prepare `cdc_ages2to20yr_correlations-by-sex_htcm-wtkg_summarized` 
# dataset goes here

# cdc_ages2to20yr_correlations-by-sex_htcm-wtkg_allreps.csv must be created
# first

## METHODS ####

# The mean correlations over the 10x replicates is calculated for use in the 
# SimKid package. In the SimKid package the optimized correlations are validated
# by simulating virtual populations, calculating BMI statistics, and overlaying 
# with the respective CDC BMI vs. age growth charts.

## READY ####

library(dplyr)
library(ggplot2)

## LOAD ####

htwt_cor_listing_all <- read.csv(
  "data-raw/cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates.csv",
  stringsAsFactors = F
)

## SUMMARIZE ####

num_per_age <- 2000
# from cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates.R

htwt_cor_listing <- htwt_cor_listing_all %>%
  group_by(ITER)%>%
  mutate(ROWN=row_number())%>%
  ungroup()%>%
  as.data.frame()%>%
  mutate(NSUBJ_AGEMO_SEXF = num_per_age/2)%>%
  group_by(SEXF,AGEGRP)%>%
  summarise(
    NITER = n(), 
    NSUBJ_AGEMO_SEXF = unique(NSUBJ_AGEMO_SEXF), 
    MEAN_HTWT_COR = mean(HTWT_COR), 
    ROWN = unique(ROWN)
  )%>%
  ungroup()%>%
  as.data.frame()%>%
  arrange(ROWN)%>%
  select(-ROWN)

## THEME AND PLOT ####

theme_set(theme_bw())
theme_update(
  axis.text.x = element_text(angle = -45, vjust = 0, hjust = 0.5)
)

tmpdf <- htwt_cor_listing_all%>%
  group_by(ITER,SEXF)%>%
  mutate(AGEGRPN=row_number())%>%
  ungroup()%>%
  as.data.frame()%>%
  mutate(SEXF = ifelse(SEXF == 1, "Female","Male"))

tmpdfsumm <- htwt_cor_listing%>%
  group_by(SEXF)%>%
  mutate(AGEGRPN=row_number())%>%
  ungroup()%>%
  as.data.frame()%>%
  mutate(SEXF = ifelse(SEXF == 1, "Female","Male"))

p1<- ggplot()+
  geom_line(aes(x=AGEGRPN,y=HTWT_COR,group=as.factor(ITER)),tmpdf)+
  geom_point(aes(x=AGEGRPN,y=HTWT_COR,group=as.factor(ITER)),tmpdf)+
  geom_line(aes(x=AGEGRPN,y=MEAN_HTWT_COR),tmpdfsumm,color="red2")+
  geom_point(aes(x=AGEGRPN,y=MEAN_HTWT_COR),tmpdfsumm,color="red2")+
  facet_wrap(~SEXF)+
  scale_x_continuous(
    breaks = unique(tmpdf$AGEGRPN), labels = unique(tmpdf$AGEGRP)
  )+
  labs(
    x = "Age Bucket (month)",
    y = "Height and Weight Z-Score Correlation"
  )

rm(tmpdf,tmpdfsumm)

print(p1)

## WRITE ####

cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized <- htwt_cor_listing
write.csv(
  cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized, 
  "data-raw/cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized.csv"
)
usethis::use_data(
  cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_summarized, 
  overwrite = TRUE
)

## END ####