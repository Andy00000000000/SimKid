## code to prepare `cdc_ages2to20yr_correlations-by-sex_htcm-wtkg_allreplicates` dataset goes here

# cdc0.csv, who0.csv, and htwt0.csv must be created first

## METHODS ####

# Code to optimize the correlation between height and weight by age bucket and sex for CDC growth chart ages 2 to 20 years

# Virtual subject age is created: 1000 males and 1000 females per each month of age ranging from 25 to 239 months.
# Virtual subject height and weight are created using the CDC growth charts (i.e., LMS parameters) and BMI is calculated.
# The height and weight distributions by age are constrained between the 0.1 and 99.9 percentiles (i.e., z-scores from -3 to 3).
# The correlations between z-score of height and z-score of weight are optimized separately by sex and age using a 1-year age bucket (ex. correlation for ages 2-3 yr, 3-4 yr, etc.).
# Percentiles of BMI (3rd, 10th, 25th, 50th, 75th, 90th, 97th) for the virtual population is compared to matching percentiles of observed BMI (i.e. the CDC growth chart of BMI vs age using the lower end of the 1-year age bucket) to calculate sum of squares.
# R optimize function is used to minimize the sum of squares, providing the optimal correlation between z-scores of height and weight per sex and year of age.
# This process is repeated 10x.
# In cdc_ages2to20yr_correlations-by-sex_htcm-wtkg_summarized.R the mean correlations over the 10x replicates is calculated for use in the SimKid package.
# In the SimKid package the optimized correlations are validated by simulating virtual populations, calculating BMI statistics, and overlaying with the respective CDC BMI vs. age growth charts.

## READY ####

rm(list = ls())

library(dplyr)
library(ggplot2)

## USER ####

masterseed <- 514
nreps <- 10
num_per_age <- 2000

## INITIALIZE DEMO ####

demo0 <- data.frame(ID = 1:num_per_age, SEXF = NA, AGEMO = NA, AGE = NA, WTKG = NA, HTCM = NA, BMI = NA, BSA1 = NA, BSA2 = NA, BSA3 = NA)

demo0[1:(nrow(demo0)/2),"SEXF"]<-0 # half male
demo0[(nrow(demo0)/2 + 1):nrow(demo0),"SEXF"]<-1 # half female

## LOAD ####

ped0 <- read.csv("data-raw/kid0",stringsAsFactors = F)
htwt0 <- read.csv("data-raw/htwt0.csv",stringsAsFactors = F)

## FUNCTION OPTIMIZE HTCM AND WTKG CORRELATION FOR 2 TO 20 YR USING BMI-for-Age ####

fun_optim_2to20yr_mvn <- function(demo0 = NULL, ped0 = NULL, htwt0 = NULL, seed = NULL, cor = T, cor_in = NA){
  ### MANIP TO READY ####
  
  set.seed(seed)
  seedl <- sample(1:1E6, 1000)
  
  demo <- demo0 %>% mutate(Z1=NA,Z2=NA) %>% filter(AGEMO > 24)
  
  ped0 <- ped0 %>%
    mutate(AGEMO = substr(AGEGRP,2,(nchar(AGEGRP)-1)))%>%
    mutate(AGEMO = gsub(",.*", "", AGEMO))%>%
    mutate(AGEMO = suppressWarnings(as.numeric(AGEMO)))%>%
    filter(AGEMO > 24) %>%
    filter(CHART == "CDC")
  
  htwt0 <- htwt0 %>%
    filter(VAR == "HTWT_GT2YR")%>%
    mutate(HTCM = substr(HTCMGRP,2,(nchar(HTCMGRP)-1)))%>%
    mutate(HTCM = gsub(",.*", "", HTCM))%>%
    mutate(HTCM = as.numeric(HTCM))%>%
    filter(HTCMGRP != "[77,77]", HTCMGRP != "[45,45]")%>%
    filter(CHART == "CDC")
  
  digits_htcm <- 0
  
  ### GENERATE VARIABILITY ####
  
  if(nrow(demo)>0){
    if(cor == F){
      
      set.seed(seedl[1])
      demo$Z1 <- msm::rtnorm(nrow(demo), 0, 1, -3, 3)
      
      set.seed(seedl[2])
      demo$Z2 <- msm::rtnorm(nrow(demo), 0, 1, -3, 3)
      
    }else{
      
      cov_in <- cor_in # covariance and correlation are equal since SD=1 (Z score distributions)
      
      set.seed(seedl[1])
      tmpz <- tmvtnorm::rtmvnorm(nrow(demo), rep(0, 2), matrix(c(1,cov_in,cov_in,1), ncol = 2), lower = rep(-3,2), upper = rep(3,2))
      demo$Z1 <- tmpz[,1]
      demo$Z2 <- tmpz[,2]
    }
  }
  
  ### CALCULATE HTCM ####
  
  demo <- demo %>%
    mutate(VAR = "HTCM")%>%
    left_join(ped0)%>%
    mutate(HTCM = ifelse(round(L,1E-6) == 0, M*exp(S*Z1), M*(1+L*S*Z1)^(1/L)))%>%
    mutate(HTCM_DIGITS = digits_htcm)%>%
    mutate(HTCM = ifelse(HTCM_DIGITS==1, 0.5*round(2*HTCM), round(HTCM)))%>% # nearest 0.5
    select(ID,SEXF,AGEMO,AGE,AGEGRP,WTKG,HTCM,Z2)
  
  if(any(is.na(demo$HTCM))==T){
    rm(demo)
  }
  
  ### CALCULATE WTKG ####
  
  demo <- demo %>%
    mutate(VAR = "WTKG")%>%
    left_join(ped0)%>%
    mutate(WTKG = ifelse(round(L,1E-6) == 0, M*exp(S*Z2), M*(1+L*S*Z2)^(1/L)))%>%
    mutate(WTKG = round(WTKG,2))%>%
    select(ID,SEXF,AGEMO,AGE,AGEGRP,WTKG,HTCM)
  
  if(any(is.na(demo$WTKG))==T){
    rm(demo)
  }
  
  ### CALCULATE BMI ####
  
  demo <- demo %>%
    mutate(BMI = round(WTKG/((HTCM/100)^2),1))
  
  ### SUMMARIZE BMI ####
  
  summ <- demo %>%
    group_by(AGEGRP,SEXF)%>%
    summarise(
      SP03 = quantile(BMI,probs=0.03),
      SP10 = quantile(BMI,probs=0.10),
      SP25 = quantile(BMI,probs=0.25),
      SP50 = quantile(BMI,probs=0.50),
      SP75 = quantile(BMI,probs=0.75),
      SP90 = quantile(BMI,probs=0.90),
      SP97 = quantile(BMI,probs=0.97)
    )%>%
    ungroup()%>%
    as.data.frame()%>%
    left_join(ped0 %>% filter(VAR=="BMI"))
  
  ### CALCULATE SUM OF SQUARES ####
  
  eq_sos <- summ %>%
    mutate(
      SOS1 = (SP03-P3)^2,
      SOS2 = (SP10-P10)^2,
      SOS3 = (SP25-P25)^2,
      SOS4 = (SP50-P50)^2,
      SOS5 = (SP75-P75)^2,
      SOS6 = (SP90-P90)^2,
      SOS7 = (SP97-P97)^2
    )%>%
    select(starts_with("SOS"))
  
  sos <- sum(eq_sos)
  
  ### RETURN ####
  
  # return(sos)
}

## LOOP REPLICATIONS ####

set.seed(masterseed)
rep_seed <- sample(1:1E6, nreps)

htwt_cor_listing_all <- data.frame()

for(iter_est in 1:nreps){
  print(iter_est)
  ## READY ####
  
  demo_all <- data.frame(ID=0)
  
  set.seed(rep_seed[iter_est])
  seedl <- sample(1:1E6, 10000)
  iter <- 1
  
  ## SIM AGE WHEN FIXED N PER AGE BUCKET ####
  
  for(age_iter in 25:239){ #ZZZ limits of 2 to 20 yr
    ## SIM AGE ####
    
    demo <- demo0
    demo$AGEMO <- age_iter
    demo <- demo %>% mutate(AGE = round(AGEMO/12,3))
    
    demo_all <- demo_all%>%
      bind_rows(demo %>% mutate(ID = ID+max(demo_all$ID)))%>%
      filter(ID > 0)
    
    rm(demo)
    
    ## END ####
  }
  rm(age_iter)
  
  ## OPTIMIZE HTWT CORRELATION ACCORDING TO BMI-for-AGE by SEXF ####
  
  agebin <- seq(24,240,12) #ZZZ note optimizing ht wt correlation by every 1-yr age bucket (2 to 20 years) and by sexf
  agebin[1] <- 25
  agegrp <- sapply(1:(length(agebin)-1),function(i){paste0("[",agebin[i],",",agebin[i+1],")")})
  
  for(age_iter in 1:(length(agebin)-1)){
    for(sexf_iter in 0:1){
      
      tmpdemo <- demo_all %>%
        filter(AGEMO >= agebin[age_iter])%>%
        filter(AGEMO < agebin[age_iter+1])%>%
        filter(SEXF == sexf_iter)
      
      fit <- optimize(f = fun_optim_2to20yr_mvn, interval = c(0,1), demo0 = tmpdemo, ped0 = ped0, htwt0 = htwt0, seed = seedl[iter], cor = T)
      
      cor_out <- fit$minimum
      
      iter <- iter+1
      
      htwt_cor_listing <- data.frame(ITER = iter_est, SEXF = sexf_iter, AGEGRP = agegrp[age_iter], HTWT_COR = cor_out)
      
      htwt_cor_listing_all <- htwt_cor_listing_all %>%
        bind_rows(htwt_cor_listing)
      
      rm(tmpdemo,cor_out,fit,htwt_cor_listing)
    }
    rm(sexf_iter)
  }
  rm(age_iter,demo_all)
  
  ## END ####
}
rm(rep_seed,iter_est,iter,seedl,demo0,agebin,agegrp)

## WRITE ####

cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates <- htwt_cor_listing_all

write.csv(cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates, "data-raw/cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates.csv")
usethis::use_data(cdc_ages2to20yr_correlations_by_sex_htcm_wtkg_allreplicates, overwrite = TRUE)

## END ####
