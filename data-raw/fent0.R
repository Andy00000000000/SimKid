## code to prepare `fent0` dataset goes here

# fent0.csv in data-raw and fent0.rda in data was prepared on April 11, 2025

## NOTES ####

# Fenton growth charts for male and female weight were digitized up to 40 weeks (for full-term) from the literature [1-3] using PinPoint Digitizer [4].
# Fitting of weight LMS parameters by age and sex was done in R using the optimize function [5] and the sum of squares statistic between digitized and predicted weight percentiles.

# [1] https://ucalgary.ca/resource/preterm-growth-chart
# [2] Fenton, T.R., Kim, J.H. A systematic review and meta-analysis to revise the Fenton growth chart for preterm infants. BMC Pediatr 13, 59 (2013). https://doi.org/10.1186/1471-2431-13-59
# [3] Fenton, T.R., Nasser, R., Eliasziw, M. et al. Validating the weight gain of preterm infants between the reference growth curve of the fetus and the term infant. BMC Pediatr 13, 92 (2013). https://doi.org/10.1186/1471-2431-13-92
# [4] https://mhismail.github.io/PinPoint-Landing/
# [5] https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/optimize

## READY ENVIRONMENT ####

library(dplyr)
library(ggplot2)

## LOAD DIGITIZED DATA ####

csv0_male <- read.csv("data-raw/fenton-preterm-boys-wtkg-digitized.csv",stringsAsFactors = F)
csv0_female <- read.csv("data-raw/fenton-preterm-girls-wtkg-digitized.csv",stringsAsFactors = F)

csv0 <- csv0_male %>%
  bind_rows(csv0_female)

rm(csv0_male,csv0_female)

## OPTIMIZE LMS PARAMETERS FOR WEIGHT ####

### function for minimization ####

fun_eqs <- function(theta, fixed){
  
  tmpeq <- c()
  
  WTKG1 <- fixed[1]
  WTKG2 <- fixed[2]
  WTKG3 <- fixed[3] # 50th percentile
  WTKG4 <- fixed[4]
  WTKG5 <- fixed[5]
  
  Z1 <- fixed[6]
  Z2 <- fixed[7]
  Z3 <- fixed[8]
  Z4 <- fixed[9]
  Z5 <- fixed[10]
  
  L <- theta[1]
  M <- exp(theta[2])
  S <- exp(theta[3])
  
  tmpeq[1] <- (WTKG1 - M*(1+L*S*Z1)^(1/L))^2 # sum of squares
  tmpeq[2] <- (WTKG2 - M*(1+L*S*Z2)^(1/L))^2
  tmpeq[3] <- (WTKG3 - M*(1+L*S*Z3)^(1/L))^2
  tmpeq[4] <- (WTKG4 - M*(1+L*S*Z4)^(1/L))^2
  tmpeq[5] <- (WTKG5 - M*(1+L*S*Z5)^(1/L))^2
  
  sum <- sum(tmpeq)
  
  return(sum) # optimizing towards minimum
}

### function for validation of wtkg ####

fun_pred <- function(PERC = NA, param = NULL){
  
  Z <- qnorm(PERC/100)
  
  L <- param[1]
  M <- exp(param[2])
  S <- exp(param[3])
  
  WTKG <- M*(1+L*S*Z)^(1/L)
  
  return(WTKG)
}

### loop ####

lms_listing <- csv0 %>%
  distinct(TIME_WK,SEXF)%>%
  rename(GA_WK = TIME_WK)

wtkg_val <- lms_listing

perc <- c(3,10,50,90,97)
z <- qnorm(perc/100)

for(iter in 1:nrow(lms_listing)){
  #### ready ####
  
  wtkg <- c(
    csv0[which(csv0$SEXF == lms_listing[iter,"SEXF"] & csv0$TIME_WK == lms_listing[iter,"GA_WK"] & csv0$PTILE == perc[1]),"WTKG"],
    csv0[which(csv0$SEXF == lms_listing[iter,"SEXF"] & csv0$TIME_WK == lms_listing[iter,"GA_WK"] & csv0$PTILE == perc[2]),"WTKG"],
    csv0[which(csv0$SEXF == lms_listing[iter,"SEXF"] & csv0$TIME_WK == lms_listing[iter,"GA_WK"] & csv0$PTILE == perc[3]),"WTKG"],
    csv0[which(csv0$SEXF == lms_listing[iter,"SEXF"] & csv0$TIME_WK == lms_listing[iter,"GA_WK"] & csv0$PTILE == perc[4]),"WTKG"],
    csv0[which(csv0$SEXF == lms_listing[iter,"SEXF"] & csv0$TIME_WK == lms_listing[iter,"GA_WK"] & csv0$PTILE == perc[5]),"WTKG"]
  )
  
  wtkg_val[iter,paste0("OP",ifelse(perc[1] < 10,"0",""),perc[1])] <- wtkg[1]
  wtkg_val[iter,paste0("OP",ifelse(perc[2] < 10,"0",""),perc[2])] <- wtkg[2]
  wtkg_val[iter,paste0("OP",ifelse(perc[3] < 10,"0",""),perc[3])] <- wtkg[3]
  wtkg_val[iter,paste0("OP",ifelse(perc[4] < 10,"0",""),perc[4])] <- wtkg[4]
  wtkg_val[iter,paste0("OP",ifelse(perc[5] < 10,"0",""),perc[5])] <- wtkg[5]
  
  #### initial estimates ####
  
  theta <- c(0.3,log(wtkg[3]),log(sd(wtkg)))
  fixed <- c(wtkg,z)
  
  #### optimize with sum of squares ####
  
  fit <- optim(par = theta, fn = fun_eqs, fixed = fixed)
  
  lms_listing[iter,"L"] <- fit$par[1]
  lms_listing[iter,"M"] <- exp(fit$par[2])
  lms_listing[iter,"S"] <- exp(fit$par[3])
  
  #### validate predictions ####
  
  wtkg_val[iter,paste0("PP",ifelse(perc[1] < 10,"0",""),perc[1])] <- fun_pred(PERC = perc[1], param = fit$par)
  wtkg_val[iter,paste0("PP",ifelse(perc[2] < 10,"0",""),perc[2])] <- fun_pred(PERC = perc[2], param = fit$par)
  wtkg_val[iter,paste0("PP",ifelse(perc[3] < 10,"0",""),perc[3])] <- fun_pred(PERC = perc[3], param = fit$par)
  wtkg_val[iter,paste0("PP",ifelse(perc[4] < 10,"0",""),perc[4])] <- fun_pred(PERC = perc[4], param = fit$par)
  wtkg_val[iter,paste0("PP",ifelse(perc[5] < 10,"0",""),perc[5])] <- fun_pred(PERC = perc[5], param = fit$par)
  
  rm(wtkg)
  
  #### end ####
}
rm(iter)

## PLOT VALIDATION ####

theme_set(theme_bw())
theme_update(
  plot.caption = element_text(hjust = 0)
)

p1 <- ggplot(wtkg_val %>% filter(SEXF == 0), aes(x = GA_WK))+
  geom_line(aes(y = OP03), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = OP10), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = OP50), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = OP90), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = OP97), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP03), color = "red", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP10), color = "red", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP50), color = "red", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP90), color = "red", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP97), color = "red", alpha = 0.5, linetype = 2)+
  scale_x_continuous(breaks = seq(22,40,2))+
  labs(x = "Gestational Age (week)", y = "Percentiles of Weight", title = "Male Fenton Growth Chart", caption = paste0("Note: ", paste0(perc,collapse = ", ")," percentiles of weight for age\ndigitization of reported (black) and optimized LMS (red)"))

p2 <- ggplot(wtkg_val %>% filter(SEXF == 1), aes(x = GA_WK))+
  geom_line(aes(y = OP03), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = OP10), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = OP50), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = OP90), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = OP97), color = "black", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP03), color = "red", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP10), color = "red", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP50), color = "red", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP90), color = "red", alpha = 0.5, linetype = 2)+
  geom_line(aes(y = PP97), color = "red", alpha = 0.5, linetype = 2)+
  scale_x_continuous(breaks = seq(22,40,2))+
  labs(x = "Gestational Age (week)", y = "Percentiles of Weight", title = "Female Fenton Growth Chart", caption = paste0("Note: ", paste0(perc,collapse = ", ")," percentiles of weight for age\ndigitization of reported (black) and optimized LMS (red)"))

print(p1)
print(p2)

## COMBINE TO FINALIZE ####

fent_wtage <- lms_listing%>%
  dplyr::mutate_if(is.character,as.numeric)%>%
  dplyr::mutate(VAR="WTKG",CHART="FENTON",PNA = 0, AGEMO = GA_WK)

fent_p <- wtkg_val%>%
  dplyr::mutate_if(is.character,as.numeric)%>%
  dplyr::select(GA_WK,SEXF,OP03,OP10,OP50,OP90,OP97)%>%
  dplyr::rename(P3=OP03,P10=OP10,P50=OP50,P90=OP90,P97=OP97)%>%
  dplyr::mutate(P5=NA,P25=NA,P75=NA,P95=NA)

fent0 <- fent_wtage %>%
  dplyr::left_join(fent_p)%>%
  dplyr::group_by(VAR,SEXF)%>%
  dplyr::mutate(LSAGEMO = dplyr::lag(AGEMO))%>%
  dplyr::mutate(NXAGEMO = dplyr::lead(AGEMO))%>%
  dplyr::ungroup()%>%
  as.data.frame()%>%
  dplyr::mutate(LOAGEMO = AGEMO)%>%
  dplyr::mutate(UPAGEMO = ifelse(is.na(NXAGEMO), AGEMO, NXAGEMO))%>%
  dplyr::mutate(AGEGRP = paste0("PNA: 0; GA_WK: [",LOAGEMO,",",UPAGEMO,ifelse(LOAGEMO!=UPAGEMO, ")", "]")))%>%
  dplyr::select(CHART,VAR,SEXF,AGEGRP,L,M,S,P3,P5,P10,P25,P50,P75,P90,P95,P97)

## WRITE ####

write.csv(fent0,"data-raw/fent0.csv", row.names = F, quote = T,na = "")
usethis::use_data(fent0, overwrite = TRUE)

## END ####

