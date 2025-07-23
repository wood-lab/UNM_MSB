library(datasets)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(glmmTMB)

HYBAMA_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/ABQ_DATA/IND_PROJ_BRADYN/data/processed/Hybognathus_amarus_processed_human_readable_2025.07.06.csv")
levee_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/al_midrio_R_data.csv")
#Setting Angostura and Cochiti dam bounds

#setting Corrales Levee bounds
# connor load datasets 
HYBAMA_data<- read.csv("data/processed/Hybognathus_amarus_processed_human_readable_2025.07.06.csv")
levee_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/al_midrio_R_data.csv")

ab_corrales_levee <- HYBAMA_data %>% 
  mutate(corrales_locale = case_when(
    Latitude > 35.28136319711 ~"above",
    Latitude >= 35.1609175651113 & Latitude <= 35.28136319711 ~ "within",
    Latitude < 35.1609175651113 ~ "below",
    TRUE ~ "no_intervention"
  ))
#Dam system Locations and Dates

#setting Corrales levee construction date
before_ab_corrales_levee <- ab_corrales_levee %>% 
  mutate(before_after_corrales = case_when(
    YearCollected >=1997 ~ "after",
    YearCollected >=1986 & YearCollected<1997 ~"during",
    YearCollected < 1986 ~"before",
    TRUE ~"no_intervention"
  ))

#setting Alb. Middle Rio Grande West Levee bounds
ab_amrg_wlevee <- before_ab_corrales_levee %>% 
  mutate(w_amrg_locale = case_when(
    Latitude>35.095806160112076 ~"above",
    Latitude>= 34.94913162611374 & Latitude <=35.095806160112076 ~ "within",
    Latitude < 34.94913162611374 ~"below",
    TRUE ~ "no_intervention"
  ))
#Setting Alb. Middle Rio Grande West Levee construction dates
before_ab_amrg_wlevee<-ab_amrg_wlevee %>% 
  mutate(before_after_wamrg=case_when(
    YearCollected>=1956~"after",
    YearCollected>=1951 & YearCollected <1956 ~"during",
    YearCollected<1951 ~"before",
    TRUE ~ "no_intervention"
  ))
#renaming data
few_levee<-before_ab_amrg_wlevee
#Setting Sandoval Levee bounds
ab_sandoval_levee<-few_levee %>% 
  mutate(sandoval_locale = case_when(
    Latitude > 35.376442340109 ~"above",
    Latitude>= 35.2268848801106 & Latitude <= 35.376442340109 ~ "within",
    Latitude < 35.2268848801106 ~"below",
    TRUE ~ "no_intervention"
  ))
#Setting Sandoval Levee construction dates
before_ab_sandoval_levee<-ab_sandoval_levee %>% 
  mutate(before_after_sandoval=case_when(
    YearCollected>=1935~"after",
    YearCollected>=1930 & YearCollected <1935 ~"during",
    YearCollected<1930 ~"before",
    TRUE ~ "no_intervention"
  ))
#Setting Alb. Middle Rio Grande East Levee System One and Two
ab_amrg_elevee<-before_ab_sandoval_levee %>% 
  mutate(e_amrg_locale = case_when(
    Latitude > 35.22783185411061 ~ "above",
    Latitude >= 35.00309478973807 & Latitude <= 35.22783185411061 ~ "within",
    Latitude < 35.00309478973807 ~ "below",
    TRUE ~ "no_intervention"
  ))
view(ab_amrg_elevee)
#BACI_levee
BACI_levee<-ab_sandoval_levee
view(BACI_levee)
#Summing parasite counts
BACI_levee$parasite_sum <- rowSums(BACI_levee[, c("cope.lern", "cope.imler","mono.dact","mono.gyro","myxo.b","nem.cl","nem.unk","trem.b","trem.d","trem.diplo","trem.dlum","trem.em","trem.fim","trem.gold","trem.l","trem.meta.unk","trem.ridge")], na.rm=TRUE)

#Filter for above Corrales
above_corrales_levee<-BACI_levee[tolower(BACI_levee$corrales_locale)=="above",]

#filter for above and before corrales
above_corrales_blevee<-above_corrales_levee[tolower(above_corrales_levee$before_after_corrales)=="before",]


#filter for above and after corrales
above_corrales_alevee<-above_corrales_levee[tolower(above_corrales_levee$before_after_corrales)=="after",]

#plots for Above corrales
plot(above_corrales_levee$parasite_sum~above_corrales_levee$YearCollected) # instead of looking at sums of counts we want to see the average occurance at each setting (see code below)
view(HYBAMA_data)
averages <- HYBAMA_data %>% 
  group_by(corrales_locale,before_after_corrales) %>% 
  summarize(avg.bin= mean(parasite_sum))


summary(lm(above_corrales_levee$parasite_sum~above_corrales_levee$YearCollected))
plot(above_corrales_levee$parasite_sum~above_corrales_levee$YearCollected,
     main = "Number of Parasites Above Corrales Levee Over time",
     xlab = "Year Collected",
     ylab = "Number of Parasites",
     pch = 19,
     col = "purple")
regression_above <-lm(parasite_sum ~ YearCollected,data=above_corrales_levee )
abline(regression, col="red", lwd=2)

summary(regression_above)

#filter for below and in corrales

below_corrales_levee<-BACI_levee[
  tolower(BACI_levee$parasite_sum)%in%c("below","within"),
  ]
#must be above west middle rio grande
true_below_corrales<-below_corrales_levee[
  tolower(below_corrales_levee$w_amrg_locale)=="above",
]
view(true_below_corrales)

#plotting below and in corrales
plot(true_below_corrales$parasite_sum~true_below_corrales$YearCollected,
     main = "Number of Parasites Within and Below Corrales Levee Over time",
     xlab = "Year Collected",
     ylab = "Number of Parasites",
     pch = 19,
     col = "red")
regression <-lm(parasite_sum ~ YearCollected,data=true_below_corrales )
abline(regression, col="blue", lwd=2)
summary(regression)

summary(lm(below_corrales_levee$parasite_sum~below_corrales_levee$YearCollected))




#Filter for above west middle rio grande

above_wamrg_levees<-BACI_levee[
  tolower(BACI_levee$w_amrg_locale)== "above",
  ]

true_above_wamrg<-above_wamrg_levees[
  tolower(above_wamrg_levees$corrales_locale)=="below",
]
view(true_above_wamrg)
plot(true_above_wamrg$parasite_sum ~ true_above_wamrg$YearCollected)
       
#filter for above and before west middle rio grande
true_above_bwamrg<-true_above_wamrg[
  tolower(true_above_wamrg$before_after_wamrg)=="before",
  ]
plot(true_above_bwamrg$parasite_sum ~ true_above_bwamrg$YearCollected)
view(true_above_bwamrg)

#filter for above and after west middle rio grande
true_above_awamrg<-true_above_wamrg[
  tolower(true_above_wamrg$before_after_wamrg)=="after",
  ]
plot(true_above_awamrg$parasite_sum~true_above_awamrg$YearCollected)

view(above_wamrg_alevees)
#filter for in and below west middle rio grande
below_wamrg_levee<-BACI_levee[
  tolower(BACI_levee$w_amrg_locale)%in%c("below","within"),
]
#Plotting
plot(below_wamrg_levee$parasite_sum ~ below_wamrg_levee$YearCollected,
     main = "Parasite Abundace Within and Below WAMRG",
     xlab = "Year Collected",
     ylab = "Parasite Count",
     pch = 19,
     col = "orange")
wamrg_regression <-lm(parasite_sum ~ YearCollected,data=below_wamrg_levee)
abline(wamrg_regression, col="blue", lwd=2)
summary(wamrg_regression)



