library(datasets)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

HYBAMA_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/ABQ_DATA/IND_PROJ_BRADYN/data/processed/Hybognathus_amarus_processed_human_readable_2025.07.06.csv")
levee_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/al_midrio_R_data.csv")
#Setting Angostura and Cochiti dam bounds
#setting Corrales Levee bounds

ab_corrales_levee <- HYBAMA_data %>% 
  mutate(corrales_locale = case_when(
    Latitude > 35.28136319711 ~"corrales_levee_above",
    Latitude >= 35.1609175651113 & Latitude <= 35.28136319711 ~ "corrales_levee",
    Latitude < 35.1609175651113 ~ "corrales_levee_below",
    TRUE ~ "no_intervention"
  ))

#setting Corrales levee construction date
before_ab_corrales_levee <- ab_corrales_levee %>% 
  mutate(before_after_corrales = case_when(
    YearCollected >=1997 ~ "after_corrales",
    YearCollected >=1986 & YearCollected<1997 ~"during_corrales",
    YearCollected < 1986 ~"before_corrales",
    TRUE ~"no_intervention"
  ))

#setting Alb. Middle Rio Grande West Levee bounds
ab_amrg_wlevee <- before_ab_corrales_levee %>% 
  mutate(w_amrg_locale = case_when(
    Latitude>35.095806160112076 ~"amrg_wlevee_above",
    Latitude>= 34.94913162611374 & Latitude <=35.095806160112076 ~ "amrg_wlevee",
    Latitude < 34.94913162611374 ~"amrg_wlevee_below",
    TRUE ~ "no_intervention"
  ))
#Setting Alb. Middle Rio Grande West Levee construction dates
before_ab_amrg_wlevee<-ab_amrg_wlevee %>% 
  mutate(before_after_wamrg=case_when(
    YearCollected>=1956~"after_w_mrg",
    YearCollected>=1951 & YearCollected <1956 ~"during_w_mrg",
    YearCollected<1951 ~"before_w_mrg",
    TRUE ~ "no_intervention"
  ))
#renaming data
BACI_levee<-before_ab_amrg_wlevee

BACI_levee$parasite_sum <- rowSums(BACI_levee[, c("cope.lern", "cope.imler","mono.dact","mono.gyro","myxo.b","nem.cl","nem.unk","trem.b","trem.d","trem.diplo","trem.dlum","trem.em","trem.fim","trem.gold","trem.l","trem.meta.unk","trem.ridge")], na.rm=TRUE)


view(BACI_levee$parasite_sum)



character_counts<-BACI_levee %>% count(before_after_wamrg)
year_counts<-BACI_levee %>% count(YearCollected)

print(year_counts)
print(character_counts)

view(character_counts)  
view(year_counts)

hist(character_counts$n)
plot(character_counts$n~year_counts$YearCollected)


