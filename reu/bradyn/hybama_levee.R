library(datasets)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

HYBAMA_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/ABQ_DATA/IND_PROJ_BRADYN/data/processed/Hybognathus_amarus_processed_human_readable_2025.07.06.csv")
levee_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/al_midrio_R_data.csv")
#Setting Angostura and Cochiti dam bounds
#setting Corrales Levee bounds

# connor load datasets 
HYBAMA_data<- read.csv("data/processed/Hybognathus_amarus_processed_human_readable_2025.07.06.csv")
levee_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/al_midrio_R_data.csv")

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
#Summing parasite counts
BACI_levee$parasite_sum <- rowSums(BACI_levee[, c("cope.lern", "cope.imler","mono.dact","mono.gyro","myxo.b","nem.cl","nem.unk","trem.b","trem.d","trem.diplo","trem.dlum","trem.em","trem.fim","trem.gold","trem.l","trem.meta.unk","trem.ridge")], na.rm=TRUE)

#Filter for above Corrales
above_corrales_levee<-BACI_levee[tolower(BACI_levee$corrales_locale)=="corrales_levee_above",]

#filter for above and before corrales
above_corrales_blevee<-above_corrales_levee[tolower(above_corrales_levee$before_after_corrales)=="before_corrales",]
view(above_corrales_blevee)

#filter for above and after corrales
above_corrales_alevee<-above_corrales_levee[tolower(above_corrales_levee$before_after_corrales)=="after_corrales",]

#plots for Above corrales
plot(above_corrales_levee$parasite_sum~above_corrales_levee$YearCollected) # instead of looking at sums of counts we want to see the average occurance at each setting (see code below)

averages <- above_corrales_levee %>% 
  group_by(XXXwhatever you want to knowXXX) %>% 
  summarize(observations = n(), .groups = "drop")

summary(lm(above_corrales_levee$parasite_sum~above_corrales_levee$YearCollected))


#filter for below and in corrales

below_corrales_levee<-BACI_levee[tolower(BACI_levee$corrales_locale)=="corrales_levee_below",]

view(below_corrales_levee)

#filter for below corrales and above west middle rio grande
true_below_corrales<-below_corrales_levee[tolower(below_corrales_levee$w_amrg_locale)=="amrg_wlevee_above",]

#Filter for above west middle rio grande

above_wamrg_levees<-BACI_levee[tolower(BACI_levee$w_amrg_locale)=="amrg_wlevee_above",]

#filter for above and before west middle rio grande
above_wamrg_blevees<-above_wamrg_levees[tolower(above_wamrg_levees$before_after_wamrg)=="before_w_mrg",]

#filter for above and after west middle rio grande
above_wamrg_alevees<-above_wamrg_levees[tolower(above_wamrg_levees$before_after_wamrg)=="after_w_mrg",]
view(above_wamrg_alevees)
#filter for in and below west middle rio grande

view(filtered_wamrg_alevees)
view(filtered_wamrg_balevees)


plot(BACI_levee$parasite_sum~BACI_levee$YearCollected)
view(BACI_levee$corrales_locale)

character_counts<-BACI_levee %>% count(before_after_wamrg)

year_counts<-BACI_levee %>% count(YearCollected)

print(year_counts)

print(character_counts)

view(character_counts)  
view(year_counts)

hist(character_counts$n)
plot(character_counts$n~year_counts$YearCollected)


