library(datasets)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

HYBAMA_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/ABQ_DATA/IND_PROJ_BRADYN/data/processed/Hybognathus_amarus_processed_human_readable_2025.07.06.csv")
levee_data<- read.csv("C:/Users/Bradyn/OneDrive/GEO366/al_midrio_R_data.csv")
view(HYBAMA_data)

#setting Corrales Levee bounds

ab_corrales_levee <- HYBAMA_data %>% 
  mutate(individual_levee_location = case_when(
    Latitude > 35.28136319711 ~"levee_above",
    Latitude >= 35.1609175651113 & Latitude <= 35.28136319711 ~ "corrales_levee",
    Latitude < 35.1609175651113 ~ "levee_below",
    TRUE ~ "no_intervention"
  ))

view(ab_corrales_levee)
#setting Corrales levee construction date
before_ab_corrales_levee <- ab_corrales_levee %>% 
  mutate(before_after_corrales = case_when(
    YearCollected >=1997 ~ "after_corrales",
    YearCollected >=1986 & YearCollected<1997 ~"during_corrales",
    YearCollected < 1986 ~"before_corrales",
    TRUE ~"no_intervention"
  ))
view(before_ab_corrales_levee)


