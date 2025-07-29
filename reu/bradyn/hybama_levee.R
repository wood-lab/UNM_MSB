library(datasets)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(parameters)
library(DHARMa)
library(emmeans)
library(ggeffects)

HYBAMA_data<- read_csv("data/processed/Hybognathus_amarus_processed_human_readable_2025.07.06.csv", 
                       col_types = cols(Sex = col_character()))
HYBAMA_data$Sex <- ifelse(HYBAMA_data$Sex == TRUE, "M",
                  ifelse(HYBAMA_data$Sex == FALSE, "F", NA))

GAMAFF_data<- read_csv("data/processed/Gambusia_affinis_processed_human_readable_2025.07.21.csv")
two_fish_data<- bind_rows(HYBAMA_data,GAMAFF_data)
view(two_fish_data)
levee_data<- read.csv("reu/bradyn/al_midrio_R_data.csv")
#Setting Cochiti dam bounds and dates
cochiti_dam<-two_fish_data %>% 
  mutate(dam_locale = case_when(
    Latitude >= 35.2 & Latitude <=35.6481 ~"cochiti_bound",
    TRUE ~ "no_intervention"
  ))

BACI_dam<-cochiti_dam %>% 
  mutate(dam_CI=case_when(
    YearCollected>=1975~"impact",
    YearCollected<1975~"control",
    TRUE~"no_intervention"
  ))
#setting Corrales Levee bounds
ab_corrales_levee <- BACI_dam %>% 
  mutate(corrales_locale = case_when(
    Latitude > 35.28136319711 ~"above",
    Latitude >= 35.1609175651113 & Latitude <= 35.28136319711 ~ "within",
    Latitude < 35.1609175651113 ~ "below",
    TRUE ~ "no_intervention"
  ))

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
    YearCollected>=1975~"after",
    YearCollected<1975 ~"before",
    TRUE ~ "no_intervention"
  ))
#Setting Alb. Middle Rio Grande East Levee System One and Two bounds
ab_amrg_elevee<-before_ab_sandoval_levee %>% 
  mutate(e_amrg_locale = case_when(
    Latitude > 35.22783185411061 ~ "above",
    Latitude >= 35.00309478973807 & Latitude <= 35.22783185411061 ~ "within",
    Latitude < 35.00309478973807 ~ "below",
    TRUE ~ "no_intervention"
  ))
#Setting Alb. Middle Rio Grande East Levee System One and Two construction dates
before_ab_amrg_elevee <- ab_amrg_elevee %>% 
  mutate(before_after_eamrg=case_when(
    YearCollected>=1969 ~"after",
    YearCollected>=1951 & YearCollected <1969 ~"during",
    YearCollected<1951 ~"before",
    TRUE ~ "no_intervention"
  ))

#BACI_levee Corrales
BACI_levee<-before_ab_amrg_elevee
BACI_levee<-BACI_levee %>% 
  filter(
    !corrales_locale%in%c("no_intervention")
  ) %>% 
  filter(
    !before_after_corrales%in%c("no_intervention")
  )

BACI_levee$before_after_corrales<-factor(BACI_levee$before_after_corrales,levels=
           c("before","during","after")
         )

BACI_levee$corrales_locale<-factor(BACI_levee$corrales_locale,levels=
                                           c("above","within","below")
)
#BACI_levee East Levee
BACI_levee<-BACI_levee %>% 
  filter(
    !e_amrg_locale%in%c("no_intervention")
  ) %>% 
  filter(
    !before_after_eamrg%in%c("no_intervention")
  )

BACI_levee$before_after_eamrg<-factor(BACI_levee$before_after_eamrg,levels=
                                           c("before","during","after")
)

BACI_levee$e_amrg_locale<-factor(BACI_levee$e_amrg_locale,levels=
                                     c("above","within","below")
)
#BACI_levee West Levee
BACI_levee<-BACI_levee %>% 
  filter(
    !w_amrg_locale%in%c("no_intervention")
  ) %>% 
  filter(
    !before_after_wamrg%in%c("no_intervention")
  )

BACI_levee$before_after_wamrg<-factor(BACI_levee$before_after_wamrg, levels=
                                        c("before","during","after")
)
BACI_levee$w_amrg_locale<-factor(BACI_levee$w_amrg_locale,levels=
                                   c("above","within","below")
)
#BACI_levee Sandoval Levee
BACI_levee<-BACI_levee %>% 
  filter(
    !sandoval_locale%in%c("no_intervention")
  ) %>% 
  filter(
    !before_after_sandoval%in%c("no_intervention")
  )

BACI_levee$before_after_sandoval<-factor(BACI_levee$before_after_sandoval,levels=
                                           c("before","after")
)

BACI_levee$sandoval_locale<-factor(BACI_levee$sandoval_locale,levels=
                                     c("above","within","below")
)
#BACI_Cochiti
BACI_levee<-BACI_levee %>% 
  filter(
    !dam_locale%in%c("no_intervention")
  ) %>% 
  filter(
    !dam_CI%in%c("no_intervention")
  )
BACI_levee$dam_CI<-factor(BACI_levee$dam_CI,levels=
                                           c("control","impact")
)

BACI_levee$dam_locale<-factor(BACI_levee$sandoval_locale,levels=
                                     c("cochiti_bound")
)
#Summing parasite counts
BACI_levee$parasite_sum <- rowSums(BACI_levee[, c("cope.lern", "cope.imler","mono.dact","mono.gyro","myxo.b","nem.cl","nem.unk","trem.b","trem.d","trem.diplo","trem.dlum","trem.em","trem.fim","trem.gold","trem.l","trem.meta.unk","trem.ridge","crus.d","crus.lersp","mono.salsp","mono.ss","trem.dips","trem.iz","trem.unk","nem.larv","nem.l","nem.myst","acanth.spk","cest.botsp","myxo.myxid","myxo.g")], na.rm=TRUE)


# instead of looking at sums of counts we want to see the average occurance at each setting (see code below)
averages <- BACI_levee %>% 
  group_by(dam_locale,dam_CI) %>% 
  summarize(avg.bin= mean(parasite_sum))
#Attempting to model data IGNORE ALL I HAVE NO IDEA WHAT IM DOING
cochiti_filter <- BACI_levee %>%
  filter(dam_locale %in% c("cochiti_bound"))
view(cochiti_filter)
xtabs(~ dam_locale + dam_CI, data = cochiti_filter)
#corrales_model
model<-glmmTMB(
  parasite_sum~ corrales_locale*before_after_corrales,
  family = nbinom2(),
  data= BACI_levee, 
  ziformula = ~1
  )
#corrales model plot
predict_1 <- ggpredict(
  model,
  terms = c("corrales_locale", "before_after_corrales"),
  type = "zero_inflated",
)
predict_plot<-ggplot(data = predict_1, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.05,
                position = position_dodge(width = 0.5)) +
  geom_line(color = "steelblue") +
  geom_point(size = 5, pch=21, 
             position=position_dodge(width=0.5),
             fill = "white", color = "steelblue") +
  labs(x = "Before/After Corrales", y = "Predicted Parasite Sum")

#Alb. Middle Rio Grande East Levee Model
E_model<-glmmTMB(
  parasite_sum~e_amrg_locale*before_after_eamrg,
  family=nbinom2(),
  data=BACI_levee,
  ziformula = ~1
)
E_summary<- BACI_levee %>% 
  group_by(e_amrg_locale,before_after_eamrg) %>% 
  summarize(total=n())
#Alb. Middle Rio Grande East Levee Model
predict_2 <- ggpredict(
  E_model,
  terms = c("e_amrg_locale", "before_after_eamrg"),
  type = "zero_inflated"
)
predict_plot2<-ggplot(data = predict_2, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.05,
                position = position_dodge(width = 0.5)) +
  geom_line(color = "steelblue") +
  geom_point(size = 5, pch=21, 
             position=position_dodge(width=0.5),
             fill = "white", color = "steelblue") +
  labs(x = "Before/After East MRG", y = "Predicted Parasite Sum")
#Alb. Middle Rio Grande West levee model
W_model<-glmmTMB(
  parasite_sum~w_amrg_locale*before_after_wamrg,
  family=nbinom2(),
  data=BACI_levee,
  ziformula=~1
)
W_summary<- BACI_levee %>% 
  group_by(w_amrg_locale,before_after_wamrg) %>% 
  summarize(total=n())
view(BACI_levee)
#Alb. Middle Rio Grande West levee plot
predict_3 <- ggpredict(
  W_model,
  terms = c("w_amrg_locale", "before_after_wamrg"),
)

predict_plot3<-ggplot(data = predict_3, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.05,
                position = position_dodge(width = 0.5)) +
  geom_line(color = "steelblue") +
  geom_point(size = 5, pch=21, 
             position=position_dodge(width=0.5),
             fill = "white", color = "steelblue") +
  labs(x = "Before/After West MRG", y = "Predicted Parasite Sum")
#Sandoval Model
S_model<-glmmTMB(
  parasite_sum~ sandoval_locale*before_after_sandoval+(1|CatalogNumber),
  family = nbinom2(),
  data= BACI_levee, 
  ziformula = ~1
)
S_summary<- BACI_levee %>% 
  group_by(sandoval_locale,before_after_sandoval) %>% 
  summarize(total=n())
#Sandoval Plot
predict_4 <- ggpredict(
  S_model,
  terms = c("sandoval_locale", "before_after_sandoval"),
)

predict_plot4<-ggplot(data = predict_4, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.05,
                position = position_dodge(width = 0.5)) +
  geom_line(color = "steelblue") +
  geom_point(size = 5, pch=21, 
             position=position_dodge(width=0.5),
             fill = "white", color = "steelblue") +
  labs(x = "Before/After Sandoval Renovation", y = "Predicted Parasite Sum")
#Cochiti model
C_model<-glmmTMB(
  parasite_sum~ dam_locale*dam_CI,
  family = nbinom2(),
  data= BACI_levee, 
  ziformula = ~1
)
S_summary<- BACI_levee %>% 
  group_by(sandoval_locale,before_after_sandoval) %>% 
  summarize(total=n())
# here are all the model diagnostics and outputs:
simulationOutput<-simulateResiduals(fittedModel = S_model, plot = TRUE)
testZeroInflation(simulationOutput)
# if the model was a good fit, the QQ plot would have points hugging the red line, and the carPred plot on the right would be just a scattering of random points -- so this model is a bad fit 
summary(S_model) # the NAs mean it is rank deficient -- there arent enough observations in those categories to draw comparisons (which is to be expected bc we are only working with one species, very few fish)
plot(parameters(S_model)) # if the bars do NOT pass over 0 it is a significant result (red is neg, blue is positive) -- just for visualization

# the emmeans measures whether your highest order relationships are significant or not (interaction itself, not the factors of the interaction)
emm <- emmeans(model, ~ corrales_locale*before_after_corrales)
joint_tests(model) # idk what to make of this output at the moment










view(averages)
view(BACI_levee)
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



