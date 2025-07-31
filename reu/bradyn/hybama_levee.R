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
#Loading Datasets
HYBAMA_data<- read_csv("data/processed/Hybognathus_amarus_processed_human_readable_2025.07.06.csv", 
                       col_types = cols(Sex = col_character()))
HYBAMA_data$Sex <- ifelse(HYBAMA_data$Sex == TRUE, "M",
                  ifelse(HYBAMA_data$Sex == FALSE, "F", NA))

GAMAFF_data<- read_csv("data/processed/Gambusia_affinis_processed_human_readable_2025.07.21.csv")
PIMPRO_data<- read_csv("data/PIMPRO Data_2025.07.28.csv")
fish_data<- bind_rows(HYBAMA_data,GAMAFF_data,PIMPRO_data)
levee_data<- read.csv("reu/bradyn/al_midrio_R_data.csv")
#Setting Cochiti dam bounds and dates
cochiti_dam<-fish_data %>% 
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
    Latitude < 35.2268848801106 & Latitude>=35.2 ~"below",
    TRUE ~ "no_intervention"
  ))
#Setting Sandoval Levee construction dates
before_ab_sandoval_levee<-ab_sandoval_levee %>% 
  mutate(before_after_sandoval=case_when(
    YearCollected>=1975~"after",
    YearCollected>=1965 & YearCollected<1975~"during",
    YearCollected<1965 ~"before",
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

#Summing parasite counts
before_ab_amrg_elevee$parasite_sum <- rowSums(before_ab_amrg_elevee[, c("cope.lern", "cope.imler","mono.dact","mono.gyro","myxo.b","nem.cl","nem.unk","trem.b","trem.d","trem.diplo","trem.dlum","trem.em","trem.fim","trem.gold","trem.l","trem.meta.unk","trem.ridge","crus.d","crus.lersp","mono.salsp","mono.ss","trem.dips","trem.iz","trem.unk","nem.larv","nem.l","nem.myst","acanth.spk","cest.botsp","myxo.myxid","myxo.g","CRUS.LERCYP.SKIN","TREM.N.BODY_CAVITY","MONO.GYRSP.PELVICFIN","CRUS.FIN.DORSALFIN","MONO.GYRSP.DORSALFIN","TREM.DIPHUR.LIVER","TREM.NEASP.EYE","TREM.DIPHUR.CONNECTIVE_TISSUE","TREM.P.CONNECTIVE_TISSUE","TREM.DIPS.CONNECTIVETISSUE","TREM.CENT.INTESTINE","TREM.DIPS.INTESTINE","NEM.CAP.INTESTINE","CEST.B.INTESTINE","NEM.CONT.INTESTINE","TREM.BUC.INTESTINE","NEM.UNK.INTESTINE","NEM.LARV.INTESTINE","PROT.MYXSP.GILL(NUMBER_OF_CLUSTERS)","MYXO.M.GILL","MONO.DACSP.GILL","MONO.GYRSP.GILL","NEM.LARV.GALL BLADDER","TREM.CENT.FLUSH","TREM.DIPHUR.FLUSH","TREM.P.FLUSH")], na.rm=TRUE)
#renaming data
BACI_levee<-before_ab_amrg_elevee
rename(BACI_levee, sample_id = `...1`)

#NEW CORRALES DATA
corrales_BACI<-BACI_levee
corrales_BACI<-corrales_BACI %>% 
  filter(
    !corrales_locale%in%c("no_intervention")
  ) %>% 
  filter(
    !before_after_corrales%in%c("no_intervention")
  )

corrales_BACI$before_after_corrales<-factor(corrales_BACI$before_after_corrales,levels=
                                           c("before","during","after")
)

corrales_BACI$corrales_locale<-factor(corrales_BACI$corrales_locale,levels=
                                     c("above","within","below")
)

corrales_BACI %>%
  group_by(corrales_locale,before_after_corrales) %>% 
  summarise(
    avg_parasites = mean(parasite_sum, na.rm = TRUE),
    sd_parasites = sd(parasite_sum, na.rm = TRUE),
    se_parasites = sd(parasite_sum) / sqrt(n()),
    sample_id = n(),
  )
corrales_BACI %>%
  group_by(corrales_locale, before_after_corrales) %>%
  summarise(mean_parasites = mean(parasite_sum), .groups = "drop") %>%
  ggplot(aes(x = before_after_corrales, y = mean_parasites, color = corrales_locale)) +
  geom_point(size = 3) +
  geom_line(aes(group = corrales_locale)) +
  labs(
    title = "Average Parasite Load per Fish in Corrales",
    x = "Time Period",
    y = "Average Parasite Count"
  )


model<-glmmTMB(
  parasite_sum~ corrales_locale*before_after_corrales,
  family = nbinom2(),
  data= corrales_BACI,
)

summary<- corrales_BACI %>% 
  group_by(corrales_locale,before_after_corrales) %>% 
  summarize(total=n())

cmodelOutput<-simulateResiduals(fittedModel = model, plot = TRUE)
summary(model)
plot(parameters(model))

predict_1 <- ggpredict(
  model,
  terms = c("corrales_locale", "before_after_corrales"),
)
predict_plot<-ggplot(data = predict_1, aes(x = x, y = predicted, group = group))+
  facet_wrap(~group)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, position = position_dodge(width = 0.5)) + 
  geom_line(color = "#E30B5D") + 
  geom_point(size = 5, pch=21, position=position_dodge(width=0.5), fill = "#E30B5D", color = "#E30B5D") + 
  labs(x = element_blank(), y = "Predicted Parasite Sum")+ 
  theme(panel.background = element_blank())


#NEW EAST LEVEE DATA
e_levee_BACI<-BACI_levee
e_levee_BACI<-e_levee_BACI %>% 
  filter(
    !e_amrg_locale%in%c("no_intervention")
  ) %>% 
  filter(
    !before_after_eamrg%in%c("no_intervention")
  )

e_levee_BACI$before_after_eamrg<-factor(e_levee_BACI$before_after_eamrg,levels=
                                        c("before","during","after")
)

e_levee_BACI$e_amrg_locale<-factor(e_levee_BACI$e_amrg_locale,levels=
                                   c("above","within","below")
)
e_levee_BACI %>%
  group_by(e_amrg_locale,before_after_eamrg) %>% 
  summarise(
    avg_parasites = mean(parasite_sum, na.rm = TRUE),
    sd_parasites = sd(parasite_sum, na.rm = TRUE),
    se_parasites = sd(parasite_sum) / sqrt(n()),
    sample_id = n(),
  )
e_levee_BACI %>%
  group_by(e_amrg_locale, before_after_eamrg) %>%
  summarise(mean_parasites = mean(parasite_sum), .groups = "drop") %>%
  ggplot(aes(x = before_after_eamrg, y = mean_parasites, color = e_amrg_locale)) +
  geom_point(size = 3) +
  geom_line(aes(group = e_amrg_locale)) +
  labs(
    title = "Average Parasite Load per Fish in East Levee",
    x = "Time Period",
    y = "Average Parasite Count"
  )


E_model<-glmmTMB(
  parasite_sum~ e_amrg_locale*before_after_eamrg,
  family = nbinom2(),
  data= e_levee_BACI,
  ziformula=~1
)

summary<- e_levee_BACI %>% 
  group_by(e_amrg_locale,before_after_eamrg) %>% 
  summarize(total=n())

emodelOutput<-simulateResiduals(fittedModel = E_model, plot = TRUE)
summary(E_model)
plot(parameters(E_model))

predict_2 <- ggpredict(
  E_model,
  terms = c("e_amrg_locale", "before_after_eamrg"),
)
predict_2<-ggplot(data = predict_2, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, position = position_dodge(width = 0.5)) + 
  geom_line(color = "orange") + 
  geom_point(size = 5, pch=21, position=position_dodge(width=0.5), fill = "orange", color = "orange") + 
  labs(x = element_blank(), y = "Predicted Parasite Sum")+ 
  theme(panel.background = element_blank())


#NEW WEST LEVEE DATA
w_levee_BACI<-BACI_levee
w_levee_BACI<-w_levee_BACI %>% 
  filter(
    !w_amrg_locale%in%c("no_intervention")
  ) %>% 
  filter(
    !before_after_wamrg%in%c("no_intervention")
  )

w_levee_BACI$before_after_wamrg<-factor(w_levee_BACI$before_after_wamrg,levels=
                                          c("before","during","after")
)

w_levee_BACI$w_amrg_locale<-factor(w_levee_BACI$w_amrg_locale,levels=
                                     c("above","within","below")
)
w_levee_BACI %>%
  group_by(w_amrg_locale,before_after_wamrg) %>% 
  summarise(
    avg_parasites = mean(parasite_sum, na.rm = TRUE),
    sd_parasites = sd(parasite_sum, na.rm = TRUE),
    se_parasites = sd(parasite_sum) / sqrt(n()),
    sample_id = n(),
  )
w_levee_BACI %>%
  group_by(w_amrg_locale, before_after_wamrg) %>%
  summarise(mean_parasites = mean(parasite_sum), .groups = "drop") %>%
  ggplot(aes(x = before_after_wamrg, y = mean_parasites, color = w_amrg_locale)) +
  geom_point(size = 3) +
  geom_line(aes(group = w_amrg_locale)) +
  labs(
    title = "Average Parasite Load per Fish in West Levee",
    x = "Time Period",
    y = "Average Parasite Count"
  )


W_model<-glmmTMB(
  parasite_sum~ w_amrg_locale*before_after_wamrg+(1|YearCollected),
  family = nbinom2(),
  data= w_levee_BACI,
  ziformula=~1
)

summary<- w_levee_BACI %>% 
  group_by(w_amrg_locale,before_after_wamrg) %>% 
  summarize(total=n())

wmodelOutput<-simulateResiduals(fittedModel = W_model, plot = TRUE)
summary(W_model)
plot(parameters(W_model))

predict_3 <- ggpredict(
  W_model,
  terms = c("w_amrg_locale", "before_after_wamrg"),
  bias_correction = TRUE,
)
predict_3<-ggplot(data = predict_3, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, position = position_dodge(width = 0.5)) + 
  geom_line(color = "orange") + 
  geom_point(size = 5, pch=21, position=position_dodge(width=0.5), fill = "orange", color = "orange") + 
  labs(x = element_blank(), y = "Predicted Parasite Sum")+ 
  theme(panel.background = element_blank())


#NEW SANDOVAL DATA
sandoval_BACI<-BACI_levee
sandoval_BACI<-sandoval_BACI %>% 
  filter(
    !sandoval_locale%in%c("no_intervention")
  ) %>% 
  filter(
    !before_after_sandoval%in%c("no_intervention")
  )

sandoval_BACI$before_after_sandoval<-factor(sandoval_BACI$before_after_sandoval,levels=
                                          c("before","during","after")
)

sandoval_BACI$sandoval_locale<-factor(sandoval_BACI$sandoval_locale,levels=
                                     c("above","within","below")
)
sandoval_BACI %>%
  group_by(sandoval_locale,before_after_sandoval) %>% 
  summarise(
    avg_parasites = mean(parasite_sum, na.rm = TRUE),
    sd_parasites = sd(parasite_sum, na.rm = TRUE),
    se_parasites = sd(parasite_sum) / sqrt(n()),
    sample_id = n(),
  )
sandoval_BACI %>%
  group_by(sandoval_locale, before_after_sandoval) %>%
  summarise(mean_parasites = mean(parasite_sum), .groups = "drop") %>%
  ggplot(aes(x = before_after_sandoval, y = mean_parasites, color = sandoval_locale)) +
  geom_point(size = 3) +
  geom_line(aes(group = sandoval_locale)) +
  labs(
    title = "Average Parasite Load per Fish in Sandoval Levee",
    x = "Time Period",
    y = "Average Parasite Count"
  )


S_model<-glmmTMB(
  parasite_sum~ sandoval_locale*before_after_sandoval+(1|CatalogNumber),
  family = nbinom2(),
  data= sandoval_BACI,
  ziformula=~1
)

summary<- sandoval_BACI %>% 
  group_by(sandoval_locale,before_after_sandoval) %>% 
  summarize(total=n())

smodelOutput<-simulateResiduals(fittedModel = S_model, plot = TRUE)
summary(S_model)
plot(parameters(S_model))

predict_4 <- ggpredict(
  S_model,
  terms = c("sandoval_locale", "before_after_sandoval"),
  bias_correction = TRUE,
)
predict_4<-ggplot(data = predict_4, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, position = position_dodge(width = 0.5)) + 
  geom_line(color = "green") + 
  geom_point(size = 5, pch=21, position=position_dodge(width=0.5), fill = "green", color = "green") + 
  labs(x = element_blank(), y = "Predicted Parasite Sum")+ 
  theme(panel.background = element_blank())


# the emmeans measures whether your highest order relationships are significant or not (interaction itself, not the factors of the interaction)
emm <- emmeans(S_model, ~ sandoval_locale*before_after_sandoval)
joint_tests(S_model) # idk what to make of this output at the moment












