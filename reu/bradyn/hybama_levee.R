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
HYBAMA_data<- read_csv("data/processed/Hybognathus_amarus_processed_machine_readable_2025.07.06.csv")
HYBAMA_data<-HYBAMA_data %>% 
  mutate(fish_species="hybognathus amarus") %>% 
  mutate(combo_fish_parasite=paste(fish_species,psite_spp,sep="_"))
                    

HYBAMA_data$Sex <- ifelse(HYBAMA_data$Sex == TRUE, "M",
                  ifelse(HYBAMA_data$Sex == FALSE, "F", NA))

GAMAFF_data<- read_csv("data/processed/Gambusia_affinis_processed_machine_readable_2025.08.01.csv")%>% 
  mutate(fish_species="gambusia affinis") %>% 
  mutate(combo_fish_parasite=paste(fish_species,psite_spp,sep="_"))

fish_data<- bind_rows(HYBAMA_data,GAMAFF_data,)
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
    Latitude < 35.1609175651113 & Latitude >=34.94913162611374 ~ "below",
    TRUE ~ "no_intervention"
  ))

#setting Corrales levee construction date
before_ab_corrales_levee <- ab_corrales_levee %>% 
  mutate(before_after_corrales = case_when(
    YearCollected >=1997 ~ "after",
    YearCollected >=1986 & YearCollected<1997 ~"during",
    YearCollected < 1986 & YearCollected>=1966 ~"before",
    TRUE ~"no_intervention"
  ))

#setting Alb. Middle Rio Grande West Levee bounds
ab_amrg_wlevee <- before_ab_corrales_levee %>% 
  mutate(w_amrg_locale = case_when(
    Latitude>35.095806160112076 & Latitude <=35.2 ~"above",
    Latitude>= 34.94913162611374 & Latitude <=35.095806160112076 ~ "within",
    Latitude < 34.94913162611374 ~"below",
    TRUE ~ "no_intervention"
  ))
#Setting Alb. Middle Rio Grande West Levee construction dates
before_ab_amrg_wlevee<-ab_amrg_wlevee %>% 
  mutate(before_after_wamrg=case_when(
    YearCollected>=1956 & YearCollected<=1976~"after",
    YearCollected>=1951 & YearCollected <1956 ~"during",
    YearCollected<1951 & YearCollected>=1931 ~"before",
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
    YearCollected>=1975 & YearCollected<=1995~"after",
    YearCollected>=1965 & YearCollected<1975~"during",
    YearCollected<1966 & YearCollected>=1946 ~"before",
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
    YearCollected>=1969 & YearCollected<=1989 ~"after",
    YearCollected>=1951 & YearCollected <1969 ~"during",
    YearCollected<1951 & YearCollected>=1931 ~"before",
    TRUE ~ "no_intervention"
  ))
#Summing parasite counts
before_ab_amrg_elevee$taxon_group <- dplyr::case_when(
  before_ab_amrg_elevee$psite_spp %in% c("cope.imler", "cope.lern","crus.d","crus.lersp") ~ "copepoda",
  before_ab_amrg_elevee$psite_spp %in% c("cest.botsp")~"cestoda",
  before_ab_amrg_elevee$psite_spp %in% c("mono.dact","mono.gyro","mono.salsp","mono.ss","mono.gyrof")~"monogenea",
  before_ab_amrg_elevee$psite_spp %in% c("myxo.b","myxo.g","myxo.myxid")~"myxozoan",
  before_ab_amrg_elevee$psite_spp %in% c("nem.cl","nem.l","nem.larv","nem.myst","nem.unk")~"nematoda",
  before_ab_amrg_elevee$psite_spp %in% c("trem.b","trem.d","trem.diplo","trem.dips","trem.dlum","trem.em","trem.fim","trem.gold","trem.iz","trem.l","trem.meta.unk","trem.ridge","trem.unk")~"trematoda",
  before_ab_amrg_elevee$psite_spp %in% c("acanth.spk")~"acanthocephalan",
  TRUE ~ "NA"
)
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
#Corrales Model
model<-glmmTMB(
  psite_count~ corrales_locale*before_after_corrales+(1|CatalogNumber)+(1|combo_fish_parasite)+(1|YearCollected),
  family = nbinom2(),
  data= corrales_BACI,
  ziformula=~1
)
cmodelOutput<-simulateResiduals(fittedModel = model, plot = TRUE)
summary(model)
plot(parameters(model))

predict_1 <- ggpredict(
  model,
  terms = c("before_after_corrales", "corrales_locale"),
  bias_correction =TRUE
) 
#FULL CORRALES PLOT
predict_C<-ggplot(data = predict_1, aes(x = x, y = predicted, group=group))+
  facet_wrap(~group)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, position = position_dodge(width = 0.5))+
  geom_line(color = "#E30B5D") + 
  geom_point(size = 5, pch=21, position=position_dodge(width=0.5), fill = "#E30B5D", color = "#E30B5D") + 
  labs(x = element_blank(), y = "Parasite Sum")+ 
  theme(panel.background = element_blank(),
        axis.title.y = element_text(face = "bold", size = 20))
predict_C +theme(strip.text.x = element_text(face = "bold", size = 14))
#CONDENSED
predict_1$x_num <- as.numeric(predict_1$x)
set.seed(1000)
predict_1$x_jittered <- predict_1$x_num + runif(nrow(predict_1), -0.01, 0.01)
ggplot(predict_1, aes(x = x_jittered, y = predicted, color = group)) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_line(aes(group = group), linewidth = 1) +
  scale_x_continuous(
    breaks = 1:3,
    labels = levels(predict_1$x)
  ) +
  scale_color_manual(
    values = c("above"  = "blue", "below" = "orange", "within" = "darkgreen")
  ) +
  labs(
    title = "Corrales",
    x = "Time Period",
    y = "Predicted Parasite Count",
    color = "Location"
  ) +
  coord_cartesian(ylim = c(0, 0.2)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

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


E_model<-glmmTMB(
  psite_count~ e_amrg_locale*before_after_eamrg+(1|CatalogNumber)+(1|combo_fish_parasite)+(1|YearCollected),
  family = nbinom2(),
  data= e_levee_BACI,
  ziformula=~1
)


emodelOutput<-simulateResiduals(fittedModel = E_model, plot = TRUE)
summary(E_model)
plot(parameters(E_model))
#FULL PREDICT PLOT
predict_2 <- ggpredict(
  E_model,
  terms = c("before_after_eamrg", "e_amrg_locale"),
  bias_correction = TRUE,
)
predict_E<-ggplot(data = predict_2, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, position = position_dodge(width = 0.5)) + 
  geom_line(color = "orange") + 
  geom_point(size = 5, pch=21, position=position_dodge(width=0.5), fill = "orange", color = "orange") + 
  labs(x = element_blank(), y = "Parasite Sum")+ 
  theme(panel.background = element_blank(),
        axis.title.y = element_text(face = "bold", size = 20))
predict_E +theme(
  strip.text.x = element_text(face = "bold", size = 14))
#CONDENSED
predict_2$x_num <- as.numeric(predict_2$x)
set.seed(42)
predict_2$x_jittered <- predict_2$x_num + runif(nrow(predict_2), -0.15, 0.15)
ggplot(predict_2, aes(x = x_jittered, y = predicted, color = group)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_line(aes(group = group), linewidth = 1) +
  scale_x_continuous(
    breaks = 1:3,
    labels = levels(predict_2$x)
  ) +
  scale_color_manual(
    values = c("above"  = "blue", "below" = "orange", "within" = "darkgreen")
  ) +
  labs(
    title = "East Alb. Middle Rio Grande",
    x = "Time Period",
    y = "Predicted Parasite Count",
    color = "Location"
  ) +
  coord_cartesian(ylim = c(0, 0.3)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

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
W_model<-glmmTMB(
  psite_count~ w_amrg_locale*before_after_wamrg+(1|YearCollected)+(1|CatalogNumber)+(1|combo_fish_parasite),
  family = nbinom2(),
  data= w_levee_BACI,
  ziformula=~1
)

wmodelOutput<-simulateResiduals(fittedModel = W_model, plot = TRUE)
summary(W_model)
plot(parameters(W_model))
#FULL PLOT
predict_3 <- ggpredict(
  W_model,
  terms = c("before_after_wamrg", "w_amrg_locale"),
  bias_correction = TRUE,
)
predict_W<-ggplot(data = predict_3, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, position = position_dodge(width = 0.5)) + 
  geom_line(color = "orange") + 
  geom_point(size = 5, pch=21, position=position_dodge(width=0.5), fill = "orange", color = "orange") + 
  labs(x = element_blank(), y = "Parasite Sum")+ 
  theme(panel.background = element_blank(),
        axis.title.y = element_text(face = "bold", size = 20))
predict_W +theme(
    strip.text.x = element_text(face = "bold", size = 14)
  )
#CONDENSED
predict_3$x_num <- as.numeric(predict_3$x)
set.seed(42)
predict_3$x_jittered <- predict_3$x_num + runif(nrow(predict_3), -0.15, 0.15)
ggplot(predict_3, aes(x = x_jittered, y = predicted, color = group)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_line(aes(group = group), linewidth = 1) +
  scale_x_continuous(
    breaks = 1:2,
    labels = levels(predict_3$x)
  ) +
  scale_color_manual(
    values = c("above"  = "blue", "below" = "orange", "within" = "darkgreen")
  ) +
  labs(
    title = "West Alb. Middle Rio Grande",
    x = "Time Period",
    y = "Predicted Parasite Count",
    color = "Location"
  ) +
  coord_cartesian(ylim = c(0, 0.05)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

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
#SANDOVAL MODEL
S_model<-glmmTMB(psite_count~ sandoval_locale*before_after_sandoval+(1|YearCollected)+(1|combo_fish_parasite),
  family = nbinom2(),
  data= sandoval_BACI,
  ziformula=~1
)

smodelOutput<-simulateResiduals(fittedModel = S_model, plot = TRUE)
summary(S_model)
plot(parameters(S_model))
#FULL PLOT
predict_4 <- ggpredict(
  S_model,
  terms = c("before_after_sandoval", "sandoval_locale"),
  bias_correction = TRUE,
)
predict_S<-ggplot(data = predict_4, aes(x = x, y = predicted, group = group)) +facet_wrap(~group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, position = position_dodge(width = 0.5)) + 
  geom_line(color = "green") + 
  geom_point(size = 5, pch=21, position=position_dodge(width=0.5), fill = "green", color = "green") + 
  labs(x = element_blank(), y = "Parasite Sum", )+ 
  theme(panel.background = element_blank(),
        axis.title.y = element_text(face = "bold", size = 20))
predict_S +
  theme(
    strip.text.x = element_text(face = "bold", size = 14)
  )
#CONDENSED MODEL
predict_4$x_num <- as.numeric(predict_4$x)
set.seed(50)
ggplot(predict_4, aes(x = x, y = predicted, color = group)) +
  geom_point(size=5)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_line(aes(group = group), linewidth = 1) +
  scale_color_manual(
    values = c("above"  = "blue", "below" = "orange", "within" = "darkgreen")
  ) +
  labs(
    title = "Sandoval",
    x = "Time Period",
    y = "Predicted Parasite Count",
    color = "Location"
  ) +
  coord_cartesian(ylim = c(0, 0.2)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

predict_4$x_jittered <- predict_4$x_num + runif(nrow(predict_4), -0.15, 0.15)
scale_x_continuous(
  breaks = 1:3,
  labels = levels(predict_4$x)
)
# the emmeans measures whether your highest order relationships are significant or not (interaction itself, not the factors of the interaction)
emm <- emmeans(S_model, ~ sandoval_locale*before_after_sandoval)
joint_tests(S_model) # idk what to make of this output at the moment












