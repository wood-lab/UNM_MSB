# Asking the "big question" of the HEAP project
# Chelsea Wood
# chelwood@uw.edu
# 5 August 2025

# Load the packagaes you need

library(ggplot2)
library(tidyverse)
library(lme4)
library(car)
library(ggeffects)
library(reshape2)
library(MASS)


# R is really useful for doing quick tallies and plots.
# Let's do some together!

hyb_ama_data<-read.csv("data/processed/Hybognathus_amarus_processed_machine_readable_2025.07.06.csv")
gam_aff_data<-read.csv("data/processed/Gambusia_affinis_processed_machine_readable_2025.08.01.csv")
hyb_ama_data$fish_spp<-"Hybognathus amarus"
gam_aff_data$fish_spp<-"Gambusia affinis"

all_data <- rbind(hyb_ama_data,gam_aff_data)


# Trim out all the myxos
library(dplyr)
minus_myxos <- all_data %>%
  filter(!grepl("myx", psite_spp))

(offset(log(TotalLength_mm)))

model_draft<-glmer.nb(as.numeric(psite_count)~CI*scale(YearCollected)+scale(Latitude)+scale(TotalLength_mm)+
                        (1|psite_spp)+(1|CatalogNumber),
                      data=minus_myxos,family="nbinom")
summary(model_draft)

big_predictions<-ggeffect(model_draft,c("YearCollected","CI"))

big_plot<-ggplot(big_predictions,aes(x,predicted),group=group,color=group)+
  geom_point(aes(group=group,color=group),size=4,pch=19)+
  geom_errorbar(data=big_predictions,mapping=aes(x=x,ymin=conf.low,ymax=conf.high,group=group,color=group),width=0.03)+
  geom_line(aes(group=group,color=group))+
  #scale_color_manual(name = c(""),values=plasma_pal)+
  xlab("Year")+
  ylab("predicted parasite abundance\n per parasite taxon per host individual")+
  theme_minimal()+
  #labs(linetype="parasite life history strategy")+
  theme(plot.title=element_text(size=18,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=18,color="black"),axis.title.x=element_text(size=16),
        panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color=NA),
        panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=(rev(levels(big_predictions$x))))+
  theme(legend.position="top",legend.title = element_text(size = 18),
        legend.text = element_text(size=12))
big_plot

