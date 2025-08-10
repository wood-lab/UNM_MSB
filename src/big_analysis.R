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


# Add in life history strategy

minus_myxos$LH_strategy<-vector("character",length(minus_myxos$CatalogNumber))

for(i in 1:length(minus_myxos$CatalogNumber)) {
  
  if(grepl("mono", minus_myxos$psite_spp[i])){
    minus_myxos$LH_strategy[i] <- "direct"
    
  } else {
    
    if(grepl("cope", minus_myxos$psite_spp[i])){
      minus_myxos$LH_strategy[i] <- "direct"
      
    } else {
      
      if(grepl("crus", minus_myxos$psite_spp[i])){
        minus_myxos$LH_strategy[i] <- "direct"
        
      } else {
      
      minus_myxos$LH_strategy[i] <- "complex"
    }
    }
  }}

minus_myxos$LH_strategy 

thing <- minus_myxos %>%
  group_by(LH_strategy, psite_spp) %>%
  summarise(n = n())
View(thing)


# Add in parasite taxon

minus_myxos$psite_taxon<-vector("character",length(minus_myxos$CatalogNumber))

for(i in 1:length(minus_myxos$CatalogNumber)) {
  
  if(grepl("mono", minus_myxos$psite_spp[i])){
    minus_myxos$psite_taxon[i] <- "monogene"
    
  } else {
    
    if(grepl("cope", minus_myxos$psite_spp[i])){
      minus_myxos$psite_taxon[i] <- "copepod"
      
    } else {
      
      if(grepl("crus", minus_myxos$psite_spp[i])){
        minus_myxos$psite_taxon[i] <- "other crustacean"
        
      } else {
        
        if(grepl("nem", minus_myxos$psite_spp[i])){
          minus_myxos$psite_taxon[i] <- "nematode"
          
        } else {
          
          if(grepl("ces", minus_myxos$psite_spp[i])){
            minus_myxos$psite_taxon[i] <- "cestode"
            
          } else {
            
            if(grepl("trem", minus_myxos$psite_spp[i])){
              minus_myxos$psite_taxon[i] <- "trematode"
              
            } else {
              
              if(grepl("acan", minus_myxos$psite_spp[i])){
                minus_myxos$psite_taxon[i] <- "acanthocephalan"
                
              } else {
      
      minus_myxos$psite_taxon[i] <- "NA"
    }
  }
        }}}}}}

thing <- minus_myxos %>%
  group_by(psite_taxon, psite_spp) %>%
  summarise(n = n())
View(thing)


# Messing around with models

(offset(log(TotalLength_mm)))

model_draft<-glmer.nb(as.numeric(psite_count)~CI+scale(YearCollected)+scale(Latitude)+scale(TotalLength_mm)+
                        (1|psite_spp)+(1|CatalogNumber),
                      data=minus_myxos,family="nbinom")
summary(model_draft)


# Parasite-taxon level

model_draft<-glmer.nb(as.numeric(psite_count)~psite_taxon*scale(YearCollected)+scale(Latitude)+scale(TotalLength_mm)+
                        (1|psite_spp)+(1|CatalogNumber),
                      data=minus_myxos,family="nbinom")
summary(model_draft)


# Test the big hyps

model_draft_1<-glmer.nb(as.numeric(psite_count)~CI*scale(YearCollected)+scale(Latitude)+scale(TotalLength_mm)+
                        (1|fish_spp/psite_spp)+(1|CatalogNumber),
                      data=minus_myxos,family="nbinom")
summary(model_draft_1)



# Three-way interaction


model_draft_1<-glmer.nb(as.numeric(psite_count)~CI*scale(YearCollected)*LH_strategy+scale(Latitude)+scale(TotalLength_mm)+
                          (1|fish_spp/psite_spp)+(1|CatalogNumber),
                        data=minus_myxos,family="nbinom")
summary(model_draft_1)


# Then pull out the individual parasite trajectories and plot them as in the Puget Sound paper

model_draft_2<-glmer.nb(as.numeric(psite_count)~CI*scale(YearCollected)*LH_strategy+scale(Latitude)+scale(TotalLength_mm)+
                          (scale(YearCollected)|fish_spp/psite_spp)+(1|CatalogNumber),
                        data=minus_myxos,family="nbinom")
summary(model_draft_2)





big_predictions<-ggeffect(model_draft_1,c("YearCollected","CI","LH_strategy"))
str(big_predictions)

diverging_pal <- c("#5ab4ac","#d8b365")
darker_pal <- c("#018571","#a6611a")

big_plot<-ggplot(big_predictions,aes(x,predicted),group=group,color=group)+
  geom_ribbon(data=big_predictions,mapping=aes(x=x,ymin=conf.low,ymax=conf.high,fill=group),alpha=0.5)+
  geom_line(aes(group=group,color=group))+
  scale_color_manual(values=darker_pal)+
  scale_fill_manual(values=diverging_pal)+
  facet_grid(~facet)+
  xlab("Year")+
  ylab("predicted parasite abundance\n per parasite taxon per host individual")+
  theme_minimal()+
  #labs(linetype="parasite life history strategy")+
  theme(plot.title=element_text(size=24,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),strip.text.x = element_text(size = 20),
        axis.text.x=element_text(size=18,color="black"),axis.title.x=element_text(size=16),
        panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color=NA),
        panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_discrete(limits=(rev(levels(big_predictions$x))))+
  theme(legend.position="top",legend.title = element_blank(),
        legend.text = element_text(size=20))
big_plot

