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
library(dplyr)


# R is really useful for doing quick tallies and plots.
# Let's do some together!

hyb_ama_data<-read.csv("data/processed/Hybognathus_amarus_processed_machine_readable_2025.07.06.csv")
gam_aff_data<-read.csv("data/processed/Gambusia_affinis_processed_machine_readable_2025.08.01.csv")
pim_pro_data<-read.csv("data/processed/Pimephales_promelas_processed_machine_readable_2025.08.12.csv")
hyb_ama_data$fish_spp<-"Hybognathus amarus"
gam_aff_data$fish_spp<-"Gambusia affinis"
pim_pro_data$fish_spp<-"Pimephales promelas"
all_data <- rbind(hyb_ama_data,gam_aff_data,pim_pro_data)


# Quick tallies

length(unique(all_data$IndividualFishID))
length(unique(all_data$psite_spp))
sum(as.numeric(all_data$psite_count),na.rm=T)


# Trim out parasites at <5% prevalence

all_data$positive <- ifelse(all_data$psite_count > 0 , 1, 0)

thing <- all_data %>%
  group_by(fish_spp,psite_spp) %>%
  summarise(pos = sum(as.numeric(positive),na.rm=T), count = length(positive))

thing$prev <- thing$pos/thing$count
view(thing)


# Trim out all the myxos

minus_myxos <- all_data %>%
  filter(!grepl("myx", psite_spp))


# Keep only the parasites at >5% prevalence

common_psites <- minus_myxos %>%
  filter(grepl("mono.salsp|mono.ss|trem.dips|mono.dact|mono.gyro|trem.b|trem.d|trem.diplo|mono.dacsp|trem.diphur", psite_spp))


# Add in life history strategy

common_psites$LH_strategy<-vector("character",length(common_psites$CatalogNumber))

for(i in 1:length(common_psites$CatalogNumber)) {
  
  if(grepl("mono", common_psites$psite_spp[i])){
    common_psites$LH_strategy[i] <- "direct"
    
  } else {
    
    if(grepl("cope", common_psites$psite_spp[i])){
      common_psites$LH_strategy[i] <- "direct"
      
    } else {
      
      if(grepl("crus", common_psites$psite_spp[i])){
        common_psites$LH_strategy[i] <- "direct"
        
      } else {
      
        common_psites$LH_strategy[i] <- "complex"
    }
    }
  }}

common_psites$LH_strategy 

thing <- common_psites %>%
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


# Add in number of hosts

minus_myxos$n_hosts<-vector("character",length(minus_myxos$CatalogNumber))

for(i in 1:length(minus_myxos$CatalogNumber)) {
  
  if(grepl("mono", minus_myxos$psite_spp[i])){
    minus_myxos$n_hosts[i] <- "1"
    
  } else {
    
    if(grepl("cope", minus_myxos$psite_spp[i])){
      minus_myxos$n_hosts[i] <- "1"
      
    } else {
      
      if(grepl("crus", minus_myxos$psite_spp[i])){
        minus_myxos$n_hosts[i] <- "1"
        
      } else {
        
        if(grepl("nem", minus_myxos$psite_spp[i])){
          minus_myxos$n_hosts[i] <- "2"
          
        } else {
          
          if(grepl("ces", minus_myxos$psite_spp[i])){
            minus_myxos$n_hosts[i] <- "2"
            
          } else {
            
            if(grepl("trem", minus_myxos$psite_spp[i])){
              minus_myxos$n_hosts[i] <- "3"
              
            } else {
              
              if(grepl("acan", minus_myxos$psite_spp[i])){
                minus_myxos$n_hosts[i] <- "2"
                
              } else {
                
                minus_myxos$n_hosts[i] <- "NA"
              }
            }
          }}}}}}

thing <- minus_myxos %>%
  group_by(psite_taxon, psite_spp, n_hosts) %>%
  summarise(n = n())
View(thing)


#### Messing around with models


# I couldn't get models to converge with an offset term, but maybe that's okay? Putting it here for safekeeping.

(offset(log(TotalLength_mm)))


# Basic model with no interactions

model_draft<-glmer.nb(as.numeric(psite_count)~CI+scale(YearCollected)+scale(Latitude)+scale(TotalLength_mm)+
                        (1|psite_spp)+(1|CatalogNumber),
                      data=minus_myxos,family="nbinom")
summary(model_draft)


# Parasite-taxon level inferences about change over time (no CI)

model_draft<-glmer.nb(as.numeric(psite_count)~psite_taxon*scale(YearCollected)+scale(Latitude)+scale(TotalLength_mm)+
                        (1|psite_spp)+(1|CatalogNumber),
                      data=minus_myxos,family="nbinom")
summary(model_draft)


# Test the big hyps - basic model with no test for how things differ among parasite types

model_draft_1<-glmer.nb(as.numeric(psite_count)~CI*scale(YearCollected)+scale(Latitude)+scale(TotalLength_mm)+
                        (1|fish_spp/psite_spp)+(1|CatalogNumber),
                      data=minus_myxos,family="nbinom")
summary(model_draft_1)


# Test the big hyps - now accounting for differences between complex versus direct


model_draft_1<-glmer.nb(as.numeric(psite_count)~CI*scale(YearCollected)*LH_strategy+scale(Latitude)+scale(TotalLength_mm)+
                          (1|fish_spp/psite_spp)+(1|CatalogNumber),
                        data=minus_myxos,family="nbinom")
summary(model_draft_1)


# Test the big hyps - now accounting for differences between complex versus direct PLUS adding a random effect
# term to test whether different parasite species just have different baseline change through time


model_draft_2<-glmer.nb(as.numeric(psite_count)~CI*scale(YearCollected)*LH_strategy+scale(Latitude)+scale(TotalLength_mm)+
                          (scale(YearCollected)|fish_spp/psite_spp)+(1|CatalogNumber),
                        data=minus_myxos,family="nbinom")
summary(model_draft_2)


# Test the big hyps - now accounting for differences among parasites with 1, 2, or 3 hosts PLUS adding a random effect
# term to test whether different parasite species just have different baseline change through time PLUS allowing
# each parasite taxon to have a different relationship with TL


model_draft_3<-glmer.nb(as.numeric(psite_count)~CI*scale(YearCollected)*LH_strategy+scale(Latitude)+
                          (scale(YearCollected)|fish_spp/psite_spp)+(scale(TotalLength_mm)|fish_spp/psite_spp)
                        +(1|CatalogNumber),
                        data=common_psites,family="nbinom")
summary(model_draft_3)


# Create a prediction plot

big_predictions<-ggeffect(model_draft_3,c("YearCollected","CI","LH_strategy"))
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


# Then pull out the individual parasite trajectories and plot them as in the Puget Sound paper

random_effects<-ranef(model_draft_3)
random_effects<-random_effects$`psite_spp:fish_spp`
random_effects<-as.data.frame(random_effects)
random_effects$n_hosts<-c(2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3)
random_effects$fish_spp<-c("GAMAFF","GAMAFF","HYBAMA","HYBAMA","GAMAFF","GAMAFF","HYBAMA","HYBAMA","GAMAFF","GAMAFF",
                           "GAMAFF","HYBAMA","GAMAFF","GAMAFF","GAMAFF","HYBAMA","HYBAMA","HYBAMA","HYBAMA","GAMAFF",
                           "HYBAMA","HYBAMA","HYBAMA","HYBAMA","GAMAFF","HYBAMA","HYBAMA","HYBAMA","GAMAFF")

random_effects$psite_code<-rownames(random_effects)
colnames(random_effects)[1]<-"intercept"
colnames(random_effects)[2]<-"slope"


# Put everything in order so you don't have to do it manually later on

final_data_ordered <- random_effects %>%
  arrange(psite_code) %>%
  arrange(factor(n_hosts, levels = c("1","2","3")))


# Get your colors

library(wesanderson)
pal<-wes_palette(name="Zissou1",10,type="continuous")
cols <- wes_palette(10, name = "Zissou1",type="continuous")[c(1,4,7)]


# Organized by number of hosts

indiv_psites_plot<-ggplot(final_data_ordered,aes(x=slope,y=psite_code,label=n_hosts))+
  geom_rect(xmin=-2.09,xmax=2.09,ymin=20.5,ymax=29.6,fill="#E4B80E",alpha=0.05)+
  geom_rect(xmin=-2.09,xmax=2.09,ymin=13.5,ymax=20.5,fill="#9EBE91",alpha=0.05)+
  geom_rect(xmin=-2.09,xmax=2.09,ymin=0.4,ymax=13.5,fill="#3B9AB2",alpha=0.05)+
  geom_point()+
  #geom_errorbar(aes(xmin=Estimate-Std..Error,xmax=Estimate+Std..Error))+
  geom_vline(xintercept = 0,lty=1)+
  #geom_hline(yintercept = 65.5,lty=3,lwd=0.25)+
  #geom_hline(yintercept = 44.5,lty=3,lwd=0.25)+
  xlab("random effect of year")+
  ylab("parasite code")+
  xlim(-1.9,1.9)+
  scale_y_discrete(limits=rev(final_data_ordered$psite_code))+
  #geom_text(x=-2,angle=0,hjust=0,vjust=0.5)+
  #annotate("text",label=c("Copepoda","Hirudinea","Monogenea","Trematoda","Cestoda","Nematoda","Acanthocephala"),
  #         x=,y=(final_data_ordered$order+1.35),hjust=0.5,vjust=0.5,size=3)+
  annotate("text",label=c("1 host","2 hosts","3+ hosts"),x=-1.8,y=c(25,17,7),size=5,hjust = 0.5)+
  theme_classic()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"),panel.grid.major.y = element_line(color="darkgray"),
        axis.text.y = element_text(size=7), axis.title = element_text(size = 18))+
  coord_cartesian(clip="off")

indiv_psites_plot



# Put everything in order so you don't have to do it manually later on

final_data_ordered <- random_effects %>%
  arrange(psite_code) %>%
  arrange(fish_spp)


# Organized by host species

fish_spp_plot<-ggplot(final_data_ordered,aes(x=slope,y=psite_code,label=fish_spp))+
  geom_rect(xmin=-2.09,xmax=2.09,ymin=16.5,ymax=29.6,fill="#E4B80E",alpha=0.05)+
  #geom_rect(xmin=-2.09,xmax=2.09,ymin=13.5,ymax=20.5,fill="#9EBE91",alpha=0.05)+
  geom_rect(xmin=-2.09,xmax=2.09,ymin=0.4,ymax=16.5,fill="#3B9AB2",alpha=0.05)+
  geom_point()+
  #geom_errorbar(aes(xmin=Estimate-Std..Error,xmax=Estimate+Std..Error))+
  geom_vline(xintercept = 0,lty=1)+
  #geom_hline(yintercept = 65.5,lty=3,lwd=0.25)+
  #geom_hline(yintercept = 44.5,lty=3,lwd=0.25)+
  xlab("random effect of year")+
  ylab("parasite code")+
  xlim(-1.9,1.9)+
  scale_y_discrete(limits=rev(final_data_ordered$psite_code))+
  #geom_text(x=-2,angle=0,hjust=0,vjust=0.5)+
  #annotate("text",label=c("Copepoda","Hirudinea","Monogenea","Trematoda","Cestoda","Nematoda","Acanthocephala"),
  #         x=,y=(final_data_ordered$order+1.35),hjust=0.5,vjust=0.5,size=3)+
  annotate("text",label=c(expression(italic("Gambusia affinis")),expression(italic("Hybognathus amarus"))),
                          x=1.5,y=c(23,8),size=5,hjust = 0.5)+
  theme_classic()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"),panel.grid.major.y = element_line(color="darkgray"),
        axis.text.y = element_text(size=7), axis.title = element_text(size = 18))+
  coord_cartesian(clip="off")

fish_spp_plot
