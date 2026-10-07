# Asking the "big question" of the HEAP project
# Chelsea Wood
# chelwood@uw.edu
# Started 5 August 2025, picked back up 30 Sep 2026

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


# Create Table 1 for the MS, summarizing the data for each fish host species.

total_n_fish_lines <- minus_myxos %>%
  group_by(IndividualFishID,fish_spp) %>%
  summarize(count = n())
sum(total_n_fish_lines$count)

total_n_fish <- total_n_fish_lines %>%
  group_by(fish_spp) %>%
  summarize(count = n())

body_size_fish <- minus_myxos %>%
  group_by(fish_spp) %>%
  summarize(TL = mean(TotalLength_mm), min_TL = min(TotalLength_mm), max_TL = max(TotalLength_mm))

sum(total_n_fish$count)

399+209+317

minus_myxos$host_psite_combo<-paste(minus_myxos$fish_spp,minus_myxos$psite_spp,sep="_")
psite_species <- levels(as.factor(minus_myxos$host_psite_combo))
length(psite_species)

sum(as.numeric(minus_myxos$psite_count),na.rm=TRUE)


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


# Keep only the parasites at >5% prevalence and create Table 2, summarizing the prevalence and identity 
# of common parasite taxa.

minus_myxos$positive<-vector("numeric",length(minus_myxos$host_psite_combo))

for(i in 1:length(minus_myxos$host_psite_combo)) {
  
  if(is.na(minus_myxos$psite_count[i])){
    minus_myxos$positive[i] <- NA
  } else {
    
    if(minus_myxos$psite_count[i] > 0){
      minus_myxos$positive[i] <- 1
      
    } else {
      
      if(minus_myxos$psite_count[i] == 0){
        minus_myxos$positive[i] <- 0
      }
    }
  }
}

thing <- minus_myxos %>%
  group_by(host_psite_combo) %>%
  summarise(pos = sum(as.numeric(positive),na.rm=T), count = length(positive))

thing$prev <- thing$pos/thing$count
view(thing)
length(thing$host_psite_combo)

high_prev_psites <- thing %>%
  filter(prev>=0.05)
view(high_prev_psites)
length(high_prev_psites$host_psite_combo)

high_prev_psites$host_psite_combo

final_dataset <- minus_myxos %>%
  filter(host_psite_combo == "Gambusia affinis_mono.salsp" | host_psite_combo == "Gambusia affinis_mono.ss" |
           host_psite_combo == "Gambusia affinis_trem.dips" |
           host_psite_combo == "Hybognathus amarus_mono.dact" | host_psite_combo == "Hybognathus amarus_mono.gyro" |
           host_psite_combo == "Hybognathus amarus_trem.b" | host_psite_combo == "Hybognathus amarus_trem.d" |
           host_psite_combo == "Hybognathus amarus_trem.diplo" | 
           host_psite_combo == "Pimephales promelas_mono.dacsp" |
           host_psite_combo == "Pimephales promelas_trem.diphur")

levels(as.factor(final_dataset$host_psite_combo))

sum(as.numeric(final_dataset$psite_count),na.rm=TRUE)



#### MODELS


# I couldn't get models to converge with an offset term, but maybe that's okay? Putting it here for safekeeping.

(offset(log(TotalLength_mm)))


# Test the big hyps

final_dataset$psite_count <- as.numeric(final_dataset$psite_count)
final_dataset$YearCollected_sc <- scale(final_dataset$YearCollected)
final_dataset$TotalLength_mm_sc <- scale(final_dataset$TotalLength_mm)
final_dataset$CatalogNumber_chr <- as.character(final_dataset$CatalogNumber)

model_draft_1<-glmer.nb(psite_count~CI*YearCollected_sc+fish_spp*TotalLength_mm_sc+
                          (1+CI*YearCollected_sc|host_psite_combo)+
                          (1|CatalogNumber_chr),
                        data=final_dataset,family="nbinom")
summary(model_draft_1)


# Okay, but this doesn't control for spatial autocorrelation among sites - CatalogNumber sort of does, but maybe
# we want a more explicit term that accounts for the fact that near sites should be more similar than far sites.

final_dataset <- final_dataset %>%
  group_by(CI) %>%
  mutate(
    Latitude_within = Latitude - mean(Latitude, na.rm = TRUE)
  ) %>%
  ungroup()

model_draft_2<-glmer.nb(psite_count~CI*YearCollected_sc+fish_spp*TotalLength_mm_sc+Latitude_within+
                          (1+CI*YearCollected_sc|host_psite_combo)+
                          (1|CatalogNumber_chr),
                        data=final_dataset,family="nbinom")
summary(model_draft_2)

VarCorr(model_draft_2)


# Model is clearly unhappy. Let's simplify the random effects structure.

final_dataset$CI_num <- as.numeric(
  final_dataset$CI == "impact"
)

model_draft_3<-glmer.nb(psite_count~
                          CI_num*YearCollected_sc+
                          fish_spp*TotalLength_mm_sc+
                          Latitude_within+
                          (1 | host_psite_combo) +
                          (0 + CI_num | host_psite_combo) +
                          (0 + YearCollected_sc | host_psite_combo) +
                          (0 + I(CI_num * YearCollected_sc) | host_psite_combo) +
                          (1|CatalogNumber_chr),
                        data=final_dataset,
                        family="nbinom",
                        control = glmerControl(
                          optimizer = "bobyqa",
                          optCtrl = list(maxfun = 2e5)))


summary(model_draft_3)


# Still unhappy. We only have 10 parasite species - what if we just treated them as a fixed effect?

model_draft_4<-glmer.nb(psite_count~
                          CI*YearCollected_sc*host_psite_combo+
                          fish_spp*TotalLength_mm_sc+
                          Latitude_within+
                          (1|CatalogNumber_chr),
                        data=final_dataset,
                        family="nbinom",
                        control = glmerControl(
                          optimizer = "bobyqa",
                          optCtrl = list(maxfun = 2e5)))

summary(model_draft_4)

# Rank deficient. Let's figure out why.

fixef(model_draft_4, add.dropped = TRUE)
with(final_dataset, table(host_psite_combo, CI))

dropped <- fixef(model_draft_4, add.dropped = TRUE)
dropped[is.na(dropped)]

# K, looks like fish_spp and host_psite_combo are collinear. Take out the main effect of fish_spp.

model_draft_5<-glmer.nb(psite_count~
                          CI*YearCollected_sc*host_psite_combo+
                          fish_spp:TotalLength_mm_sc+
                          Latitude_within+
                          (1|CatalogNumber_chr),
                        data=final_dataset,
                        family="nbinom",
                        control = glmerControl(
                          optimizer = "bobyqa",
                          optCtrl = list(maxfun = 2e5)))

summary(model_draft_5)


big_predictions<-ggeffect(model_draft_5,c("YearCollected_sc","CI","host_psite_combo"))
str(big_predictions)

diverging_pal <- c("#5ab4ac","#d8b365")
darker_pal <- c("#018571","#a6611a")

big_plot<-ggplot(big_predictions,aes(x,predicted),group=group,color=group)+
  geom_ribbon(data=big_predictions,mapping=aes(x=x,ymin=conf.low,ymax=conf.high,fill=group),alpha=0.5)+
  geom_line(aes(group=group,color=group))+
  scale_color_manual(values=darker_pal)+
  scale_fill_manual(values=diverging_pal)+
  facet_grid(~facet)+
  ylim(0,10)+
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


# Okay, so it looks like only two species really possess the interaction, and only marginally. So having psite_spp
# interact with CI*Year is not particularly helpful. Pare down the model accordingly.

model_draft_7<-glmer.nb(psite_count~
                          CI*YearCollected_sc*fish_spp+
                          host_psite_combo*TotalLength_mm_sc+
                          Latitude_within+
                          (1|CatalogNumber_chr),
                        data=final_dataset,
                        family="nbinom",
                        control = glmerControl(
                          optimizer = "bobyqa",
                          optCtrl = list(maxfun = 2e5)))

summary(model_draft_7)


# Okay, nothing. Let's start eliminating terms to spare degrees of freedom.

model_draft_8<-glmer.nb(psite_count~
                          CI+YearCollected_sc*fish_spp+
                          host_psite_combo*TotalLength_mm_sc+
                          Latitude_within+
                          (1|CatalogNumber_chr),
                        data=final_dataset,
                        family="nbinom",
                        control = glmerControl(
                          optimizer = "bobyqa",
                          optCtrl = list(maxfun = 2e5)))

summary(model_draft_8)


big_predictions<-ggpredict(model_draft_8,c("YearCollected_sc","fish_spp"))
str(big_predictions)

diverging_pal <- c("#5ab4ac","#d8b365")
darker_pal <- c("#018571","#a6611a")

big_plot<-ggplot(big_predictions,aes(x,predicted,group=group))+
  #geom_ribbon(data=big_predictions,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),alpha=0.5)+
  geom_line(aes(group=group,color=group))+
  #scale_color_manual(values=darker_pal)+
  scale_fill_manual(values=diverging_pal)+
  #facet_grid(~facet)+
  #ylim(0,10)+
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


# Okay, so I think the story here isn't about urbanization... there isn't a signal of BA*CI. Instead, the overriding
# pattern is a decline in the parasites of HYBAMA - the only fish that has diminished in density across the study 
# period. To differentiate this conclusion from that of Gabby's paper, I think that we need to bring in the fish 
# density data - test whether the decline in parasites observed in HYBAMA is correlated with declines in fish density.
# Luckily, Tom told us where we can find some fish density data.

# Steve Platania's Middle Rio Grande survey might work (1993 onward), but it loses a huge chunk of the dataset
# (especially early years when HYBAMA parasites were much more abundant).



# But I think fish species might be the most important factor here... Gambusia in particular looks really different
# from the other two. How to account for that?

model_draft_6<-glmer.nb(psite_count~
                          CI*YearCollected_sc+
                          host_psite_combo*TotalLength_mm_sc+
                          Latitude_within+
                          (1|CatalogNumber_chr),
                        data=final_dataset,
                        family="nbinom",
                        control = glmerControl(
                          optimizer = "bobyqa",
                          optCtrl = list(maxfun = 2e5)))

summary(model_draft_6)




# Then pull out the individual parasite trajectories and plot them as in the Puget Sound paper

random_effects<-ranef(model_draft_1)
random_effects<-random_effects$`host_psite_combo`
random_effects<-as.data.frame(random_effects)
random_effects$n_hosts<-c(1,1,3,1,1,3,3,3,1,3)
random_effects$fish_spp<-c("GAMAFF","GAMAFF","GAMAFF","HYBAMA","HYBAMA","HYBAMA","HYBAMA",
                           "HYBAMA","PIMPRO","PIMPRO")

random_effects$psite_code<-rownames(random_effects)
colnames(random_effects)[1]<-"intercept"
colnames(random_effects)[4]<-"slope"


# Put everything in order so you don't have to do it manually later on

final_data_ordered <- random_effects %>%
  arrange(psite_code) %>%
  arrange(factor(n_hosts, levels = c("1","2","3")))


# Get your colors

library(wesanderson)
pal<-wes_palette(name="Zissou1",10,type="continuous")
cols <- wes_palette(10, name = "Zissou1",type="continuous")[c(1,4,7)]


# Organized by number of hosts

indiv_psites_plot<-ggplot(random_effects,aes(x=slope,y=psite_code,label=n_hosts))+
  #geom_rect(xmin=-2.09,xmax=2.09,ymin=20.5,ymax=29.6,fill="#E4B80E",alpha=0.05)+
  #geom_rect(xmin=-2.09,xmax=2.09,ymin=13.5,ymax=20.5,fill="#9EBE91",alpha=0.05)+
  #geom_rect(xmin=-2.09,xmax=2.09,ymin=0.4,ymax=13.5,fill="#3B9AB2",alpha=0.05)+
  geom_point()+
  #geom_errorbar(aes(xmin=Estimate-Std..Error,xmax=Estimate+Std..Error))+
  geom_vline(xintercept = 0,lty=1)+
  #geom_hline(yintercept = 65.5,lty=3,lwd=0.25)+
  #geom_hline(yintercept = 44.5,lty=3,lwd=0.25)+
  xlab("interaction effect year*CI")+
  ylab("parasite code")+
  #xlim(-1.9,1.9)+
  scale_y_discrete(limits=rev(final_data_ordered$psite_code))+
  #geom_text(x=-2,angle=0,hjust=0,vjust=0.5)+
  #annotate("text",label=c("Copepoda","Hirudinea","Monogenea","Trematoda","Cestoda","Nematoda","Acanthocephala"),
  #         x=,y=(final_data_ordered$order+1.35),hjust=0.5,vjust=0.5,size=3)+
  #annotate("text",label=c("1 host","2 hosts","3+ hosts"),x=-1.8,y=c(25,17,7),size=5,hjust = 0.5)+
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
  #geom_rect(xmin=-2.09,xmax=2.09,ymin=16.5,ymax=29.6,fill="#E4B80E",alpha=0.05)+
  #geom_rect(xmin=-2.09,xmax=2.09,ymin=13.5,ymax=20.5,fill="#9EBE91",alpha=0.05)+
  #geom_rect(xmin=-2.09,xmax=2.09,ymin=0.4,ymax=16.5,fill="#3B9AB2",alpha=0.05)+
  geom_point()+
  #geom_errorbar(aes(xmin=Estimate-Std..Error,xmax=Estimate+Std..Error))+
  geom_vline(xintercept = 0,lty=1)+
  #geom_hline(yintercept = 65.5,lty=3,lwd=0.25)+
  #geom_hline(yintercept = 44.5,lty=3,lwd=0.25)+
  xlab("random effect of year")+
  ylab("parasite code")+
  #xlim(-1.9,1.9)+
  scale_y_discrete(limits=rev(final_data_ordered$psite_code))+
  #geom_text(x=-2,angle=0,hjust=0,vjust=0.5)+
  #annotate("text",label=c("Copepoda","Hirudinea","Monogenea","Trematoda","Cestoda","Nematoda","Acanthocephala"),
  #         x=,y=(final_data_ordered$order+1.35),hjust=0.5,vjust=0.5,size=3)+
  #annotate("text",label=c(expression(italic("Gambusia affinis")),expression(italic("Hybognathus amarus"))),
   #                       x=1.5,y=c(23,8),size=5,hjust = 0.5)+
  theme_classic()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"),panel.grid.major.y = element_line(color="darkgray"),
        axis.text.y = element_text(size=7), axis.title = element_text(size = 18))+
  coord_cartesian(clip="off")

fish_spp_plot
