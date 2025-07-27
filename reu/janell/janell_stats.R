# Janell's data exploration, plotting, and analysis
# by Janell Goldsmith and Chelsea Wood
# janellngoldsmith@gmail.com and chelwood@uw.edu
# Started 20 July 2025


# Install the packagaes you need

install.packages("lme4")
install.packages("car")
install.packages("ggeffects")
install.packages("reshape2")
install.packages("MASS")


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
view(hyb_ama_data)
gam_aff_data<-read.csv("data/processed/Gambusia_affinis_processed_machine_readable_2025.07.21.csv")
View(gam_aff_data)


# Let's do some quick data tallies - we can see how many values we have for estimates of each parasite species in each
# host across the different treatment-decade combinations

hyb_ama_data %>% 
  group_by(combo) %>% 
  summarize(n = n())

gam_aff_data %>% 
  group_by(combo) %>% 
  summarize(n = n())


# First, we need to limit our dataset to just the diplostomatids. Let's see which parasite categories exist, and which
# we need to prune out.

levels(as.factor(hyb_ama_data$psite_spp))
levels(as.factor(gam_aff_data$psite_spp))


# Looks like, for HYBAMA, we want trem.diplo, trem.dlum, and trem.em. 
# Let's trim the dataset so that it includes only those taxa.

diplo_data_hybama <- hyb_ama_data %>%
  filter(psite_spp == "trem.diplo" | psite_spp == "trem.dlum" | psite_spp == "trem.em")
levels(as.factor(diplo_data_hybama$psite_spp))
view(diplo_data_hybama)


# Looks like, for GAMAFF, we want trem.dips. 
# Let's trim the dataset so that it includes only that taxon.

diplo_data_gamaff <- gam_aff_data %>%
  filter(psite_spp == "trem.dips")
levels(as.factor(diplo_data_gamaff$psite_spp))
view(diplo_data_gamaff)


# Now we want to sum up all three categories to get a total count of all diplostomatids (trem.diplo + trem.dlum +
# trem.em). Let's do that with a pipe:

#diplo_data <- diplo_data_separate %>%
 # group_by(IndividualFishID, CatalogNumber, YearCollected, DissectionDate, TotalLength_mm, CI, combo, Latitude, Longitude) %>%
#  summarize(diplo = sum(psite_count))
#view(diplo_data)


# Cool. Now we just need to merge in the bird data.

bird_data<-read.csv("reu/janell/bird_data_cleaned.csv")
view(bird_data)

hybama_plus_birds <- merge(diplo_data_hybama, bird_data, by.x = "YearCollected", by.y = "Year", all.x = TRUE)
head(hybama_plus_birds)
hybama_plus_birds$fish_spp<-"Hybognathus amarus"

gamaff_plus_birds <- merge(diplo_data_gamaff, bird_data, by.x = "YearCollected", by.y = "Year", all.x = TRUE)
head(gamaff_plus_birds)
gamaff_plus_birds$fish_spp<-"Gambusia affinis"

all_data <- rbind(hybama_plus_birds,gamaff_plus_birds)
view(all_data)


### PRELIMINARY ANALYSIS - CHELSEA, updated 27 JULY 2025

plot(all_data$psite_count~all_data$Sum.of.Number.Party.Hours)

model_1<-glmer.nb(psite_count~Sum.of.Number.Party.Hours+
                    offset(log(TotalLength_mm))+(1|fish_spp/psite_spp),data=all_data)
summary(model_1)

plot(all_data$psite_count~all_data$Sum.of.Number.Party.Hours)
plot(all_data$psite_count~all_data$YearCollected)
plot(all_data$Sum.of.Number.Party.Hours~all_data$YearCollected)


### FANCIER PLOTS


# Addressing Question 1: 

q1_plot<-ggplot(all_data,aes(YearCollected,Sum.of.Number.Party.Hours))+
  geom_point(size=4,pch=19)+
  xlab("year collected")+
  ylab("bird abundance (number of birds per party-hour)")+
  theme_minimal()+
  theme(plot.title=element_text(size=18,hjust=0.5,face="plain"),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color=NA),
        panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #annotate("text",label="effect of year:\np < 0.0001",x = 1.9, y = 0.45, size = 6)+
  theme(legend.position="top",legend.title = element_text(size = 18),
        legend.text = element_text(size=14))
q1_plot


# Addressing Question 2: 

library(viridis)
plasma_pal <- c(viridis::plasma(n = 4))
pal<-viridis(n=4)

q2_plot<-ggplot(all_data,aes(jitter(YearCollected,5),psite_count))+
  geom_point(aes(group=psite_spp,color=psite_spp),size=4,pch=19)+
  scale_color_manual(name = c("parasite taxonomic group"), values=plasma_pal, 
                     limits = c("trem.diplo","trem.dlum","trem.em","trem.dips"))+
  xlab("year collected")+
  ylab("parasite abundance (number of parasite individuals per host individual)")+
  theme_minimal()+
  theme(plot.title=element_text(size=18,hjust=0.5,face="plain"),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color=NA),
        panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #annotate("text",label="effect of year:\np < 0.0001",x = 1.9, y = 0.45, size = 6)+
  theme(legend.position="top",legend.title = element_text(size = 18),
        legend.text = element_text(size=14))
q2_plot


# Addressing Question 3: 

q3_plot<-ggplot(all_data,aes(jitter(Sum.of.Number.Party.Hours,10),psite_count))+
  geom_point(aes(group=psite_spp,color=psite_spp),size=4,pch=19)+
  scale_color_manual(name = c("parasite taxonomic group"), values=plasma_pal, 
                     limits = c("trem.diplo","trem.dlum","trem.em","trem.dips"))+
  xlab("year collected")+
  ylab("parasite abundance (number of parasite individuals per host individual)")+
  theme_minimal()+
  theme(plot.title=element_text(size=18,hjust=0.5,face="plain"),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color=NA),
        panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #annotate("text",label="effect of year:\np < 0.0001",x = 1.9, y = 0.45, size = 6)+
  theme(legend.position="top",legend.title = element_text(size = 18),
        legend.text = element_text(size=14))
q3_plot

