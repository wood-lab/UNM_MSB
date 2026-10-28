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
library(maps)
library(mapdata)
library(maptools) #for shapefiles
library(scales) #for transparency
library(rgdal)
library(maps)
library(mapdata)
library(scales) #for transparency
library(ggmap)
library(ggsnap)
library(tidyverse)
library(tidyr)
library(dplyr)
library(cowplot)
library(sp)
library(raster)


# Read in the data

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



# Start the MS with just a plot of the raw data: parasite counts, fish_spp as series, divided into control and impact.
# This will show that the decline through time is visible with the naked eye, and driven primarily by HYBAMA.
# It will also show that the story doesn't differ that much between control and impact, justifying the turn from
# BA*CI to BA*fish_spp.

plot(final_dataset$psite_count~final_dataset$YearCollected)

control_only<-final_dataset %>%
  filter(CI=="control")

impact_only<-final_dataset %>%
  filter(CI=="impact")

plot(control_only$psite_count~control_only$YearCollected)

plot(impact_only$psite_count~impact_only$YearCollected)


# Weird that there are a handful of very late specimens that don't show up in control or impact...

recents<-final_dataset %>%
  filter(YearCollected>2010)
View(recents)


# Ah, the more recent specimens that Tom and Emily collected for us are missing lat/long, and are therefore missing
# CI classification. I can extract lats and longs from GBIF.

final_dataset <- final_dataset %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124139",35.029588,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124139",-106.672074,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124141",35.09061,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124141",-106.683663,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124143",35.323975,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124143",-106.556611,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124144",35.029588,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124144",-106.672074,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124146",35.09061,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124146",-106.683663,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124148",35.323975,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124148",-106.556611,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124149",35.323975,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124149",-106.556611,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124152",35.09061,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124152",-106.683663,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124153",35.323975,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124153",-106.556611,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124155",35.029588,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124155",-106.672074,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124157",35.09061,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124157",-106.683663,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124158",35.323975,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124158",-106.556611,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124159",35.323975,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124159",-106.556611,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124161",35.029588,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124161",-106.672074,Longitude))) %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:124163",35.029588,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:124163",-106.672074,Longitude)))

recents<-final_dataset %>%
  filter(YearCollected>2010)
View(recents)


# While we're at it, let's just check to make sure there aren't any missing data.

thing<-final_dataset %>%
  filter(is.na(YearCollected))
thing

# Every entry has a valid year. Now check latitude.

thing<-final_dataset %>%
  filter(is.na(Latitude))
thing

# Okay, a handful still missing latitude. Let's check on them in GBIF and fill in the blanks.

# MSB:Fish:438 - looks like this is incorrectly written - should be MSB:Fish:4389

final_dataset <- final_dataset %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:438",35.092867,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:438",-106.682332,Longitude))) %>%
  mutate(CatalogNumber = (ifelse(CatalogNumber == "MSB:Fish:438","MSB:Fish:4389",CatalogNumber)))
  
thing<-final_dataset %>%
  filter(CatalogNumber == "MSB:Fish:438")

thing<-final_dataset %>%
  filter(CatalogNumber == "MSB:Fish:4389")

thing<-final_dataset %>%
  filter(is.na(Latitude))

# MBS:Fish:1069 needs to be MSB:Fish:1069

final_dataset <- final_dataset %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MBS:Fish:1069",35.051774,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MBS:Fish:1069",-106.671997,Longitude))) %>%
  mutate(CatalogNumber = (ifelse(CatalogNumber == "MBS:Fish:1069","MSB:Fish:1069",CatalogNumber)))

thing<-final_dataset %>%
  filter(CatalogNumber == "MBS:Fish:1069")

thing<-final_dataset %>%
  filter(CatalogNumber == "MSB:Fish:1069")

thing<-final_dataset %>%
  filter(is.na(Latitude))

# MSB:Fish:7832-2 needs to be MSB:Fish:7832

final_dataset <- final_dataset %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:7832-2",35.283028,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:7832-2",-106.598139,Longitude))) %>%
  mutate(CatalogNumber = (ifelse(CatalogNumber == "MSB:Fish:7832-2","MSB:Fish:7832",CatalogNumber)))

thing<-final_dataset %>%
  filter(CatalogNumber == "MSB:Fish:7832-2")

thing<-final_dataset %>%
  filter(CatalogNumber == "MSB:Fish:7832")

thing<-final_dataset %>%
  filter(is.na(Latitude))

# MSB:Fish:7835 - this number is wrong (corresponds to different fish species in GBIF)
# I'm confident that this was mistyped - MSB:Fish:7832 was collected on the 18th in 1990 and is missing the fish
# that should be MSB:Fish:7832-5. I checked the original datasheets and the original dataset.

final_dataset <- final_dataset %>%
  mutate(Latitude = (ifelse(CatalogNumber == "MSB:Fish:7835",35.283028,Latitude))) %>%
  mutate(Longitude = (ifelse(CatalogNumber == "MSB:Fish:7835",-106.598139,Longitude))) %>%
  mutate(CatalogNumber = (ifelse(CatalogNumber == "MSB:Fish:7835","MSB:Fish:7832",CatalogNumber)))

thing<-final_dataset %>%
  filter(CatalogNumber == "MSB:Fish:7835")

thing<-final_dataset %>%
  filter(CatalogNumber == "MSB:Fish:7832")

thing<-final_dataset %>%
  filter(is.na(Latitude))


# Okay, now get rid of existing CI and combo and reassign

final_dataset$CI<-NULL
final_dataset$combo<-NULL

final_dataset$CI<-vector("character",length(final_dataset$CatalogNumber))

for(i in 1:length(final_dataset$CatalogNumber)) {
  
  if(is.na(final_dataset$Latitude[i])){
    final_dataset$CI[i] <- NA
  } else {
    
    if(final_dataset$Latitude[i] > 35.211106){
      final_dataset$CI[i] <- "control"
      
    } else {
      
      final_dataset$CI[i] <- "impact"
      
    }
  }
}
final_dataset$CI

thing<-final_dataset %>%
  filter(is.na(CI))


# Then make a map, make sure everything is kosher

min(final_dataset$Latitude)
max(final_dataset$Latitude)

lat_limits <- c(34.74, 35.5)

polygon<-readOGR("mapping/data/Albuquerque_City_Limits-polygon.shp")
polygon_df<-fortify(polygon)

water<-readOGR("mapping/data/NHDArea.shp")
water_df<-fortify(water)

bounds<-c(left=-106.89, bottom=34.74, right=-106.45, top=35.5)
map<-get_stadiamap(bounds, zoom=11, maptype = "stamen_terrain_background") %>% ggmap()+
  geom_point()+
  xlab("Longitude (°W)")+
  ylab("Latitude (°N)")+
  theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text=element_text(size=14),axis.title=element_text(size=14))+
  geom_rect(xmin=-106.89,ymin=34.74,xmax=-106.45,ymax=35.2102673,fill="grey",alpha=0.2,linetype="blank")+
  geom_hline(yintercept=35.2102673,linetype="dashed")+
  geom_map(data=water_df,map=water_df,aes(x=long,y=lat,map_id=id),color="lightsteelblue3",fill="lightsteelblue3")+
  geom_map(data=polygon_df,map=polygon_df,aes(x=long,y=lat,map_id=id),color="black",fill=NA)+
  coord_cartesian(ylim = lat_limits, expand = FALSE)+
  annotate("text",x=-106.6,y=35.0975,label="City of Albuquerque",size=4.5)+
  annotate("text",x=-106.58,y=35.33,label="Rio Grande River",size=5,angle=55)
map

# Just check to make sure everything is on the river where it belongs
# NEXT STEP: Investigate the points that are not on the river and exclude any that aren't in Rio Grande proper
# Color these points by fish species
# Put the fish species legend on top
# Put the map with the points into the panel

map<-get_stadiamap(bounds, zoom=11, maptype = "stamen_terrain_background") %>% ggmap()+
  xlab("Longitude (°W)")+
  ylab("Latitude (°N)")+
  theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text=element_text(size=14),axis.title=element_text(size=14))+
  geom_rect(xmin=-106.89,ymin=34.74,xmax=-106.45,ymax=35.2102673,fill="grey",alpha=0.2,linetype="blank")+
  geom_hline(yintercept=35.2102673,linetype="dashed")+
  geom_map(data=water_df,map=water_df,aes(x=long,y=lat,map_id=id),color="lightsteelblue3",fill="lightsteelblue3")+
  geom_map(data=polygon_df,map=polygon_df,aes(x=long,y=lat,map_id=id),color="black",fill=NA)+
  annotate("text",x=-106.6,y=35.0975,label="City of Albuquerque",size=4.5)+
  annotate("text",x=-106.58,y=35.33,label="Rio Grande River",size=5,angle=55)+
  geom_point(data = lots, aes(x=Longitude,y=Latitude))
map


# coalesce all of the data into just fish individuals

lots <- final_dataset %>%
  group_by(IndividualFishID,Latitude,Longitude,YearCollected,fish_spp) %>%
  summarize(count = n())

# make the plot
library(wesanderson)
wes_palette("Zissou1")
pal<-wes_palette(name = "Zissou1", 3, type = "continuous")

big_plot<-ggplot(lots,aes(jitter(YearCollected, amount = 0.5),jitter(Latitude, amount = 0.1)))+
  geom_point(aes(group=fish_spp,color=fish_spp),size=2,pch=19)+
  scale_color_manual(values=pal,name="Host spp.")+
  scale_y_continuous(limits = lat_limits,
                     expand = expansion(mult = 0))+
  xlab("year")+
  ylab("")+
  geom_hline(yintercept = 35.211106, linetype = "dashed")+
  theme_minimal()+
  #labs(linetype="parasite life history strategy")+
  theme(axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length.y = unit(0.1, "cm"),
        plot.title=element_text(size=18,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=18,color="black"),axis.title.x=element_text(size=16),
        panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color=NA),
        panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  theme(legend.position="none")
big_plot

# Align the y-axes of the map and the plot

panels_cd <- cowplot::plot_grid(
  map, big_plot,
  align = "v",
  axis = "lr",
  rel_widths = c(1, 3)
)


# Put the map and the plot together

final_figure <- ggdraw(plot=NULL,xlim=c(0,30),ylim=c(0,10))+
  draw_plot(panels_cd,x=0,y=0,width=10,height=10)+
  draw_label("(a)",x=0.5,y=9.75,size=30)+
  draw_label("(b)",x=5.5,y=9.75,size=30)
final_figure




View(final_dataset)


# then make the temporal plots of psite change - just the raw data



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

model_draft_6<-glmer.nb(psite_count~
                          CI+YearCollected_sc*host_psite_combo+
                          fish_spp:TotalLength_mm_sc+
                          Latitude_within+
                          (1|CatalogNumber_chr),
                        data=final_dataset,
                        family="nbinom",
                        control = glmerControl(
                          optimizer = "bobyqa",
                          optCtrl = list(maxfun = 2e5)))

summary(model_draft_6)


big_predictions<-ggeffect(model_draft_6,c("YearCollected_sc","host_psite_combo"))
str(big_predictions)

diverging_pal <- c("#5ab4ac","#d8b365")
darker_pal <- c("#018571","#a6611a")

big_plot<-ggplot(big_predictions,aes(x,predicted),group=group,color=group)+
  geom_ribbon(data=big_predictions,mapping=aes(x=x,ymin=conf.low,ymax=conf.high,fill=group),alpha=0.5)+
  geom_line(aes(group=group,color=group))+
  #scale_color_manual(values=darker_pal)+
  #scale_fill_manual(values=diverging_pal)+
  #facet_grid(~facet)+
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


# Okay, it looks like a few HYBAMA parasites have just collapsed in abundance. Can we just look at those raw data?


# Then I think you want to look specifically into each of the HYBAMA parasites and what their temporal trajectory was.

HYBAMA_MONO_DACT <- final_dataset %>%
  filter(fish_spp == "Hybognathus amarus" & psite_spp == "mono.dact")

plot(HYBAMA_MONO_DACT$psite_count~HYBAMA_MONO_DACT$YearCollected)

# Not seen since 1960. Neat.

HYBAMA_TREM.B <- final_dataset %>%
  filter(fish_spp == "Hybognathus amarus" & psite_spp == "trem.b")

plot(HYBAMA_TREM.B$psite_count~HYBAMA_TREM.B$YearCollected)


HYBAMA_TREM.DIPLO <- final_dataset %>%
  filter(fish_spp == "Hybognathus amarus" & psite_spp == "trem.diplo")

plot(HYBAMA_TREM.DIPLO$psite_count~HYBAMA_TREM.DIPLO$YearCollected)


# Still around, but lower abundance.



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


# Show me what is happening with each individual parasite species, colored by fish species.

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

fish_density<-read.csv("data/RGSM_Albuquerque_core_station_species_1993_2019.csv",header=T,sep=",")

# Get an average density per year

fish_data <- fish_density %>%
  group_by(ReportingYear, Species) %>%
  summarize(mean_density = mean(Density_per_100m2))

fish_plot<-ggplot(fish_data,aes(ReportingYear,mean_density,group=Species))+
  geom_point(aes(group=Species,color=Species))

fish_model<-lm(mean_density~ReportingYear*Species, data=fish_data)
summary(fish_model)


# Now merge by year and fish species
# Start by creating a variable that will allow you to merge across datasets

fish_data$year_host <- paste(fish_data$ReportingYear, fish_data$Species, sep = "_")

final_dataset$year_host <- paste(final_dataset$YearCollected, final_dataset$fish_spp, sep = "_")

final_w_fish <- merge(final_dataset, fish_data, by = "year_host", all = TRUE)

View(final_w_fish)

# But also you want to add a one-year lag, right?

lag_data <- data.frame(
  YearCollected = final_w_fish$YearCollected,
  lagged_density = final_w_fish$mean_density
)

lag_data$one_year_lagged <- (lag_data$YearCollected-1)

lag_data$year_host <- paste(lag_data$one_year_lagged, lag_data$fish_spp, sep = "_")

final_w_fish_lagged <- merge(final_w_fish, lag_data, by = "year_host", all = TRUE)

View(final_w_fish_lagged)

#... Okay this isn't right at the moment, but let's leave it aside for now.


# What about looking at water temperature?

temp_data <- read.csv("data/Rio_Grande_Albuquerque_temperature_annual_site_summary.csv", header = T, sep = ",")

temp_data_final <- temp_data %>%
  group_by(year) %>%
  summarize(mean_temp = mean(annual_mean_temp_C))

plot(temp_data_final$mean_temp~temp_data_final$year)

# that 1949 point is super weird, get rid of it.

temp_data_final <- temp_data_final %>%
  filter(year != 1949)

final_w_temp <- merge(final_dataset, temp_data_final, by.x = "YearCollected", by.y = "year", all = TRUE)

temp_plot<-ggplot(final_w_temp,aes(mean_temp,psite_count,group=fish_spp))+
  geom_point(aes(group=fish_spp,color=fish_spp))

temp_model<-lm(psite_count~mean_temp*fish_spp, data=final_w_temp)
summary(temp_model)

temp_model<-glmer.nb(psite_count~mean_temp*fish_spp+
                          host_psite_combo*TotalLength_mm_sc+
                          Latitude_within+
                          (1|CatalogNumber_chr),
                        data=final_w_temp,
                        family="nbinom",
                        control = glmerControl(
                          optimizer = "bobyqa",
                          optCtrl = list(maxfun = 2e5)))

summary(temp_model)

temp_model_2<-glmer.nb(psite_count~mean_temp+fish_spp+
                       host_psite_combo*TotalLength_mm_sc+
                       Latitude_within+
                       (1|CatalogNumber_chr),
                     data=final_w_temp,
                     family="nbinom",
                     control = glmerControl(
                       optimizer = "bobyqa",
                       optCtrl = list(maxfun = 2e5)))

summary(temp_model_2)

temp_model_3<-glmer.nb(psite_count~mean_temp+host_psite_combo+
                         fish_spp*TotalLength_mm_sc+
                         Latitude_within+
                         (1|CatalogNumber_chr),
                       data=final_w_temp,
                       family="nbinom",
                       control = glmerControl(
                         optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)))

summary(temp_model_3)

temp_model_3<-glmer.nb(psite_count~mean_temp+host_psite_combo+
                         fish_spp*TotalLength_mm_sc+
                         Latitude_within+
                         (1|CatalogNumber_chr),
                       data=final_w_temp,
                       family="nbinom",
                       control = glmerControl(
                         optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)))

summary(temp_model_3)

temp_model_4<-glmer.nb(psite_count~mean_temp+fish_spp+fish_spp:host_psite_combo+
                         TotalLength_mm_sc+
                         Latitude_within+
                         (1|CatalogNumber_chr),
                       data=final_w_temp,
                       family="nbinom",
                       control = glmerControl(
                         optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)))

summary(temp_model_4)

temp_model_5<-glmer.nb(psite_count~mean_temp+fish_spp+fish_spp:host_psite_combo+
                         TotalLength_mm_sc+
                         #Latitude_within+
                         (1|CatalogNumber_chr),
                       data=final_w_temp,
                       family="nbinom",
                       control = glmerControl(
                         optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)))

summary(temp_model_5)

# Doesn't look like there's much to the temperature story



# The bigger question is, what's up with parasite diversity?  Is there a big difference between what happens with
# HYBAMA and the remaining species? Is that why there isn't a density relationship for HYBAMA, because some parasite
# species have been bottlenecked out of the host population?

### DIVERSITY ANALYSIS

levels(as.factor(minus_myxos$Parasite_taxonomic_group))

# You've already created a column of 0/1 above, all_data$positive. Now sum across those to obtain
# raw richness for each fish individual.

raw_richness_dataset <- all_data %>%
  group_by(IndividualFishID,CatalogNumber,YearCollected,TotalLength_mm,CI,Latitude,fish_spp) %>%
  summarize(raw_richness = sum(positive,na.rm=T))

View(raw_richness_dataset)

# Before you dive into analysis, just see what's up.

raw_richness_plot<-ggplot(raw_richness_dataset,aes(YearCollected,raw_richness),group=CI,color=CI)+
  #facet_wrap(vars(Fish_sp.x),nrow=2,ncol=4)+
  geom_point(aes(group=CI,color=CI),position=position_jitter(width=0.15),size=3,pch=19)+
  geom_smooth(aes(color=CI),method="lm")+
  scale_color_manual(values=c("cadetblue","burlywood4"))+
  xlab("year")+
  ylab("raw richness (number of parasite taxa per fish individual)")+
  theme_minimal()+
  #ylim(0,10)+
  #labs(linetype="parasite life history strategy")+
  theme(strip.text=element_text(size=20,face="italic"),
        plot.title=element_text(size=30,hjust=0.5,face="plain"),
        axis.text.y=element_text(size=20),
        axis.title.y=element_text(size=26),
        axis.text.x=element_text(size=20,color="black"),
        axis.title.x=element_text(size=26),
        panel.background=element_rect(fill="white",color="black"),
        panel.grid.minor=element_line(color=NA),panel.spacing = unit(1, "lines"))+
  #scale_x_discrete(limits=(rev(levels(raneff_predictions$x))))+
  theme(legend.position="top",legend.title = element_blank(),
        legend.text = element_text(size=20))
raw_richness_plot


hist(raw_richness_dataset$raw_richness)

# Now let's try analyzing. First scale all numerical values, including year. And make sure that
# CatalogNumber is getting treated as a categorical variable.

raw_richness_dataset$YearCollected_sc <- scale(raw_richness_dataset$YearCollected)
raw_richness_dataset$CatalogNumber_chr <- as.character(raw_richness_dataset$CatalogNumber)

model_draft_raw_richness<-glmer.nb(raw_richness~CI*YearCollected_sc+fish_spp*scale(TotalLength_mm)+
                                     +(1|CatalogNumber_chr)+(1|Latitude),
                                   data=raw_richness_dataset,family="nbinom")
summary(model_draft_raw_richness)


model_draft_raw_richness_2<-glmer.nb(raw_richness~CI*YearCollected_sc*fish_spp+fish_spp*scale(TotalLength_mm)+
                                       +(1|CatalogNumber_chr)+(1|Latitude),
                                   data=raw_richness_dataset,family="nbinom")
summary(model_draft_raw_richness_2)


model_draft_raw_richness_3<-glmer.nb(raw_richness~YearCollected_sc*fish_spp+fish_spp*scale(TotalLength_mm)+
                                       +(1|CatalogNumber_chr)+(1|Latitude),
                                     data=raw_richness_dataset,family="nbinom")
summary(model_draft_raw_richness_3)


# Now plot.

raw_richness_predictions<-ggeffect(model_draft_raw_richness_3,c("YearCollected_sc", "fish_spp"))

prediction_plot<-ggplot(raw_richness_predictions,aes(x,predicted),group=group,color=group)+
  geom_point(aes(group=group,color=group),size=4,pch=19,position = position_dodge(width = 0.5))+
  geom_errorbar(data=raw_richness_predictions,mapping=aes(x=x,ymin=conf.low,ymax=conf.high,group=group,color=group),
                width=0.04,position = position_dodge(width = 0.5))+
  geom_line(aes(group=group,color=group),position = position_dodge(width = 0.5))+
  scale_color_manual(values=c("cadetblue","burlywood4","black"))+
  xlab("")+
  ylab("predicted parasite taxon richness per host individual")+
  theme_minimal()+
  #labs(linetype="parasite life history strategy")+
  theme(plot.title=element_text(size=18,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=18,color="black"),axis.title.x=element_text(size=16),
        panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color=NA),
        panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=(rev(levels(big_predictions$x))))+
  theme(legend.position="top",legend.title = element_blank(),
        legend.text = element_text(size=12))
prediction_plot






levels(as.factor(final_dataset$psite_taxon))

fish_data


plot(final_w_fish$psite_count~final_w_fish$mean_density)

density_model<-lm(psite_count~mean_density*fish_spp, data=final_w_fish)
summary(density_model)

density_predictions<-ggpredict(density_model,c("mean_density","fish_spp"))
str(density_predictions)

diverging_pal <- c("#5ab4ac","#d8b365")
darker_pal <- c("#018571","#a6611a")

big_plot<-ggplot(density_predictions,aes(x,predicted,group=group))+
  #geom_ribbon(data=big_predictions,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),alpha=0.5)+
  geom_line(aes(group=group,color=group))+
  #scale_color_manual(values=darker_pal)+
  scale_fill_manual(values=diverging_pal)+
  #facet_grid(~facet)+
  #ylim(0,10)+
  xlab("host density")+
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


# Interesting... it looks like all fish have been caught at lower densities in later years. There is a positive
# relationship with density for GAMAFF and PIMPRO, but not so for HYBAMA. Could be time lags? Also, could be
# that, once HYBAMA has been through a population bottleneck, some parasites just drop out and are not recoverable.
# That would be detectable if we looked at the number of species infecting each.

# First, investigate time lags.

final_w_fish$density_lagged_one_year <- final_w_fish$YearCollected-1

density_model<-lm(psite_count~mean_density*fish_spp, data=final_w_fish)
summary(density_model)

density_predictions<-ggpredict(density_model,c("mean_density","fish_spp"))
str(density_predictions)

diverging_pal <- c("#5ab4ac","#d8b365")
darker_pal <- c("#018571","#a6611a")

big_plot<-ggplot(density_predictions,aes(x,predicted,group=group))+
  #geom_ribbon(data=big_predictions,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),alpha=0.5)+
  geom_line(aes(group=group,color=group))+
  #scale_color_manual(values=darker_pal)+
  scale_fill_manual(values=diverging_pal)+
  #facet_grid(~facet)+
  #ylim(0,10)+
  xlab("host density")+
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
