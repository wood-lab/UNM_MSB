# Tallying up specimens
# Counting and keeping track of the number of specimens

library(tidyverse)

# Read in the list of "in-bounds" specimens - these are specimens that have >10 individuals.

lots<-read.csv("data/final_lots_CAREER.csv")
str(lots)

sum(lots$number_requested)

thing <- lots %>%
  group_by(ScientificName) %>%
  summarize(count = n())

# Okay, we obviously can't dissect 7,000 fish this summer. We'll need to thin this out, using the same
# approach as we took for the TUBRI fish.

# Our goal is to look for differences in the trajectory of parasite change over time, above ("control")
# and below ("impact") the North Diversion Channel

lots$CI<-vector("character",length(lots$InstitutionCode))

for(i in 1:length(lots$InstitutionCode)) {
  
  if(is.na(lots$Latitude[i])){
    lots$CI[i] <- NA
  } else {
    
    if(lots$Latitude[i] > 35.211106){
      lots$CI[i] <- "control"
      
    } else {
      
      lots$CI[i] <- "impact"
      
    }
  }
}
lots$CI


# Just make sure everything looks okay and is making sense

#install.packages("devtools")
#devtools::install_github("stadiamaps/ggmap") 
library("ggmap")
#register_stadiamaps("45588e55-a703-4f6e-9ef0-ee6ed672b73a", write = TRUE)

bounds<-c(left=-106.89, bottom=34.82, right=-106.35, top=35.66)
map_check<-get_stadiamap(bounds, zoom=11, maptype = "stamen_terrain_background") %>% ggmap()+
  geom_point(data = lots, aes(x=Longitude,y=Latitude,fill=CI),shape=21)+
  scale_fill_manual(values=c("white","#bdbdbd"))+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
map_check


# A few points on a tributary and a reservoir or lake far in the north. Get rid of them.

lots<-lots %>%
  filter(Longitude < -106.35)

map_check<-get_stadiamap(bounds, zoom=11, maptype = "stamen_terrain_background") %>% ggmap()+
  geom_point(data = lots, aes(x=Longitude,y=Latitude,fill=CI),shape=21)+
  scale_fill_manual(values=c("white","#bdbdbd"))+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
map_check


# How many of each species are there in this stretch of the Rio Grande?

total_counts<-lots %>%
  group_by(ScientificName) %>%
  summarize(total_available = sum(IndividualCount,na.rm = T))
total_counts

levels(as.factor(lots$ScientificName))


# Create a separate dataset for each of the fish species.

gam_aff<-lots %>%
  filter(ScientificName =="Gambusia affinis")

hyb_ama<-lots %>%
  filter(ScientificName =="Hybognathus amarus")

pim_pro<-lots %>%
  filter(ScientificName =="Pimephales promelas")


# Make sure everything looks good and exclude species / latitudes where you don't have before and after data for
# every impact category or site.

# GAM_AFF

plot(jitter(gam_aff$Latitude,10)~gam_aff$YearCollected)+abline(a = 35.211106, b = 0, lty = 2)+abline(v = 1953, lty = 2)


# HYB_AMB

plot(jitter(hyb_ama$Latitude,10)~hyb_ama$YearCollected)+abline(a = 35.211106, b = 0, lty = 2)+abline(v = 1953, lty = 2)

#Need to cut off high latitudes, which don't extend across the entire time series

hyb_ama<-hyb_ama %>%
  filter(Latitude < 35.4)

plot(jitter(hyb_ama$Latitude,10)~hyb_ama$YearCollected)+abline(a = 35.211106, b = 0, lty = 2)+abline(v = 1953, lty = 2)


# PIM_PRO

plot(jitter(pim_pro$Latitude,10)~pim_pro$YearCollected)+abline(a = 35.211106, b = 0, lty = 2)+abline(v = 1953, lty = 2)

#Need to cut off high latitudes, which don't extend across the entire time series

pim_pro<-pim_pro %>%
  filter(Latitude < 35.4)

plot(jitter(pim_pro$Latitude,10)~pim_pro$YearCollected)+abline(a = 35.211106, b = 0, lty = 2)+abline(v = 1953, lty = 2)


#Okay, now we've trimmed each species to the appropriate latitudes and cut out some species. Re-create the
#dataset.

lots<-rbind.data.frame(gam_aff,hyb_ama,pim_pro)


# Create a new column to indicate what decade you're in

lots$decade<-vector("character",length(lots$InstitutionCode))


# Now loop to populate the decade column for the first mill

for(i in 1:length(lots$InstitutionCode)) {
  
  if(is.na(lots$YearCollected[i])){
    lots$decade[i] <- NA
    
  } else {
    
    if(lots$YearCollected[i] <1941 & lots$YearCollected[i] > 1930){
      lots$decade[i] <- "1931-1940"
      
    } else {
    
    if(lots$YearCollected[i] <1951 & lots$YearCollected[i] > 1940){
      lots$decade[i] <- "1941-1950"
      
    } else {
    
    if(lots$YearCollected[i] <1961 & lots$YearCollected[i] > 1950){
      lots$decade[i] <- "1951-1960"
      
    } else {
        
        if(lots$YearCollected[i] <1971 & lots$YearCollected[i] > 1960){
          lots$decade[i] <- "1961-1970"
          
        } else {
          
          if(lots$YearCollected[i] <1981 & lots$YearCollected[i] > 1970){
            lots$decade[i] <- "1971-1980"
            
          } else {
            
            if(lots$YearCollected[i] <1991 & lots$YearCollected[i] > 1980){
              lots$decade[i] <- "1981-1990"
              
            } else {
              
              if(lots$YearCollected[i] <2001 & lots$YearCollected[i] > 1990){
                lots$decade[i] <- "1991-2000"
                
              } else {
                
                if(lots$YearCollected[i] <2011 & lots$YearCollected[i] > 2000){
                  lots$decade[i] <- "2001-2010"
                  
                } else {
                  
                  if(lots$YearCollected[i] <2021 & lots$YearCollected[i] > 2010){
                    lots$decade[i] <- "2011-2020"
                    
                  } else {
                    
                    if(lots$YearCollected[i] <2031 & lots$YearCollected[i] > 2020){
                      lots$decade[i] <- "2021-2030"
                      
                    }}}}}}}}}}}}

lots$decade


### Export this dataset - it contains all of the lots that it would be okay to sample from.

all_lots_okay_to_dissect<-lots
write.csv(all_lots_okay_to_dissect,"data/all_lots_okay_to_dissect.csv")


# GAM_AFF
gam_aff<-lots %>%
  filter(ScientificName =="Gambusia affinis")

gam_aff_matrix<-gam_aff %>%
  group_by(CI, decade) %>%
  summarize(total_available = n())

gam_aff$combo<-paste(gam_aff$CI,gam_aff$decade,sep="_")

# Selecting a maximum of 5 fish individuals from each of 10 lots from each CI*decade combo

control_1941to1950<-gam_aff[which(gam_aff$combo == "control_1941-1950"), ]
control_1951to1960<-gam_aff[which( gam_aff$combo == "control_1951-1960"), ]
control_1961to1970<-gam_aff[which( gam_aff$combo == "control_1961-1970"), ]
control_1971to1980<-gam_aff[which( gam_aff$combo == "control_1971-1980"), ]
control_1981to1990<-gam_aff[which( gam_aff$combo == "control_1981-1990"), ]
control_1991to2000<-gam_aff[ sample( which( gam_aff$combo == "control_1991-2000"), 10, replace = F), ]
control_2001to2010<-gam_aff[which( gam_aff$combo == "control_2001-2010"), ]
impact_1931to1940<-gam_aff[ sample( which( gam_aff$combo == "impact_1931-1940"), 10, replace = F), ]
impact_1941to1950<-gam_aff[which( gam_aff$combo == "impact_1941-1950"), ]
impact_1951to1960<-gam_aff[which( gam_aff$combo == "impact_1951-1960"), ]
impact_1961to1970<-gam_aff[which( gam_aff$combo == "impact_1961-1970"), ]
impact_1971to1980<-gam_aff[which( gam_aff$combo == "impact_1971-1980"), ]
impact_1981to1990<-gam_aff[ sample( which( gam_aff$combo == "impact_1981-1990"), 10, replace = F), ]
impact_1991to2000<-gam_aff[ sample( which( gam_aff$combo == "impact_1991-2000"), 10, replace = F), ]


gam_aff_selected<-rbind.data.frame(control_1941to1950,control_1951to1960,control_1961to1970,
                                   control_1971to1980,control_1981to1990,control_1991to2000,
                                   control_2001to2010,impact_1931to1940,impact_1941to1950,
                                   impact_1951to1960,impact_1961to1970,impact_1971to1980,
                                   impact_1981to1990,impact_1991to2000)

gam_aff_matrix<-gam_aff_selected %>%
  group_by(combo) %>%
  summarize(total_request = n())

plot(jitter(gam_aff_selected$Latitude,5)~gam_aff_selected$YearCollected)+abline(a = 35.211106, b = 0, lty = 2)+abline(v = 1953, lty = 2)

write.csv(gam_aff_matrix,file="data/gam_aff_goal.csv")



# HYB_AMA
hyb_ama<-lots %>%
  filter(ScientificName =="Hybognathus amarus")

hyb_ama_matrix<-hyb_ama %>%
  group_by(CI, decade) %>%
  summarize(total_available = n())

# That one NA is real - no date recorded.

hyb_ama$combo<-paste(hyb_ama$CI,hyb_ama$decade,sep="_")

# Selecting a maximum of 5 fish individuals from each of 10 lots from each CI*decade combo

control_1941to1950<-hyb_ama[which(hyb_ama$combo == "control_1941-1950"), ]
control_1951to1960<-hyb_ama[which( hyb_ama$combo == "control_1951-1960"), ]
#control_1961to1970<-hyb_ama[which( hyb_ama$combo == "control_1961-1970"), ]
control_1971to1980<-hyb_ama[which( hyb_ama$combo == "control_1971-1980"), ]
control_1981to1990<-hyb_ama[which( hyb_ama$combo == "control_1981-1990"), ]
control_1991to2000<-hyb_ama[ sample( which( hyb_ama$combo == "control_1991-2000"), 10, replace = F), ]
#control_2001to2010<-hyb_ama[which( hyb_ama$combo == "control_2001-2010"), ]
impact_1931to1940<-hyb_ama[which( hyb_ama$combo == "impact_1931-1940"), ]
impact_1941to1950<-hyb_ama[which( hyb_ama$combo == "impact_1941-1950"), ]
impact_1951to1960<-hyb_ama[which( hyb_ama$combo == "impact_1951-1960"), ]
impact_1961to1970<-hyb_ama[which( hyb_ama$combo == "impact_1961-1970"), ]
impact_1971to1980<-hyb_ama[which( hyb_ama$combo == "impact_1971-1980"), ]
impact_1981to1990<-hyb_ama[which( hyb_ama$combo == "impact_1981-1990"), ]
impact_1991to2000<-hyb_ama[ sample( which( hyb_ama$combo == "impact_1991-2000"), 10, replace = F), ]
impact_2001to2010<-hyb_ama[which( hyb_ama$combo == "impact_2001-2010"), ]



hyb_ama_selected<-rbind.data.frame(control_1941to1950,control_1951to1960,#control_1961to1970,
                                   control_1971to1980,control_1981to1990,control_1991to2000,
                                   #control_2001to2010,
                                   impact_1931to1940,impact_1941to1950,
                                   impact_1951to1960,impact_1961to1970,impact_1971to1980,
                                   impact_1981to1990,impact_1991to2000,impact_2001to2010)

hyb_ama_matrix<-hyb_ama_selected %>%
  group_by(combo) %>%
  summarize(total_request = n())

plot(jitter(hyb_ama_selected$Latitude,5)~hyb_ama_selected$YearCollected)+abline(a = 35.211106, b = 0, lty = 2)+abline(v = 1953, lty = 2)

write.csv(gam_aff_matrix,file="data/hyb_ama_goal.csv")



# PIM_PRO
pim_pro<-lots %>%
  filter(ScientificName =="Pimephales promelas")

pim_pro_matrix<-pim_pro %>%
  group_by(CI, decade) %>%
  summarize(total_available = n())

pim_pro$combo<-paste(pim_pro$CI,pim_pro$decade,sep="_")

# Selecting a maximum of 5 fish individuals from each of 10 lots from each CI*decade combo

control_1941to1950<-pim_pro[which(pim_pro$combo == "control_1941-1950"), ]
control_1951to1960<-pim_pro[which( pim_pro$combo == "control_1951-1960"), ]
#control_1961to1970<-pim_pro[which( pim_pro$combo == "control_1961-1970"), ]
control_1971to1980<-pim_pro[which( pim_pro$combo == "control_1971-1980"), ]
control_1981to1990<-pim_pro[ sample( which( pim_pro$combo == "control_1981-1990"), 10, replace = F), ]
control_1991to2000<-pim_pro[ sample( which( pim_pro$combo == "control_1991-2000"), 10, replace = F), ]
control_2001to2010<-pim_pro[which( pim_pro$combo == "control_2001-2010"), ]
impact_1931to1940<-pim_pro[which( pim_pro$combo == "impact_1931-1940"), ]
impact_1941to1950<-pim_pro[which( pim_pro$combo == "impact_1941-1950"), ]
impact_1951to1960<-pim_pro[which( pim_pro$combo == "impact_1951-1960"), ]
impact_1961to1970<-pim_pro[which( pim_pro$combo == "impact_1961-1970"), ]
impact_1971to1980<-pim_pro[ sample( which( pim_pro$combo == "impact_1971-1980"), 10, replace = F), ]
impact_1981to1990<-pim_pro[ sample( which( pim_pro$combo == "impact_1981-1990"), 10, replace = F), ]
impact_1991to2000<-pim_pro[ sample( which( pim_pro$combo == "impact_1991-2000"), 10, replace = F), ]
impact_2011to2020<-pim_pro[which( pim_pro$combo == "impact_2011-2020"), ]



pim_pro_selected<-rbind.data.frame(control_1941to1950,control_1951to1960,#control_1961to1970,
                                   control_1971to1980,control_1981to1990,control_1991to2000,
                                   control_2001to2010,
                                   impact_1931to1940,impact_1941to1950,
                                   impact_1951to1960,impact_1961to1970,impact_1971to1980,
                                   impact_1981to1990,impact_1991to2000,impact_2011to2020)

pim_pro_matrix<-pim_pro_selected %>%
  group_by(combo) %>%
  summarize(total_request = n())

plot(jitter(pim_pro_selected$Latitude,5)~pim_pro_selected$YearCollected)+abline(a = 35.211106, b = 0, lty = 2)+abline(v = 1953, lty = 2)

write.csv(gam_aff_matrix,file="data/pim_pro_goal.csv")
 

