# UNM_MSB Parasites map and plot
# Creating a map and plot just like the one in the CAREER proposal.

library(maps)
library(mapdata)
library(maptools) #for shapefiles
library(scales) #for transparency
library(rgdal)
library(maps)
library(mapdata)
library(maptools) #for shapefiles
library(scales) #for transparency
library(ggmap)
library(ggsn)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggmap)
library(cowplot)
library(sp)
library(rgeos)
library(raster)
library(maptools)
library(rgdal)


lots<-read.csv("data/final_lots_CAREER.csv")
str(lots)

polygon<-readOGR("mapping/data/Albuquerque_City_Limits-polygon.shp")
polygon_df<-fortify(polygon)

water<-readOGR("mapping/data/NHDArea.shp")
water_df<-fortify(water)

bounds<-c(left=-106.89, bottom=34.82, right=-106.45, top=35.66)
map<-get_stadiamap(bounds, zoom=11, maptype = "stamen_terrain_background") %>% ggmap()+
  geom_point()+
  xlab("Longitude (°W)")+
  ylab("Latitude (°N)")+
  theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text=element_text(size=14),axis.title=element_text(size=14))+
  geom_rect(xmin=-106.89,ymin=34.8,xmax=-106.45,ymax=35.2102673,fill="grey",alpha=0.2,linetype="blank")+
  geom_hline(yintercept=35.2102673,linetype="dashed")+
  geom_map(data=water_df,map=water_df,aes(x=long,y=lat,map_id=id),color="lightsteelblue3",fill="lightsteelblue3")+
  geom_map(data=polygon_df,map=polygon_df,aes(x=long,y=lat,map_id=id),color="black",fill=NA)+
  annotate("text",x=-106.6,y=35.0975,label="City of Albuquerque",size=4.5)+
  annotate("text",x=-106.58,y=35.33,label="Rio Grande River",size=5,angle=55)
map


plot<-ggplot(data=lots,aes(x=YearCollected,y=Latitude))+
  annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 35.2102673,
           fill = "grey", colour = NA, alpha = 0.5)+
  annotate(geom = "rect", xmin = 1952.75, xmax = 1956.25, ymin = -Inf, ymax = Inf,
           fill = "darkgrey", colour = NA, alpha = 0.75)+
  #annotate(geom = "rect", xmin = 1999.75, xmax = 2005.25, ymin = -Inf, ymax = Inf,
  #fill = "darkgrey", colour = NA, alpha = 0.75)+
  #geom_rect(xmin=-Inf,ymin=-Inf,xmax=Inf,ymax=35.2102673,fill=adjustcolor("grey",alpha=0.02),linetype="blank")+
  geom_point(data=lots,aes(x=jitter(YearCollected,factor=40),y=jitter(Latitude,factor=40),fill=ScientificName,size=number_requested)
             ,shape=21)+
  scale_fill_manual(limits=c("Hybognathus amarus","Pimephales promelas","Gambusia affinis"),labels=c("HA","PP","GA"),
                    values=c("#3B9AB2","#E1AF00","#F21A00"),name="Host spp.")+
  scale_size_continuous(limits=c(1,15),name="Number of host individuals")+
  geom_hline(yintercept=35.2102673,linetype="dashed")+
  xlab("Year collected")+
  ylab("")+
  ylim(34.82,35.66)+
  xlim(1929,2011)+
  theme_bw()+
  theme(legend.position = "top",legend.title = element_text(size = 20, face = "bold"))+
  guides(fill = guide_legend(override.aes = list(size=7)))+
  theme(plot.margin = unit(c(0,0,0,0), "cm"), text=element_text(family='sans',size=20),
        axis.text.y = element_text(size=14),axis.text.x = element_text(size=14),legend.text = element_text(size=18))
plot

panels_cd<-plot_grid(map,plot,align="h",axis="bt",rel_widths = c(1,3))


final_figure <- ggdraw(plot=NULL,xlim=c(0,20),ylim=c(0,10))+
  #draw_image(panel_a,x=0,y=10,width=10,height=10)+
  #draw_image(panel_b,x=10,y=10,width=10,height=10)+
  draw_plot(panels_cd,x=0,y=0,width=20,height=10)+
  draw_label("(a)",x=0.5,y=9.75,size=30)+
  draw_label("(b)",x=5.5,y=9.75,size=30)
final_figure



