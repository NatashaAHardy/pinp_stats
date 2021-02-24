########### PINP Bird Detections ###########

#### Workspace ----
setwd("/Users/natasha/Documents/Professional/WORK/CURRENT JOBS/PINP/PINP DATA/pinp_stats/data")
getwd()

# Packages

#Install

#Load
library(tidyverse)
library(tidyr)
#library(reshape)
library(reshape2)
library(plyr)
library(dplyr)
library(PNWColors)


#### Load & manip data ----

## See Rmd file

#### Data visualisation ----

str(fig5.pal)

#### Figure 2a ----

#Overall bird detection by method: need sum of all samples containing bird by method of detection
#Also not working
bird_sum <- ddply(birdD_long, "metric", summarise,
                  detected = sum(value, na.rm=TRUE),
                  not.detected = 99 - sum(value, na.rm=TRUE))
str(bird_sum)

bird_detect <- melt(bird_sum, id.vars="metric")
str(bird_detect)
bird_detect = bird_detect %>% 
  dplyr::rename(detection = `variable`)

#Can double check this with table()
table(birdD_long$metric, birdD_long$value)

#Colour palette
fig2.pal <- pnw_palette(5, name = "Bay", type = "continuous")

#Figure 2
bird_fig2 <- ggplot(bird_detect, aes(x=metric, y=value, fill=metric)) +
  geom_bar(stat="identity",position="dodge")+
  theme_bw() + theme(panel.grid.major = element_blank()) +
  theme(axis.title=element_text(size=18)) + theme(axis.text=element_text(size=18)) +
  theme(axis.text.x=element_blank())+
  theme(legend.text=element_text(size=18)) + #theme(legend.title=element_text(size=14)) +
  theme(legend.position=c(0.2,0.8), legend.justification=c(0.4,0.5))+
  scale_fill_manual(values=fig2.pal)+
  guides(fill=guide_legend(title=NULL))+
  labs(x="Metric", y="Number of samples") +
  facet_grid(.~detection)+
  theme(strip.text = element_text(face="bold", size=rel(1.25)),
        strip.background = element_rect(fill="lightgrey", colour="black",
                                        size=1))
bird_fig2

ggsave('figure2_birddetect.jpeg', plot = bird_fig2, width = 7.5, height = 5, dpi = 300)

#### Figure 4 ----

#Bird detections by location & time (medium stringency DNA QC)

#Colour palette
fig4.pal <- pnw_palette(5, name = "Bay", type = "continuous")

#Figure 2
bird_fig4a <- ggplot(bird_group_med, aes(x=time_id, y=percentage, fill=metric)) +
  geom_bar(stat="identity",position="dodge")+
  #ggtitle("a) Standard DNA filtering")+
  theme_bw() + theme(panel.grid.major = element_blank()) +
  theme(plot.title=element_text(face="bold", size=18))+
  theme(axis.title=element_text(size=18)) + theme(axis.text=element_text(size=18)) +
  theme(axis.text.x=element_blank())+
  theme(legend.text=element_text(size=18)) + #theme(legend.title=element_text(size=14)) +
  theme(legend.position=c(0.2,0.75), legend.justification=c(0.4,0.5))+
  scale_fill_manual(values=fig2.pal)+
  guides(fill=guide_legend(title=NULL))+
  labs(title ="a) Standard DNA filtering", x="Metric", y="Percentage of samples (%)") +
  facet_grid(.~detection)+
  theme(strip.text = element_text(size=rel(1.25)), #face="bold", 
        strip.background = element_rect(fill="lightgrey", colour="black",
                                        size=1))
bird_fig2a

ggsave('figure2a_birddetectmed.jpeg', plot = bird_fig2a, width = 7.5, height = 5, dpi = 300)

#20200302
#Messing around with the figure to make it look like a similar excel version

fig4a_bird <- ggplot(bird_master_group, aes(x=time_id, y=detected, fill=metric)) +
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  #ggtitle("a) Bird detections by location & sampling time")+
  theme_bw() + theme(panel.grid.major = element_blank()) +
  theme(plot.title=element_text(face="bold", size=18))+
  theme(axis.title=element_text(size=18)) + theme(axis.text=element_text(size=18)) +
  #theme(axis.text.x=element_blank())+
  theme(legend.text=element_text(size=18)) + theme(legend.title=element_text(size=18)) +
  theme(legend.position="top")+
  #theme(legend.position=c(0.2,0.75), legend.justification=c(0.4,0.5))+
  scale_fill_manual(values=fig4.pal)+
  guides(fill=guide_legend(title="Metric:"))+
  labs(title ="a) Number of samples detecting seabirds", x="Sample Time", y="Detections") +
  facet_grid(location~., scales="free", switch="y") +
  theme(strip.text = element_text(size=rel(1.5)), #face="bold", 
        strip.background = element_rect(fill="lightgrey", colour="black",
                                        size=1))
fig4a_bird

#### Analyses ----

