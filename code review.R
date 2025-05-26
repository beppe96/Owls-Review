
rm(list=ls())

library(dplyr)
library(stats)
library(ggplot2)
library(car)
library(ggmap)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(countrycode)
library(ggrepel)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)



                                                ### WORD MAPS ###                    


data<-read.table("C:/Users/2794008O/OneDrive - University of Glasgow/Desktop/Data analysis/data/PhD/Aim 1/Resubmission/new_review_worldmaps.csv",
                 sep=",", header=T)

world <- ne_countries(scale = "small", returnclass = "sf")

world %>% ggplot() + geom_sf()

world %>% st_transform(crs = "+proj=robin") %>% ggplot() + geom_sf() + coord_sf(datum = NA) + theme_minimal()

data2 <- data %>% mutate(Study = TRUE)

data_with_iso <- data2 %>% mutate(Iso3 = countrycode::countrycode(sourcevar = Country, origin = "country.name", 
                                                                  destination = "iso3c"))

countries_urbanstudy_overall <- world %>% select(geometry, iso_a3) %>% 
  left_join(data_with_iso, by = c("iso_a3" = "Iso3")) %>% filter(Study == TRUE)


# World map 1: Total number of studies concerning owls in urban areas
world <- world %>% st_transform(crs = "+proj=robin") %>% ggplot() + 
  geom_sf() + 
  coord_sf(datum = NA) + 
  geom_sf(color = "grey") +
  geom_sf(data = countries_urbanstudy_overall, aes(fill = Study_number)) +
  scale_fill_distiller(palette = 4, direction = 1, name = "Number of studies", breaks = seq(from = 0, to = 68, by = 10),
                       guide = guide_colorbar(barheight = unit(4, "cm"))) + 
  theme_minimal()

world

library(svglite)
ggsave("world_n_studies.svg")


# World map 3: Number of owl species per country

countries_owl_species_number <- world %>% select(geometry, iso_a3) %>% 
  left_join(data_with_iso, by = c("iso_a3" = "Iso3")) %>% filter(Study == TRUE)

p_world <- world %>% st_transform(crs = "+proj=robin") %>% ggplot() + 
  geom_sf() + 
  coord_sf(datum = NA) + 
  geom_sf(color = "grey") +
  geom_sf(data = countries_owl_species_number, aes(fill = Owl_species_number)) +
  scale_fill_distiller(palette = 3, direction = 1, name = "Number of species", breaks = seq(from = 0, to = 20, by = 5),
                       guide = guide_colorbar(barheight = unit(4, "cm"))) + 
  theme_minimal()
p_world

ggsave("world_n_species.svg")


####################################################################################################################
###################################################################################################################


                                              ### PIE CHART ###


data2<-read.table("C:/Users/2794008O/OneDrive - University of Glasgow/Desktop/Data analysis/data/PhD/Aim 1/piechart_urbanimpacts.csv",
                 sep=",", header=T)


bp <- ggplot(data2, aes(x="", y=N_studies, fill=Theme))+
  geom_bar(width = 1, stat = "identity")
bp


pie <- bp + coord_polar("y", start=0)
pie


pie2 <- pie + scale_fill_brewer(palette="Blues")+
  theme_minimal() 


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


piefinal <- pie + scale_fill_brewer("Blues") +  blank_theme +
  theme(axis.text.x=element_blank())
piefinal


library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)

data2 %>%
  arrange(desc(N_studies)) %>%
  mutate(prop = percent(N_studies / sum(N_studies))) -> mydf 


df2 <- mydf %>% 
  mutate(csum = rev(cumsum(rev(N_studies))), 
         pos = N_studies/2 + lead(csum, 1),
         pos = if_else(is.na(pos), N_studies/2, pos))

torta <- ggplot(mydf, aes(x = "" , y = N_studies, fill = fct_inorder(Theme))) +
  geom_col(width = 12, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "GnBu") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(prop)),
                   size = 6, nudge_x = 0.5, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Urban-related impact")) +
  theme_void() +
  theme(legend.title = element_text(size=19),
        legend.text = element_text(size=18)) +
  guides(fill = guide_legend(title = "Number of studies per impact")) 

torta


####################################################################################################################
###################################################################################################################


                                                  ### IMPACTS PLOT ###


data<-read.table("C:/Users/2794008O/OneDrive - University of Glasgow/Desktop/Data analysis/data/PhD/Aim 1/Resubmission/new_impacts_plot.csv",
                 sep=",", header=T)


# or use 'dodge' instead of 'stack'

data$impact <- factor(data$impact, levels = c("Roads", "Obstacle collision", "Chemicals", "Anthropogenic noise", "ALAN", "Reaction to human presence"))


ggplot(data, aes(fill=category, y=nspecies_tot_studied, x=impact)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



ggplot(data, aes(fill=category, y=nspecies_tot_studied, x = impact, label=category)) + 
  geom_bar(position="stack", stat="identity", alpha=.6, show.legend = F) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  xlab("Urban-related factors") +
  ylab("Number of species affected") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label = label_size), position = "stack", vjust = -0.4)

library(svglite)
ggsave("urban-related factor plot.svg")




                                 ### ATTRACTION VS AVOIDANCE PLOT ###


data<-read.table("C:/Users/2794008O/OneDrive - University of Glasgow/Desktop/Data analysis/data/PhD/Aim 1/Resubmission/new table all Copy.csv",
                 sep=",", header=T)


# 'stack'


ggplot(data, aes(fill=response, y=n_species, x=trait)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  facet_wrap(~factor, ncol=1, strip.position = "left") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = seq(0, 20, 5))



library(svglite)
ggsave("barplot.attr-avoid.svg")




####################################################################################################################
###################################################################################################################


                                              ### SPECIES LIST PLOT ###


dataspecies<-read.table("C:/Users/2794008O/OneDrive - University of Glasgow/Desktop/Data analysis/data/PhD/Aim 1/Resubmission/new_species_list.csv",
                 sep=",", header=T)


p_main <- ggplot(data = dataspecies, mapping = aes(x = reorder(latin_name, n_studies), y = n_studies)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Species") +
  ylab("Number of studies") + 
  scale_y_continuous(breaks = seq(0, 80, 10)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

p_main2 <- p_main + theme(axis.text = element_text(size = 11)) + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p_main2

ggsave("species.list.plot.svg")


## MERGE SPECIES LIST AND SPECIES WORLD MAP 

library(patchwork)
multi1 <- p_main2 + inset_element(p = p_world, left = 0.09, bottom = 0.01, right = 0.99, top = 0.55)
multi1 




####################################################################################################################
###################################################################################################################


                                                ### IUCN BARPLOT ###



dataiucn<-read.table("C:/Users/2794008O/OneDrive - University of Glasgow/Desktop/Data analysis/data/PhD/Aim 1/species_list_iucn_global.csv",
                 sep=",", header=T)


dataiucn$IUCN_Category <- factor(dataiucn$IUCN_Category, levels = c("Unknown", "VU", "NT", "LC"))



iucnplot <- ggplot(dataiucn, aes(fill=Trend, y=Total, x = IUCN_Category, label=Trend)) + 
  geom_bar(position="stack", stat="identity", alpha=.6, show.legend = T) +
  theme(axis.title.x = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  xlab("IUCN Category (Global)") +
  ylab("Number of species") +
  theme_bw() + 
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.87, 0.17), legend.title = element_text(size = 18), legend.text = element_text(size = 16)) 

iucnplot2 <- iucnplot + theme(axis.text = element_text(size = 18)) + theme(axis.title = element_text(size = 20))  
iucnplot2



####################################################################################################################
###################################################################################################################


                                                ### PUB YEAR OVER TIME ###



datayear<-read.table("C:/Users/2794008O/OneDrive - University of Glasgow/Desktop/Data analysis/data/PhD/Aim 1/Resubmission/new_study_pub_year.csv",
                     sep=",", header=T)

glimpse(datayear)

#trial
year <- ggplot(datayear, aes(x=Publication_year, y=Number_study)) + 
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, formula = y ~ splines::bs(x, 3), se=FALSE) +
  xlab("Publication year") +
  ylab("Number of studies") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(1925, 2023)) + 
  scale_y_continuous(limits = c(0, 20)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 14)) 
year


#better
year <- ggplot(datayear, aes(x=Publication_year, y=Number_study)) + 
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
  xlab("Publication year") +
  ylab("Number of studies") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(1928, 2023)) + 
  scale_y_continuous(limits = c(0, 15)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 14)) 
year

ggsave("time.trend.studies.svg")


#or an alternative for the line

library(hrbrthemes)
ggplot(datayear, aes(x=Publication_year, y=Number_study)) + 
  geom_point(size=1.5, color="black")+
  geom_line( color="blue") +
  xlab("Publication year") +
  ylab("Number of studies") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(1925, 2023)) + 
  scale_y_continuous(limits = c(0, 20)) 


