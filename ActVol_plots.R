install.packages("reshape2")
install.packages("plyr")
library("plyr")
library(dplyr)
library("reshape2")
library("conflicted")
library("tidyverse")
library("dplyr")
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(gridExtra)

filter <- dplyr::filter

#Load Activation Volume Data
vol_file <- "C:/Users/Tiffa/Documents/DeepC/DeepC R Data/ActVol_whole.csv"

vol.data <- readr::read_csv(vol_file)

##Put data into long form##

#vol.long <- melt(vol.data, id.vars = c("Species"))
#summarise <- plyr::summarise
#vol.long.ordered <- vol.long %>% 
  #ddply(c("Species", "variable")) %>% 
  #dplyr::group_by(Species, variable)

enz.vol.long <- vol.data %>% 
  tidyr::gather(key = "enz.pressure", value = "rate", -"Species") %>% 
  dplyr::mutate(enzyme= substr(enz.pressure, 1, str_locate(enz.pressure, "[0-4]")-1)) %>% 
  dplyr::mutate(pressure= substr(enz.pressure, str_locate(enz.pressure, "[0-4]"), str_length(enz.pressure))) %>% 
  dplyr::select(-enz.pressure) %>% 
  tidyr::drop_na(rate) %>% 
  dplyr::group_by(Species, enzyme, pressure)

enz.vol.long$rate <- as.numeric(enz.vol.long$rate)

## OUTLIER REMOVAL ##
enz.vol.long.clean <- enz.vol.long %>% 
  
  # 2*sigma outlier filter
  #filter(!(abs(rate - median(rate)) > 2*sd(rate)))
  # 1.5*IQR outlier filter
  dplyr::filter(!(abs(rate - median(rate)) > 1.5*IQR(rate, na.rm=TRUE)))


## SUMMARY STATS ##      
vol.long.summary <- enz.vol.long.clean %>% 
dplyr::summarise(mean = mean(rate), stderr = sd(rate)/sqrt(n()), n = n())

##AOV whole run/ pecies
#Select Data for AOV
index.vol <- enz.vol.long.clean %>%
  select(Species, enzyme, pressure, rate) %>% 
  filter(Species == "Vermillion lobate") %>% 
  filter(enzyme == "PK")

#AOV Dataframe
enz.vol <- c(index.vol$pressure)
vol.rate <- c(index.vol$rate) 

species.vol.chng <-data.frame(enz.vol, vol.rate)

#Run ANOVA
species.vol.chng.aov<-aov(vol.rate ~ enz.vol, data = species.vol.chng)
summary(species.vol.chng.aov)

##AOV enzyme/ pressure across Species
#select data for AOV
selc.vol <- enz.vol.long.clean %>%
  select(Species, enzyme, pressure, rate) %>% 
  filter(Species %in% c("Pleurobrachia bachei",
                        "Bathyctena chuni",  
                        "Lampea deep",
                        "Bathocyroe fosteri",
                        "Beroe abyssicola",
                        "Beroe forskalii",
                        "Tjalfiella pink",
                        "Undescribed spLiverLips",
                        "Lampocteis cruentiventer"))%>%
  filter(enzyme == "PK") %>% 
  filter(pressure == "0rec")

#AOV Dataframe
enz.pres <- c(selc.vol$Species)
vol.chng <- c(selc.vol$rate) 

cross.vol.chng <-data.frame(enz.pres, vol.chng)

#Run ANOVA
cross.vol.chng.aov<-aov(vol.chng ~ enz.pres, data = cross.vol.chng)
summary(cross.vol.chng.aov)

#Run Linear Regressions for Plots#
#linear model
vol.model<- vol.long.summary %>% 
  filter(Species %in% c("Bathocyroe fosteri")) %>% 
  filter(enzyme %in% C("CK"))

lm_fit <- lm(mean ~ pressure, data=vol.model)

summary(lm_fit)

##PLOTS##
# make sure x-axis plots in the right order
levels.pressure <- c("200bar", "400bar", "00bar", "0rec")

vol.long.summary$pressure <- factor(vol.long.summary$pressure, levels = levels.pressure, ordered = TRUE)
#enz.data.resid.long$pressure <- factor(enz.data.resid.long$pressure, levels = levels.factor.pressure, ordered = TRUE)

#plots
#Change Species Labels
sp.labs <- c("Pleurobrachia bachei",
             "Bathyctena chuni",
             "Lampea sp. B",
             "Bathocyroe fosteri",
             "Beroe abyssicola",
             "Beroe forskalii",
             "Platyctene sp. P",
             "Cydippida sp. RLL",
             "Lampocteis cruentiventer")

names(sp.labs) <- c("Pleurobrachia bachei",
                    "Bathyctena chuni",
                    "Lampea deep",
                    "Bathocyroe fosteri",
                    "Beroe abyssicola",
                    "Beroe forskalii",
                    "Tjalfiella pink",
                    "Undescribed spLiverLips",
                    "Lampocteis cruentiventer")
setwd("C:/Users/Tiffa/Documents/DeepC/DeepC R Data")
jpeg("fig7.jpeg", width=6, height=6, units="in",res=600, pointsize = 12)
vol.long.summary %>% 
  filter(Species %in% c("Pleurobrachia bachei",
                        "Bathyctena chuni",  
                        "Lampea deep",
                        "Bathocyroe fosteri",
                        "Beroe abyssicola",
                        "Beroe forskalii",
                        "Tjalfiella pink",
                        "Undescribed spLiverLips",
                        "Lampocteis cruentiventer"))%>% 
  #filter(enzyme %in% c("CK")) %>% 
  ggplot(aes(x = pressure, y = mean, group = enzyme, color = enzyme))+ #linetype= enzyme)) + 
  scale_x_discrete(labels = c('1-200','200-400','400-600','600-Rec'))+
  #facet_grid(~enzyme) + 
  facet_wrap(~ Species, labeller = labeller(Species = sp.labs))+
  #facet_grid(Species ~ enzyme, scales="free_x")+
  geom_point() +
  geom_line()+
  #scale_linetype_manual(values=c( "solid", "twodash", "dotted"))+
  scale_color_manual(values=c("aquamarine4","orangered3", "yellow4"))+
  scale_size_manual(values=c(1.5, 1))+
  geom_errorbar(aes(ymin = mean-stderr, ymax = mean+stderr), color = "black", position = "dodge", width = 0.3)+
  #geom_smooth(method='lm', formula= y~x, se=FALSE, color = "black")+
  #stat_smooth(method="lm",se=FALSE, color = "black")+
  xlab("Pressure (bar)")+
  ylab("Activation Volume (cm3/mol)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.spacing.x = unit(1,"line"),
        panel.border=element_rect(fill=NA),
        legend.title = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11, face = "italic"),
        axis.title.x = element_text(size=11, colour="black"),
        axis.title.y = element_text(size=11, colour="black"),
        axis.text.x = element_text(size=6.5, colour="black"),
        axis.text.y = element_text( color="black",size=11),
        aspect.ratio=1)
dev.off()
      
