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
mutate <- dplyr::mutate

#Master ctene
data.file <- "C:/Users/Tiffa/Documents/DeepC/DeepC R Data/MC_clean_depSC.csv"

enz.data.clean <- readr::read_csv(data.file)

# calculate residual rates
col.start <- 6
levels.pressure <- 5 #including 1 atm and recovery
count.enz <- (ncol(enz.data) + 1 - col.start) / levels.pressure
enz.data.resid <- calc_residual_activity(enz.data.clean, col.start, levels.pressure, count.enz)

# initial QC step: scrap any run with a residual rate value <0
enz.data.resid.good <- enz.data.resid %>% 
  dplyr::filter_all(all_vars(. >= 0))

#Split enzyme and pressure into separate cols
heat <- enz.data.resid.good %>% 
  tidyr::gather(key = "enz.pressure", value = "rate", 
                -"Sample ID", -"Species", -"Mass (g)", -"Volume (m)", -"Coversion") %>% 
  dplyr::mutate(enzyme= substr(enz.pressure, 1, str_locate(enz.pressure, "[0-9]")-1)) %>% 
  dplyr::mutate(pressure= substr(enz.pressure, str_locate(enz.pressure, "[0-9]"), str_length(enz.pressure))) %>% 
  dplyr::select(-enz.pressure) %>% 
  tidyr::drop_na(rate) %>% 
  dplyr::group_by(Species, enzyme, pressure)


#select data for heatmap
trim.heat <- heat %>% 
            select(Species, `Mass (g)`, enzyme, pressure, rate) %>% 
            na.omit()

#Change Species Labels
spec.labs <- c("Pleurobrachia bachei",
             "Bathyctena chuni",
             "Lampea sp. B",
             "Bathocyroe fosteri",
             "Beroe abyssicola",
             "B. forskalii",
             "Platyctene sp. P",
             "Cydippida sp. RLL",
             "Lampocteis cruentiventer",
             "Lobate sp. V",
             "Cydippida sp. W",
             "Cydippida sp. G",
             "Cydippida sp. C",
             "Cydippida sp. B",
             "Lampea sp. A",
             "Aulococtena acuminata",
             "Lobate sp. A",
             "Beroe sp. A")

names(spec.labs) <- c("Pleurobrachia bachei",
                    "Bathyctena chuni",
                    "Lampea deep",
                    "Bathocyroe fosteri",
                    "Beroe abyssicola",
                    "Beroe forskalii",
                    "Tjalfiella pink",
                    "Undescribed spLiverLips",
                    "Lampocteis cruentiventer",
                    "Vermillion lobate",
                    "Undescribed spW",
                    "Undescribed spN",
                    "Undescribed spGoldenCyd",
                    "Undescribed spC",
                    "Undescribed spB",
                    "Lampea shallow",
                    "Aulococtena",
                    "Amber lobate",
                    "Beroe sp")

#plot heatmap
setwd("C:/Users/Tiffa/Documents/DeepC/DeepC R Data")
jpeg("fig6.jpeg", width=6, height=6, units="in",res=600, pointsize = 12)
heat %>%   
  #filter(enzyme %in% c("CK")) %>%
    ggplot(mapping = aes(x = pressure, y = Species, fill = log10(rate))) +
    geom_tile() +
    xlab(label = "Pressure (bar)")+
    facet_wrap(~ enzyme)+
    #scale_y_reverse()+
    scale_fill_gradient(breaks= c(-1,0, 1.5), 
                      labels = c("Low", "Medium", "High"),
                      name = "Enzymatic Activity",
                      low = "yellow", high = "blue")+
    scale_x_discrete(labels = c('1','Rec','200','400','600'))+
    theme(legend.position = "none",
          axis.text.y = element_text(size=11,color="black", face="italic"),
          axis.text.x = element_text(size=9, color="black"))
dev.off()
     
                      


heat <- gplot(date = filter(heat, enzyme == "ck"), 
              aes(x = pressure, y = Species, fill = log10(rate))) +
  geom_tile()+
  xlab(label = "Pressure") +
  scale_fill_manual(breaks= c(-1,0, 1.5), labels = c("Low", "Medium", "High"))+
  scale_x_discrete(labels = c('1','Rec','200','400','600'))





