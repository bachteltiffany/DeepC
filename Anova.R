install.packages("conflicted")
install.packages("gridExtra")
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

## USER-DEFINED FUNCTIONS ##

# Order Columns for Residual Activity Calc
calc_residual_activity <- function(enz.data, col.start, levels.pressure, count.enz){
  for(n.enz in seq(1, count.enz)){
    colnum.0psi <- col.start + ((n.enz - 1) * levels.pressure)
    for(level.pressure in rev(seq(1, levels.pressure))){
      colnum.pressure <- col.start + level.pressure - 1 + ((n.enz - 1) * levels.pressure)
      colname.0psi <- colnames(enz.data)[colnum.0psi]
      colname.pressure <- colnames(enz.data)[colnum.pressure]
      #colname.residact <- paste("resid", colname.pressure, sep='_')
      colname.residact <- colname.pressure
      #print(c(colnum.pressure, colnum.0psi, colname.residact)) #TEST
      # this should replace the raw data column with residual rate
      enz.data <- enz.data %>% mutate(!!colname.residact := .[[colnum.pressure]] / .[[colnum.0psi]])
    }
  }
  return(enz.data)
}

#Load Master Ctene R
data_file <- "C:/Users/Tiffa/Documents/DeepC/DeepC R Data/Master_CteneR_spellchecked.csv"

enz.data <- readr::read_csv(data_file)

# calculate residual rates
col.start <- 6
levels.pressure <- 5 #including 1 atm and recovery
count.enz <- (ncol(enz.data) + 1 - col.start) / levels.pressure
enz.data.resid <- calc_residual_activity(enz.data, col.start, levels.pressure, count.enz)

# initial QC step: scrap any run with a residual rate value <0
enz.data.resid.good <- enz.data.resid %>% 
  dplyr::filter_all(all_vars(. >= 0))

# put dataframe in long form and group it for downstream stats
enz.data.resid.long <- enz.data.resid.good %>% 
  tidyr::gather(key = "enz.pressure", value = "rate", 
                -"Sample ID", -"Species", -"Mass (g)", -"Volume (m)", -"Coversion") %>% 
  dplyr::mutate(enzyme= substr(enz.pressure, 1, str_locate(enz.pressure, "[0-9]")-1)) %>% 
  dplyr::mutate(pressure= substr(enz.pressure, str_locate(enz.pressure, "[0-9]"), str_length(enz.pressure))) %>% 
  dplyr::select(-enz.pressure) %>% 
  tidyr::drop_na(rate) %>% 
  dplyr::group_by(Species, enzyme, pressure)

## OUTLIER REMOVAL ##
enz.data.resid.clean <- enz.data.resid.good %>% 
  # remove negative residual activities
  dplyr::filter(rate > 0) %>% 
  # 2*sigma outlier filter
  #filter(!(abs(rate - median(rate)) > 2*sd(rate)))
  # 1.5*IQR outlier filter
  dplyr::filter(!(abs(rate - median(rate)) > 1.5*IQR(rate, na.rm=TRUE)))

## SUMMARY STATS ##

enz.data.resid.clean.summary <- enz.data.resid.long.clean %>% 
  dplyr::summarise(mean = mean(rate), stderr = sd(rate)/sqrt(n()), n = n())

## MAIN ANALYSIS ##

#Select Data
index.enz <- enz.data.resid.long.clean %>%
  select(Species, enzyme, pressure, rate) %>% 
  filter(Species == "Vermillion lobate") %>% 
  filter(enzyme == "CK")
  
#AOV Dataframe
enz.pressure <- c(index.enz$pressure)
rate <- c(index.enz$rate) 

species <-data.frame(enz.pressure, rate)

#Run ANOVA
species.aov<-aov(rate ~ enz.pressure, data = species)
summary(species.aov)


## T-test ##
#Select Data
species.enz <- enz.data.resid.long.clean %>%
  select(Species, enzyme, pressure, rate) %>%
  filter(Species == "Vermillion lobate") %>% 
  filter(enzyme == "PK") %>% 
  filter(pressure %in% c("0psi", "0Recovery")) 
  

#Convert from long to wide
enz.0psi <-species.enz [species.enz$pressure == "0psi", c(2, 4)]
enz.0Rec <-species.enz [species.enz$pressure == "0Recovery", c(2, 4)]  

names(enz.0psi)[2] <- "Initial"
names(enz.0Rec)[2] <- "Recovery"

merge.wide <- merge(enz.0psi,enz.0Rec, by = "enzyme")

#Conduct t.test
index.test <- merge.wide %>% slice(1:2)

t.test(index.test$Initial,index.test$Recovery, paired = TRUE, conf.level = 0.95, alternative = "two.sided", data = species.enz)






