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

#Load Depth and Conversion Data
data.file <- "C:/Users/Tiffa/Documents/DeepC/DeepC R Data/MC_clean_depths.csv"

enz.data.clean <- readr::read_csv(data.file)

#Mutiply Extiction Coefficents By Conversion Rate
act.wet.weight<- enz.data.clean %>% 
  #select(Species, `Depth (m)`, Coversion, CK0psi, MDH0psi, PK0psi) %>% 
  #mutate(CK0psi =CK0psi) %>% 
  #mutate(MDH0psi =MDH0psi) %>% 
  #mutate(PK0psi =PK0psi) %>% 
  select(Species, CK0psi, MDH0psi, PK0psi)

# put dataframe in long form and group it for downstream stats
act.wet.weight.long <- melt(act.wet.weight, id.vars = c("Species")) 

act.wet.weight.ordered <- act.wet.weight.long %>% 
ddply(c("Species", "variable")) %>% 
dplyr::group_by(Species, variable)

## OUTLIER REMOVAL ##
#act.wet.weight.clean <- act.wet.weight.ordered %>% 
  # 2*sigma outlier filter
  #filter(!(abs(rate - median(rate)) > 2*sd(rate)))
  # 1.5*IQR outlier filter
  #dplyr::filter(!(abs(value - median(value)) > 1.5*IQR(rate, na.rm=TRUE)))

## SUMMARY STATS: Act/g Wet Weight ##      
act.wet.weight.summary <- act.wet.weight.ordered %>% 
  dplyr::summarise(mean = mean(value), stderr = sd(value)/sqrt(n()), n = n())   
  #mutate( mean = log10(mean)) %>% 
  #mutate(stderr = log10(stderr))



## SUMMARY STATS: Depth ##
sp.depth <- enz.data.clean %>% 
  select(Species, `Depth (m)`)

sp.depth.long <- melt(sp.depth, id.vars = c("Species"))

sp.depth.long.summary <- sp.depth.long %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(mean = mean(value))

#create new DF: Depth x Act/g wet weight
depth.act <- merge(act.wet.weight.summary,sp.depth.long.summary, by = "Species" )
rename <- dplyr::rename
depth.act.clean <- depth.act %>% 
  select(Species, mean.y, variable, mean.x, stderr, n) %>% 
  rename(depth = mean.y) %>% 
  rename(enzyme = variable) %>% 
  rename(rate = mean.x) %>% 
  mutate(depth = round(depth))

#Add Order to depth.act.clean
#library(xlsx)
#write.xlsx(depth.act.clean,"C:/Users/Tiffa/Documents/DeepC/DeepC R Data/act_gwetcorrect.xlsx")

data.file <- "C:/Users/Tiffa/Documents/DeepC/DeepC R Data/act_gwetcorrect.csv"

act.depth.order <- readr::read_csv(data.file)

#Run Linear Regressions for Data#
#linear model
model<- act.depth.order %>% 
  filter(enzyme %in% c("PK0psi")) %>%
  filter(Order %in% c("Cydippida")) %>% 
  select(depth, rate)

mod <- model %>% 
        lm(rate ~ depth,.)

summary(mod)

#Generalized Lenear Model
mod.glm <- model %>% 
  glm(rate ~ depth,. , family = gaussian(link = "identity")) 

summary(mod.glm)

##PLOTS: Activity/ g Wet weight##

windowsFonts(Times=windowsFont("TT Times New Roman"))

#Change Species Labels
enz.labs <- c("CK",
             "MDH",
             "PK")
names(enz.labs) <- c("CK0psi",
              "MDH0psi",
              "PK0psi")
setwd("C:/Users/Tiffa/Documents/DeepC/DeepC R Data")
jpeg("fig2.jpeg", width=6, height=6, units="in",res=600, pointsize = 12)
act.depth.order %>% 
  #filter(enzyme %in% c("CK0psi")) %>% 
  ggplot( aes(x= depth, y= rate, color = Order, shape = Order)) + 
  #geom_point(size=2)+
  geom_point(position=position_jitter(h=0.1, w=0.1),
             alpha = 0.6, size = 3)+
  coord_flip() +
  xlab("Minimum Depth of Occurrence (meters)")+
  ylab(bquote("Mean Enzymatic Activity ("*"units"~ g^-1*")"))+
  scale_x_reverse()+
  #scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  #scale_x_continuous(trans=reverselog_trans(10))+
  scale_shape_manual(values=c(18, 16, 17))+ 
  scale_color_manual(values=c('#000000','#009E73', '#D55E00'))+
  stat_summary(fun.data=mean_cl_normal) + 
  #geom_smooth(data=subset(act.depth.order, Order== "Beroida"), 
              #aes(depth, rate,color=factor(Order)), method=lm,se=FALSE)+
  #geom_smooth(method='lm')+
  #geom_smooth(method=lm)+
  #scale_y_continuous(labels = percent)+
  facet_wrap(~enzyme, ncol=2, scales = "free_x", labeller = labeller(enzyme = enz.labs))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.spacing.x = unit(1,"line"),
        panel.border=element_rect(fill=NA),
        strip.background = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        legend.title =element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size=11, colour="black"),
        axis.title.y = element_text(size=11, colour="black"),
        axis.text.x = element_text(size=11, colour="black"),
        axis.text.y = element_text( color="black",size=11 ),
        aspect.ratio=1)
dev.off()


