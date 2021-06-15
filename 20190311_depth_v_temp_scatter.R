# Plot cteno taxa in temp-depth space, also plots smoothed CTD profiles
# 20190314 JRW
install.packages("dplyr")
install.packages("ggpubr")
install.packages("rlang")
library(ggplot2)
library(reshape2)
library(ggpubr)
library("scales")
library(RColorBrewer)

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

zvt_data <- read.csv("C:/Users/Tiffa/Documents/DeepC/DeepC R Data/20190312_27Ctenos_depth_temp.tsv",sep='\t')

#zvt_data <- read.csv("C:/Users/Tiffa/Documents/DeepC/DeepC R Data/20190312_9Ctenos_depth_temp.tsv", sep='\t')
zvt_data.m <- zvt_data[,c("sp","depth_med","depth_stdev","temp_med","temp_stdev","order")]

##USER-DEFINED FUNCTIONS##

theme_black = function(base_size = 24, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35", size = 0.5),  
      panel.grid.minor = element_line(color = "grey20", size = 0.5),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      #plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}
customColors <- brewer.pal(3, "Dark2") # lighter blue for black bkgd
names(customColors) <- c("cydi","bero","loba")
#names(customColors) <- c("cydi","bero","loba")

customShapes <- 16

z.range = c(9, 4000)
T.range = c(-2, 30)

# load long-format CTD data
ctd_data <- read.csv("C:/Users/Tiffa/Documents/DeepC/DeepC R Data/20190312_CTD_MB-HI.tsv", sep='\t')
# drop depths outside specified range
ctd_data <- subset(ctd_data, depth >= min(z.range) & depth <= max(z.range))
# clean up the HOT data
#ctd_data <- subset(ctd_data, source != "HOT-298" | depth <= 1000 | depth >= 2800)
# coerce float data to float
ctd_data$depth <- as.numeric(as.character(ctd_data$depth))
ctd_data$temp <- as.numeric(as.character(ctd_data$temp))
ctd_data$sal <- as.numeric(as.character(ctd_data$sal))

# error bar bounds
zvt_data.m$errorbar.min = zvt_data.m$depth_med - zvt_data.m$depth_stdev
zvt_data.m$errorbar.min = sapply(zvt_data.m$errorbar.min, function(x) max(min(z.range),x))
zvt_data.m$errorbar.max = zvt_data.m$depth_med + zvt_data.m$depth_stdev
zvt_data.m$errorbar.max = sapply(zvt_data.m$errorbar.max, function(x) min(max(z.range),x))

windowsFonts(Times=windowsFont("TT Times New Roman"))

zvt_data.m <- zvt_data.m[which(zvt_data.m$sp != "Bero_cucu"), ]
# print version 20190314 0003: uses a vertical x-axis to enable geom_smooth
# color-coded by class
setwd("C:/Users/Tiffa/Documents/DeepC/DeepC R Data")
jpeg("fig1.jpeg", width=6, height=6, units="in",res=600, pointsize = 12)
ggplot(zvt_data.m, aes(y=temp_med, x=depth_med,color=order)) +
  coord_flip(xlim = z.range, ylim = T.range) +
  coord_flip() +
  scale_x_reverse()+
  #scale_x_continuous(trans=reverselog_trans(10))+
  geom_smooth(data=ctd_data, aes(y=temp, x=depth, linetype=source), color="black", span=.7, method='loess', se=FALSE)+
  xlab("Depth (m)")+
  ylab("Temperature (C)")+
  #ggtitle("Habitat Depth of Ctenophore Species")+
  geom_point(aes(shape= order), position=position_jitter(h=0.1, w=0.1),
             alpha = 0.6, size = 3)+
  #geom_point(aes(shape= order), size = 3, position = position_dodge(width = 0.2))+
  scale_shape_manual(values=c(18, 16, 17), labels = c("Beroida", "Cydippida", "Lobata"))+
  scale_color_manual(values=c('#000000','#009E73', '#D55E00'),labels = c("Beroida", "Cydippida", "Lobata"))+
  scale_linetype_manual(values=c( "solid", "dotted", "dashed"))+
  #scale_linetype_discrete(labels = c("Monterey Bay", "Hawaii", "Puget Sound"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.spacing.x = unit(0,"line"),
        panel.border=element_rect(fill=NA),
        legend.title = element_blank(),
        legend.text = element_text(size=11, colour="black"),
        legend.position = "none",
        legend.box = "vertical",
        plot.title = element_text(hjust = 0.5,size=11),
        axis.title.x = element_text(size=11, colour="black"),
        axis.title.y = element_text(size=11, colour="black"),
        axis.text.x = element_text(size=11, colour="black"),
        axis.text.y = element_text( color="black",size=11),
        aspect.ratio=1)
dev.off()
  
  
  #ggplot(NULL, aes(y=temp_med, x=depth_med)) +
  ## scale and theme
  #coord_flip(xlim = z.range, ylim = T.range) +
  #coord_flip() +
  #scale_x_continuous(trans=reverselog_trans(10)) + #log
  #scale_x_reverse() + #linear
  #theme_pubr() +
  ## ctd profiles ##
  #geom_smooth(data=ctd_data, aes(y=temp, x=depth, linetype=source), color="white", span=.7, method='loess', se=FALSE) +
  ## scatter plot stuff ##
  #scale_shape_manual(customShapes) +
  #scale_color_discrete(customColors) +
  #scale_color_brewer(3,"Dark2")
  #scale_color_manual(customColors)+
  #geom_point(data=filter(zvt_data.m, order == "bero"), aes(x = temp_med, y = depth_med), position="identity",size=12,alpha=0.5, color = "orange") +
  #geom_point(data=zvt_data.m, position="identity",size=12,alpha=0.5) +
  #geom_point(data=zvt_data.m, position="identity",size=12,alpha=0.5) +
  # error bars
  #geom_errorbar(data=zvt_data.m, position="identity", aes(ymin=temp_med-temp_stdev, ymax=temp_med+temp_stdev), width=0) +
  #geom_errorbarh(data=zvt_data.m, position="identity", aes(xmin=errorbar.min, xmax=errorbar.max, height=.1)) +
  # species labels
  #geom_text(data=zvt_data.m, aes(label=sp),hjust=1, vjust=0, color="Black") +
  theme(#legend.position='none',
        plot.background = element_rect(fill ="black"),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white",size = 1,linetype ="solid"),axis.text.x = element_text(angle = 45,size=10, colour="white"),axis.text.y = element_text( color="white",size=10))
# hide legend


# print version: uses a vertical x-axis to enable geom_smooth
ggplot(NULL, aes(y=temp_med, x=depth_med)) +
  ## scale and theme
  coord_flip(xlim = z.range, ylim = T.range) +
  #coord_flip() +
  #scale_x_continuous(trans=reverselog_trans(10)) + #log
  scale_x_reverse() + #linear
  #theme_pubr() +
  ## ctd profiles ##
  geom_smooth(data=ctd_data, aes(y=temp, x=depth, linetype=source), color="black", span=.7, method='loess', se=FALSE) +
  ## scatter plot stuff ##
  scale_shape_manual(customShapes) +
  geom_point(data=zvt_data.m, position="identity",size=6) +
  # error bars
  geom_errorbar(data=zvt_data.m, position="identity", aes(ymin=temp_med-temp_stdev, ymax=temp_med+temp_stdev), width=0) +
  geom_errorbarh(data=zvt_data.m, position="identity", aes(xmin=errorbar.min, xmax=errorbar.max, height=.1)) +
  # species labels
  geom_text(data=zvt_data.m, aes(label=sp),hjust=1, vjust=0, color="red") +
  theme(legend.position='none') # hide legend

# print version 20190312 1825
ggplot(NULL, aes(x=temp_med, depth_med)) +
  ## scale and theme
  scale_y_continuous(trans=reverselog_trans(10)) + #log
  #scale_y_reverse() + #linear
  #theme_pubr() +
  ## ctd profiles ##
  #geom_smooth(data=ctd_data, aes(x=temp, y=depth, linetype=source))
  ## scatter plot stuff ##
  scale_shape_manual(customShapes) +
  geom_point(data=zvt_data.m, position="identity",size=6) +
  # error bars
  geom_errorbarh(data=zvt_data.m, position="identity", aes(xmin=temp_med-temp_stdev, xmax=temp_med+temp_stdev), height=0) +
  geom_errorbar(data=zvt_data.m, position="identity", aes(ymin=errorbar.min, ymax=errorbar.max, width=.1)) +
  # species labels
  geom_text(data=zvt_data.m, aes(label=sp),hjust=1, vjust=0, color="red") +
  theme(legend.position='none') # hide legend

# black bkgd version
ggplot(zvt_data.m, aes(x=temp, depth)) +
  scale_shape_manual(customShapes) +
  scale_y_continuous(trans=reverselog_trans(10)) +
  geom_point(position="identity",size=6,colour="#1A56CC") +
  theme_black() +
  theme(legend.position='none') # hide legend
