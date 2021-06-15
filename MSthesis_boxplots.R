install.packages("egg")
library(egg)

##Pooled Boxplots##
#Make sure x axis in in the correct order
levels.factor.pressure <- c("0psi", "3000psi", "6000psi", "9000psi", "0Recovery")
enz.data.resid.long.clean$pressure <- factor(enz.data.resid.long.clean$pressure, levels = levels.factor.pressure, ordered = TRUE)
enz.data.resid.long$pressure <- factor(enz.data.resid.long$pressure, levels = levels.factor.pressure, ordered = TRUE)

#plots
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}
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
jpeg("fig5.jpeg", width=6, height=6, units="in",res=600, pointsize = 12)
enz.data.resid.long.clean %>% 
  filter(Species %in% c("Pleurobrachia bachei",
                        "Bathyctena chuni",  
                        "Lampea deep",
                        "Bathocyroe fosteri",
                        "Beroe abyssicola",
                        "Beroe forskalii",
                        "Tjalfiella pink",
                        "Undescribed spLiverLips",
                        "Lampocteis cruentiventer"))%>% 
  filter(enzyme %in% c("CK")) %>% 
  ggplot(aes(x = pressure, y = rate, c = Species, fill = Species)) +
  geom_boxplot(linetype = "dashed", outlier.alpha = 0.1, position = position_dodge(width=0.5)) +
  facet_wrap(~ Species, labeller = labeller(Species = sp.labs))+ #scales = "free",
  stat_summary(fun ="mean")+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.alpha = 0.1) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..)) +
  #stat_summary(fun = median, fun.max = length,geom = "text", aes(label = ..ymax..), size= 2, hjust = 0.5, position = position_dodge(-1))+
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median)+
  stat_summary(fun =mean, geom="point", shape=20, size=2)+
  scale_x_discrete(labels = c('1','200','400','600','Rec'))+
  scale_y_continuous(labels = percent, breaks = round(seq(min(enz.data.resid.long.clean$rate), max(enz.data.resid.long.clean$rate), by = 0.5),1), limits=c(0,2.5))+
  coord_fixed()+
  xlab("Pressure (bar)")+
  ylab(bquote("Enzymatic Activity ("*Delta ~ V[max]*") (% 1 bar Activity)"))+
  scale_fill_manual(values=c("dark grey", "dark grey","dark grey","dark grey","dark grey","dark grey","dark grey","dark grey","dark grey"))+
  theme(legend.position = "none",
        plot.title = element_text(size=11,
                      face="italic",hjust = 0.5),
        plot.background = element_rect(fill ="white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.spacing.x = unit(1,"line"),
        panel.border=element_rect(fill=NA),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11, face = "italic"),
        axis.line = element_line(colour = "black",size = 1,linetype = "solid"),
        axis.title.x = element_text(size=11, colour="black"),
        axis.title.y = element_text(size=11, colour="black"),
        axis.text.x = element_text(size=11, colour="black"),
        axis.text.y = element_text( color="black",size=11),
        aspect.ratio=1)
dev.off()
