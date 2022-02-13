library(ggplot2)
library(ggthemes)

##importing all data for biomass
arth_biomass <- read.csv2("outputs/data/arthropods_biomass_plot.csv")
pods_biomass <- read.csv2("outputs/data/pods_biomass_plot.csv")
damage_biomass <- read.csv2("outputs/data/damage_biomass_plot.csv")
#yield_biomass <- read.csv2("outputs/data/yield_biomass_plot.csv")#not including yield - not is not reliable

all_biomass <- rbind(arth_biomass,pods_biomass,damage_biomass)#,yield_biomass)
table(all_biomass$response)

#reorder and rename levels
all_biomass$response <- factor(all_biomass$response, levels = c("Araneae *","Coleoptera *","Diptera ***","Formicidae **","Hymenoptera **","Lepidoptera *","Orthoptera *","Medium pods (*)","Big pods ***","Large pods **","BPD (*)"))#"Harvested pods ***","Wet weight (g) **","Dry weight (g) **"

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.5)

#removing variables I don't want
data_biomass <- all_biomass[!all_biomass$response %in% c("Medium pods (*)","Big pods ***"),]

head(data_biomass)
plot_biomass <- ggplot()+
  geom_line(data_biomass,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(data_biomass,mapping=aes(x=biomass_tree,ymin=conf.low,ymax=conf.high),alpha=.2)+
  facet_wrap(~response,scales="free_y",nrow=5,ncol=3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Counts (tree/month)",x="Cacao tree biomass", title="")+
  theme_bw() + 
  expand_limits(y=0)+
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+#panel.grid.major = element_blank()
  theme(title = element_text(size=15))+
  theme(axis.title.x=element_text(size=30))+
  theme(axis.title.y=element_text(size=30))+
  theme(axis.text.x=element_text(size=25))+
  theme(axis.text.y=element_text(size=25))+
  #theme(legend.text=element_text(size=30),legend.title = element_blank(),legend.position="bottom",legend.key = element_blank())+ # Remove only the legend title
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5),"cm"))+
  theme(strip.text.x = element_text(size=30))+
  theme(strip.background = element_rect(fill = 'grey'))
plot_biomass

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_all_biomass.png", plot = plot_biomass, width =15 , height = 13) #To save the plot

##importing all data for shade
arth_shade <- read.csv2("outputs/data/arthropods_shade_plot.csv")
damage_shade <- read.csv2("outputs/data/damage_shade_plot.csv")

all_shade <- rbind(arth_shade,damage_shade)
table(all_shade$response)

#reorder and rename levels
all_shade$response <- factor(all_shade$response, levels = c("Coleoptera *","Neuroptera *","Orthoptera ***","DBT (*)","Pest feeding **"))

head(all_shade)
plot_shade <- ggplot()+
  geom_line(all_shade,mapping=aes(x=shade_treelevel ,y=predicted))+
  geom_ribbon(all_shade,mapping=aes(x=shade_treelevel,ymin=conf.low,ymax=conf.high),alpha=.2)+
  facet_wrap(~response,scales="free_y",nrow=2,ncol=3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Counts (tree/month)",x="Cacao tree shade cover", title="")+
  theme_bw() + 
  expand_limits(y=0)+
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+#panel.grid.major = element_blank()
  theme(title = element_text(size=15))+
  theme(axis.title.x=element_text(size=30))+
  theme(axis.title.y=element_text(size=30))+
  theme(axis.text.x=element_text(size=25))+
  theme(axis.text.y=element_text(size=25))+
  #theme(legend.text=element_text(size=30),legend.title = element_blank(),legend.position="bottom",legend.key = element_blank())+ # Remove only the legend title
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5),"cm"))+
  theme(strip.text.x = element_text(size=30))+
  theme(strip.background = element_rect(fill = 'grey'))
plot_shade

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_all_shade.png", plot = plot_shade, width =15 , height = 9) #To save the plot


##importing all data for shade x treatment
arth_shadeXtreat <- read.csv2("outputs/data/arthropods_shadeXtreat_plot.csv")
pods_shadeXtreat <- read.csv2("outputs/data/pods_shadeXtreat_plot.csv")
#yield_shadeXtreat <- read.csv2("outputs/data/yield_shadeXtreat_plot.csv")#not including yield - not is not reliable

all_shadeXtreat <- rbind(arth_shadeXtreat,pods_shadeXtreat)#,yield_shadeXtreat)
table(all_shadeXtreat$response)

#reorder and rename levels
all_shadeXtreat$response <- factor(all_shadeXtreat$response, levels = c("Hemiptera (*)","Orthoptera (*)","Flowers (*)","Medium pods *","Large pods *"))#,"Harvested pods (*)","Dry weight (g) *","Wet weight (g)*")

#keeping variables I want
data_shadeXtreat <- all_shadeXtreat[all_shadeXtreat$response %in% c("Hemiptera (*)","Orthoptera (*)"),]

head(data_shadeXtreat)
plot_shadeXtreat <- ggplot()+
  geom_line(data_shadeXtreat,mapping=aes(x=shade_treelevel ,y=predicted,colour=group))+
  geom_ribbon(data_shadeXtreat,mapping=aes(x=shade_treelevel,ymin=conf.low,ymax=conf.high,fill=group),alpha=.2)+
  facet_wrap(~response,scales="free_y",nrow=3,ncol=3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Abundance (tree/month)",x="Cacao tree shade cover", title="")+
  theme_bw() + 
  expand_limits(y=0)+
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+#panel.grid.major = element_blank()
  theme(title = element_text(size=15))+
  theme(axis.title.x=element_text(size=25))+
  theme(axis.title.y=element_text(size=25))+
  theme(axis.text.x=element_text(size=25))+
  theme(axis.text.y=element_text(size=25))+
  theme(legend.text=element_text(size=25),legend.title = element_blank(),legend.position="none",legend.key = element_blank())+ # Remove only the legend title
  theme(plot.margin = unit(c(0,5,1.5,5),"cm"))+
  theme(strip.text.x = element_text(size=30))+
  theme(strip.background = element_rect(fill = 'grey'))
plot_shadeXtreat

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_arth_shadeXtreat.png", plot = plot_shadeXtreat, width =14 , height = 7) #To save the plot


#Combining treatment plot with shadextreatment plot
#I need to load modelling scripts to upload plot
library(ggpubr)

combine_arth <- ggarrange(plot_arth, plot_shadeXtreat, labels = c("A","B"), ncol=1, nrow=2, common.legend = FALSE, font.label = list(size = 25,face="bold"), heights = c(2.5,1),align="v")
combine_arth

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_arth_final.png", plot = combine_arth, width =14 , height = 18) #To save the plot


##keeping variables I want
data_shadeXtreat1 <- all_shadeXtreat[all_shadeXtreat$response %in% c("Flowers (*)","Large pods *"),]

head(data_shadeXtreat1)
plot_shadeXtreat1 <- ggplot()+
  geom_line(data_shadeXtreat1,mapping=aes(x=shade_treelevel ,y=predicted,colour=group))+
  geom_ribbon(data_shadeXtreat1,mapping=aes(x=shade_treelevel,ymin=conf.low,ymax=conf.high,fill=group),alpha=.2)+
  facet_wrap(~response,scales="free_y",nrow=3,ncol=3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Counts (tree/month)",x="Cacao tree shade cover", title="")+
  theme_bw() + 
  expand_limits(y=0)+
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+#panel.grid.major = element_blank()
  theme(title = element_text(size=15))+
  theme(axis.title.x=element_text(size=25))+
  theme(axis.title.y=element_text(size=25))+
  theme(axis.text.x=element_text(size=25))+
  theme(axis.text.y=element_text(size=25))+
  theme(legend.text=element_text(size=25),legend.title = element_blank(),legend.position="none",legend.key = element_blank())+ # Remove only the legend title
  theme(plot.margin = unit(c(0,1.5,1.5,1.5),"cm"))+
  theme(strip.text.x = element_text(size=30))+
  theme(strip.background = element_rect(fill = 'grey'))
plot_shadeXtreat1

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_pods_shadeXtreat.png", plot = plot_shadeXtreat1, width =14 , height = 7) #To save the plot

###Combining treatment plot with shadextreatment plot
#I need to load modelling scripts to upload plot
combine_pods <- ggarrange(plot, plot_shadeXtreat1, labels = c("A","B"), ncol=1, nrow=2, common.legend = FALSE, font.label = list(size = 25,face="bold"),align="v")
combine_pods

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_pods_final.png", plot = combine_pods, width =11 , height = 10) #To save the plot



