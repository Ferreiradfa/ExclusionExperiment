library(dplyr)
library(DHARMa)

###importing yield data
yield <- read.csv2("outputs/data/yield_data_final.csv")
head(yield)
dim(yield)

#' #Yield
##removing visits where we did not collected any pods. And keeping relevant columns
colnames(yield)
#aggregating by TREE (where data is nit by visit)
yielddata<- yield %>%
  select(Farm,Treatment,Tree,Pod,Length.cm.,Diameter.cm.,Wet_weight.g.,Dry_weight.g.)%>%
  filter(Pod>=1,!Pod=="Pods were collected by farmer",Wet_weight.g.>0)
head(yielddata)


##correlation between size and yield 
#size categories: small (0 to 9), medium (9 to 14) and big (>14)
#because our categories are based on length I need to use Length.cm. with Dry_weight.g.

##all pods
colnames(yielddata)
corr_all <- round(cor(yielddata[c(5:8)], method="spearman", use="pairwise.complete.obs"),2)
corr_all#0.49

#correlation test
res_all <- cor.mtest(corr_all, conf.level = .95)

library(corrplot)
png(file="outputs/plots/yield to money/correlations_yield_length_all.png", width = 20, height = 10, units = 'in', res = 300)
corrplot(corr_all, method = "square", type = 'upper', order = "original",tl.col = "black", tl.srt = 45, tl.cex = 2, bg = "white",cl.pos = "r", cl.ratio = 0.05,cl.cex = 2,cl.align.text="l", number.cex = .5,p.mat = res_all$p, sig.level = c(.001, .01, .05), pch.cex = .9, insig = "label_sig", pch.col = "black")
dev.off()

##only control pods
corr_control <- round(cor(yielddata[yielddata$Treatment == "Control",c(5:8)], method="spearman", use="pairwise.complete.obs"),2)
corr_control#0.56

#correlation test
res_control <- cor.mtest(corr_control, conf.level = .95)

png(file="outputs/plots/yield to money/correlations_yield_length_control.png", width = 20, height = 10, units = 'in', res = 300)
corrplot(corr_control, method = "square", type = 'upper', order = "original",tl.col = "black", tl.srt = 45, tl.cex = 2, bg = "white",cl.pos = "r", cl.ratio = 0.05, cl.cex = 2,cl.align.text="l", number.cex = .5,p.mat = res_control$p, sig.level = c(.001, .01, .05), pch.cex = .9, insig = "label_sig", pch.col = "black")
dev.off()

##only exclosures pods
corr_exclosure <- round(cor(yielddata[yielddata$Treatment == "Exclosure",c(5:8)], method="spearman", use="pairwise.complete.obs"),2)
corr_exclosure#0.4

#correlation test
res_exclosure <- cor.mtest(corr_exclosure, conf.level = .95)

png(file="outputs/plots/yield to money/correlations_yield_length_exclosure.png", width = 20, height = 10, units = 'in', res = 300)
corrplot(corr_exclosure, method = "square", type = 'upper', order = "original",tl.col = "black", tl.srt = 45, tl.cex = 0.8, bg = "white",cl.pos = "r", cl.ratio = 0.05, number.cex = .5,p.mat = res_exclosure$p, sig.level = c(.001, .01, .05), pch.cex = .9, insig = "label_sig", pch.col = "black")
dev.off()



####modeling
#using all data - or shouls I only use control data? 
##checking for outliers 
plot(yielddata$Dry_weight.g.)
hist(yielddata$Dry_weight.g.)
plot(yielddata$Length.cm.)
hist(yielddata$Length.cm.)

library(ggplot2)
ggplot(yielddata) +
  aes(x = "", y = Dry_weight.g.) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#identifying outliers
boxplot.stats(yielddata$Dry_weight.g.)$out
#8,6,5,6,18,18,7,20,7,23

#removing outlier
yielddata_clean <- yielddata[!yielddata$Dry_weight.g. %in% c(8,6,5,18,7,20,23),] 

library(ggplot2)
ggplot(yielddata_clean) +
  aes(x = "", y = Dry_weight.g.) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#identifying outliers
boxplot.stats(yielddata_clean$Dry_weight.g.)$out
dim(yielddata_clean)#still have 264 rows

#modeling with gaussian distribution
dry<-glm(Dry_weight.g. ~ Length.cm., family = gaussian, data=yielddata_clean)
summary(dry)

#plot & test the residual
plot(dry)#i think I have a few outliers: 146, 133 e 135
qqnorm(resid(dry))#good
qqline(resid(dry))
shapiro.test(resid(dry))#good 
#W = 0.99348, p-value = 0.3081

DHARMa::simulateResiduals(dry, plot = TRUE)#okay
DHARMa::testDispersion(dry,alternative="greater",plot = TRUE)#n.s.
DHARMa::testDispersion(dry,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(dry, plot = TRUE)#n.s.
DHARMa::testOutliers(dry,plot = TRUE)#n.s.

##plotting final model
effec_dry <- data.frame(ggeffects::ggpredict(dry,c("Length.cm.")))#Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.

plot_dry <- ggplot()+
  geom_line(effec_dry,mapping=aes(x=x ,y=predicted))+
  geom_ribbon(effec_dry,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),alpha=.2)+
  geom_point(yielddata_clean,mapping=aes(x=Length.cm.,y=Dry_weight.g.),alpha=.3)+
  labs(y="Dry weight (g/pod)",x="Pod Length (cm)", title="***")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+#panel.grid.major = element_blank()
  theme(title = element_text(size=15))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  #theme(legend.text=element_text(size=30),legend.title = element_blank(),legend.position="bottom",legend.key = element_blank())+ # Remove only the legend title
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5),"cm"))
#theme(strip.text.x = element_text(face="italic",size=30))+
#theme(strip.background = element_rect(fill = 'grey'))+
#guides(color=guide_legend(title="Season"),fill=FALSE)
plot_dry

ggsave("outputs/plots/yield to money/predict_dryyield_length.png", plot = plot_dry, width =7 , height = 5) #To save the plot

###transforming pod size to yield
#mean length
mean_l <- mean(yielddata_clean$Length.cm.)#mean size of pods - 15.33021

#converting my mean size to dry yield 
effec_mean <- data.frame(ggeffects::ggpredict(dry,c("Length.cm.[15.33021]")))#Model has 
effec_mean
#Length.cm. Dry_weight.g. std.error   conf.low  conf.high group
#15.33021   72.84029      0.8959294   71.0843   74.59628     1

##based on pod/flower modeling we have:
#At ~10% shade cover we have: 
#Controls - 14 flowers and 2 large pods
#Exclosures - 73 flowers and 7 large pods
#At ~90% shade cover we have: 
#Controls - 31 flowers and 5 large pods
#Exclosures - 8 flowers and 2 large pods

#I'll assume that the mean of the pods that I collected are a good representations of the large pods found in our farm
#For the mean pods size I have 72.84029 g of dry beans with a CI low of 71.0843 and a CI high of 74.59628
#~10% shade:
#Control
72.84029*2#145.6806 mean dry weight
71.0843*2#142.1686 low CI
74.59628*2#149.1926 high CI
#Exclosures
72.84029*7#509.882 mean dry weight
71.0843*7#497.5901 low CI
74.59628*7#522.174 high CI

#~90% shade:
#Control
72.84029*5#364.2014 mean dry weight
71.0843*5#355.4215 low CI
74.59628*5#372.9814 high CI
#Exclosure
72.84029*2#145.6806 mean dry weight
71.0843*2#142.1686 low CI
74.59628*2#149.1926 high CI

