library(dplyr)
library(glmmTMB)
library(ggplot2)
library(ggthemes)
library(DHARMa)

###importing herbivory data
herbivory <- read.csv2("outputs/data/herbivory_data_final.csv")
head(herbivory)
dim(herbivory)

#' ### STATISTICAL ANALYSES
#' #Pods
##creating data frame with selected predictors
colnames(herbivory)
herbdata<-herbivory[c("leaf_number","area_diff","visit_diff","Farm","Tree","Treatment","shade_cover","shade_treelevel","biomass_tree","DiameterBranches")]#we decided to use shade cover for pods, leaves and yeild but it's correlated with forest cover. So maybe I'll need to use shade_treelevel
head(herbdata)

##need to scale due to warning in model (only happens when I use lme4 package)
#2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :Model is nearly unidentifiable: very large eigenvalue - Rescale variables?
herbdata$shade_cover_sc <- scale(herbdata$shade_cover, center = TRUE, scale = TRUE)
herbdata$shade_treelevel_sc <- scale(herbdata$shade_treelevel, center = TRUE, scale = TRUE)
herbdata$biomass_tree_sc <- scale(herbdata$biomass_tree, center = TRUE, scale = TRUE)
herbdata$DiameterBranches_sc <- scale(herbdata$DiameterBranches, center = TRUE, scale = TRUE)
head(herbdata)

herbdata$Treatment <- as.factor(herbdata$Treatment)
table(herbdata$Treatment)

herbdata$Farm <- as.factor(herbdata$Farm)
table(herbdata$Farm)

herbdata$Tree <- as.factor(herbdata$Tree)
table(herbdata$Tree)

herbdata$leaf_number <- as.factor(herbdata$leaf_number)
table(herbdata$leaf_number)

#adding 0.00001 to all value because beta and gamma don't work with zeros (could also use zero inflation (ziformula = ~1) but my zeros don't mean absence so I guess 0.00001 or 0 will be basically the same)
herbdata$area_diff_new  <- herbdata$area_diff + 0.001
head(herbdata)

#transform my percentage into proportions 
herbdata$prop_diff  <- herbdata$area_diff/100
head(herbdata)

#transform my percentage with no zeros into proportions 
herbdata$prop_diff_new  <- herbdata$area_diff_new/100
head(herbdata)

#log transform my effort - when using the predict function I have an error if I transform the effort directly in the model's formula 
herbdata$effort  <- log(herbdata$visit_diff)
head(herbdata)

#' proportions don't come from counts (no sample size,i.e, success and failure) so i need use a bet distributions: https://stats.stackexchange.com/questions/48028/beta-regression-of-proportion-data-including-1-and-0
#' ## i. Herbivory
###checking for outliers - I already remove them when organising the data
plot(herbdata$prop_diff)
hist(herbdata$prop_diff)

#checking distribution
library(fitdistrplus)
descdist(herbdata$prop_diff)#using beta but also testing gamma


#modeling with beta distribution - 
#should I include an offset to compensate for duration of each leaf?
plot(herbdata$visit_diff,herbdata$area_diff)#I don't see any relation between time and area difference - so no using offset

herb<-glmmTMB(prop_diff_new ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),family=beta_family(link = "logit"),data=herbdata)
summary(herb)

#checking model fit
DHARMa::simulateResiduals(herb, plot = TRUE)#awful
DHARMa::testDispersion(herb,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(herb,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(herb, plot = TRUE)#n.s.
DHARMa::testOutliers(herb,plot = TRUE)#n.s.

#beta zero inflated
herb_zero<-glmmTMB(prop_diff ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),family=beta_family(link = "logit"),ziformula = ~1, data=herbdata)
summary(herb_zero)

#checking model fit
DHARMa::simulateResiduals(herb_zero, plot = TRUE)#awful
DHARMa::testDispersion(herb_zero,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(herb_zero,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(herb_zero, plot = TRUE)#n.s.
DHARMa::testOutliers(herb_zero,plot = TRUE)#n.s.

#gamma family
gamma_herb<-glmmTMB(prop_diff_new ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),family=Gamma,data=herbdata)
summary(gamma_herb)

#plot & test the residual
qqnorm(resid(gamma_herb))
qqline(resid(gamma_herb))
shapiro.test(resid(gamma_herb))
#W = 0.54379, p-value < 2.2e-16
plot(resid(gamma_herb))#we're plotting the residuals vs observed
hist(resid(gamma_herb),20)


#log normal family
log_herb<-glmmTMB(prop_diff_new ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),family=gaussian(link=log),data=herbdata)
summary(log_herb)

#plot & test the residual
qqnorm(resid(log_herb))
qqline(resid(log_herb))
shapiro.test(resid(log_herb))
#W = 0.50875, p-value < 2.2e-16
plot(resid(log_herb)) #we're plotting the residuals vs observered
hist(resid(log_herb),20)

#Compare the four models 
anova(herb,herb_zero,gamma_herb,log_herb,test="LRT")
#herb seems to be the best -but it's overdispersed
#using herb_zero because results are the same, seems to make more sense accroding to what I read and p-values are smaller (overdispersed causes big p-values)

##model selection
drop1(herb_zero)#biomass_tree_sc                           
herb1 <- update(herb_zero,. ~ . -biomass_tree_sc)
anova(herb_zero,herb1)#keep 1 - p=0.2711
drop1(herb1)#Treatment:shade_treelevel_sc                  
herb2 <- update(herb1,. ~ . -Treatment:shade_treelevel_sc)
anova(herb1,herb2)#keep 2 - p=0.7546 
drop1(herb2)#shade_treelevel_sc                          
herb3 <- update(herb2,. ~ . -shade_treelevel_sc)
anova(herb2,herb3)#keep 3 - p=0.4968    
drop1(herb3)#Treatment                          
herb4 <- update(herb3,. ~ . -Treatment)
anova(herb3,herb4)#KEEP 3 - p=0.06808   

##final model
summary(herb3)#Treatment
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)         -3.7016     0.1344 -27.534   <2e-16 ***
#   TreatmentExclosure  -0.1924     0.1028  -1.872   0.0613 .  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#checking model fit
DHARMa::simulateResiduals(herb3, plot = TRUE)#awful
DHARMa::testDispersion(herb3,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(herb3,alternative ="less",plot = TRUE)#n.s. 
DHARMa::testZeroInflation(herb3, plot = TRUE)#n.s.
DHARMa::testOutliers(herb3,plot = TRUE)#n.s.

#plotting only significant results
eff_herb<-data.frame(ggeffects::ggpredict(herb3,"Treatment"))
eff_herb$response <- "Herbivory (*)"

##plotting final model
##Treatment
#ggefects works better when I have interactions but want only a single term
effec_herb <- data.frame(ggeffects::ggpredict(herb2, terms = "Treatment"))

plot_herb <- ggplot()+
  geom_point(herbdata,mapping=aes(x=Treatment,y=prop_diff),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_herb,mapping=aes(x=x ,y=predicted),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_herb,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Proportion",x="Treatment", title="Herbivory (*)")+
  theme_bw() +
  expand_limits(y=0)+
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))
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
plot_herb

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_herbivory_treatment.png", plot = plot_herb, width =7 , height = 5) #To save the plot

####'Plotting full model - Treatment
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.5)

plot_herb_final <- ggplot(eff_herb,aes(x=x, y=predicted, colour=x))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),width=.2,position=pd)+
  geom_line(position=pd)+
  geom_point(aes(shape=x),position=pd,size=6)+
  facet_wrap(~response,scales="free_y",nrow=5,ncol=3)+
  #scale_shape(solid=F)+ # no fill for points
  xlab("")+ ylab("Proportion\n leaf area loss")+
  theme_bw() + 
  scale_colour_colorblind()+
  expand_limits(y=0)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x=element_text(size=20,vjust=-1))+
  theme(axis.title.y=element_text(size=30,angle=90,vjust=2))+
  theme(axis.text.x=element_text(size=25,face="bold",vjust=0.5))+
  theme(axis.text.y=element_text(size=25))+
  theme(legend.text=element_text(size=30),legend.title = element_blank(),legend.position="none",legend.key = element_blank())+ # Remove only the legend title
  theme(plot.margin = unit(c(1,5,0,5),"cm"))+
  theme(strip.text.x = element_text(size=30))+
  theme(strip.background = element_rect(fill = 'grey'))+
  geom_vline(xintercept=c(1.5),color="red", linetype="dotted",size=1)#create lines between sone variables
plot_herb_final

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_treatment_herbivory.png", plot = plot_herb_final, width =8 , height = 6) #To save the plot

###Combining herbivory plot with pod damage
#I need to load modelling scripts to upload plot
library(ggpubr)

#I need to open pod damage script and run last plot
combine_herb <- ggarrange(plot, plot_herb_final, labels = c("A","B"), ncol=1, nrow=2, common.legend = FALSE, font.label = list(size = 25,face="bold"), heights = c(2,1),align="hv")
combine_herb

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_herb&dama_final.png", plot = combine_herb, width =10 , height = 14) #To save the plot

