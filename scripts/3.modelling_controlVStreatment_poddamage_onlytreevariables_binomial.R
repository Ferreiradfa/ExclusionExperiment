library(glmmTMB)
library(ggplot2)
library(ggthemes)
library(dplyr)

###importing pods and flowers data
pods <- read.csv2("outputs/data/pods_flowers_data_final.csv")
head(pods)
dim(pods)

#' ### STATISTICAL ANALYSES
#' Pods
#' creating data frame with selected predictors 
#' not using prop_alldamage because type of damages are not mutually exclusive - using prop_smalldamage: combination of IR and CherelleWilt
colnames(pods)
poddata<-pods[c("PodSmallAll","TotalPods","prop_smalldamage","prop_CherelleWilt","prop_IR","prop_BPD","prop_PestFeeding","prop_DBT","Farm","Tree","Treatment","Visit","shade_cover","shade_treelevel","biomass_tree","DiameterBranches")]#we decided to use shade cover for pods, leaves and yeild but it's correlated with forest cover. So maybe I'll need to use shade_treelevel
head(poddata)

##need to scale due to warning in model (only happens when I use lme4 package)
#2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :Model is nearly unidentifiable: very large eigenvalue - Rescale variables?
poddata$shade_cover_sc <- scale(poddata$shade_cover, center = TRUE, scale = TRUE)
poddata$shade_treelevel_sc <- scale(poddata$shade_treelevel, center = TRUE, scale = TRUE)
poddata$biomass_tree_sc <- scale(poddata$biomass_tree, center = TRUE, scale = TRUE)
poddata$DiameterBranches_sc <- scale(poddata$DiameterBranches, center = TRUE, scale = TRUE)
head(poddata)

##removing 1st visit from all trees - adjustment time
poddata <- poddata%>%filter(Visit>1)
head(poddata)

poddata$Treatment <- as.factor(poddata$Treatment)
table(poddata$Treatment)

poddata$Farm <- as.factor(poddata$Farm)
table(poddata$Farm)

poddata$Tree <- as.factor(poddata$Tree)
table(poddata$Tree)

#' I'll need to use proportions as a response variable: https://stats.stackexchange.com/questions/189115/fitting-a-binomial-glmm-glmer-to-a-response-variable-that-is-a-proportion-or-f
#' ## i.prop_smalldamage 
###checking for outliers - I already remove them when organizing the data
hist(poddata$prop_smalldamage)
plot(poddata$prop_smalldamage)

#modeling with binomial distribution
small_dam<-glmmTMB(prop_smalldamage ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),weights=PodSmallAll,family=binomial,data=poddata)
summary(small_dam)

#checking model fit
DHARMa::simulateResiduals(small_dam, plot = TRUE)#awful
DHARMa::testDispersion(small_dam,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(small_dam,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(small_dam, plot = TRUE)#a bit zero inflated
DHARMa::testOutliers(small_dam,plot = TRUE)#significant

#plotting only significant results
eff_small_dam<-data.frame(ggeffects::ggpredict(small_dam,"Treatment"))
eff_small_dam$response <- "Cherelle Wilt & IR"

##model selection
drop1(small_dam)#Treatment:shade_treelevel_sc                           
small_dam1 <- update(small_dam,. ~ . -Treatment:shade_treelevel_sc)
anova(small_dam,small_dam1)#keep 1 - p=0.8374
drop1(small_dam1)#biomass_tree_sc                         
small_dam2 <- update(small_dam1,. ~ . -biomass_tree_sc)
anova(small_dam1,small_dam2)#keep 2 - p=0.5088 
drop1(small_dam2)#shade_treelevel_sc                          
small_dam3 <- update(small_dam2,. ~ . -shade_treelevel_sc)
anova(small_dam2,small_dam3)#KEEP 2 - p=0.002498  

##final model
summary(small_dam2)#Treatment and shade_treelevel_sc 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)        -1.09193    0.10724 -10.182  < 2e-16 ***
#   TreatmentExclosure -0.36279    0.08738  -4.152  3.3e-05 ***
#   shade_treelevel_sc  0.14220    0.04585   3.101  0.00193 ** 

#checking model fit
DHARMa::simulateResiduals(small_dam2, plot = TRUE)#awful
DHARMa::testDispersion(small_dam2,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(small_dam2,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(small_dam2, plot = TRUE)#n.s.
DHARMa::testOutliers(small_dam2,plot = TRUE)#significant

##plotting final model
##Treatment
#ggefects works better when I have interactions but want only a single term
effec_small_dam <- data.frame(ggeffects::ggpredict(small_dam2, terms = "Treatment"))

plot_small_dam <- ggplot()+
  geom_point(poddata,mapping=aes(x=Treatment,y=prop_smalldamage),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_small_dam,mapping=aes(x=x ,y=predicted ),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_small_dam,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Proportion",x="Treatment", title="CherelleWilt&IR ***")+
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
plot_small_dam

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_damage_cherelle&IR_treatment.png", plot = plot_small_dam, width =7 , height = 5) #To save the plot

#shade_treelevel_sc
effec_small_dam1 <- data.frame(ggeffects::ggpredict(small_dam2,"shade_treelevel_sc")) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_small_dam1$shade_treelevel <- effec_small_dam1$x * attr(poddata$shade_treelevel_sc, 'scaled:scale') + attr(poddata$shade_treelevel_sc, 'scaled:center')

plot_small_dam1 <- ggplot()+
  geom_line(effec_small_dam1,mapping=aes(x=shade_treelevel ,y=predicted))+
  geom_ribbon(effec_small_dam1,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(poddata,mapping=aes(x=shade_treelevel,y=prop_smalldamage),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Proportion",x="Cacao tree shade cover", title="CherelleWilt&IR **")+
  theme_bw() + 
  expand_limits(y=0)+
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
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
plot_small_dam1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_damage_cherelle&IR_shadetree.png", plot = plot_small_dam1, width =7 , height = 5) #To save the plot

#' ## i. prop_CherelleWilt
###checking for outliers - I already remove them when organizing the data
hist(poddata$prop_CherelleWilt)
plot(poddata$prop_CherelleWilt)

#modeling with binomial distribution
cherelle_dam<-glmmTMB(prop_CherelleWilt ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),weights=PodSmallAll,family=binomial,data=poddata)
summary(cherelle_dam)

#checking model fit
DHARMa::simulateResiduals(cherelle_dam, plot = TRUE)#awful
DHARMa::testDispersion(cherelle_dam,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(cherelle_dam,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(cherelle_dam, plot = TRUE)#n.s.
DHARMa::testOutliers(cherelle_dam,plot = TRUE)#significant

#plotting only significant results
eff_cherelle_dam<-data.frame(ggeffects::ggpredict(cherelle_dam,"Treatment"))
eff_cherelle_dam$response <- "Cherelle Wilt"

##model selection
drop1(cherelle_dam)#Treatment:shade_treelevel_sc                        
cherelle_dam1 <- update(cherelle_dam,. ~ . -Treatment:shade_treelevel_sc)
anova(cherelle_dam,cherelle_dam1)#keep 1 - p=0.1527
drop1(cherelle_dam1)#shade_treelevel_sc                       
cherelle_dam2 <- update(cherelle_dam1,. ~ . -shade_treelevel_sc)
anova(cherelle_dam1,cherelle_dam2)#keep 2 - p=0.7723 
drop1(cherelle_dam2)#shade_treelevel_sc                          
cherelle_dam3 <- update(cherelle_dam2,. ~ . -Treatment)
anova(cherelle_dam2,cherelle_dam3)#KEEP 2 - p=0.08792   

##final model
summary(cherelle_dam2)#Treatment and biomass
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)         -1.6784     0.2440  -6.880    6e-12 ***
#   TreatmentExclosure  -0.2772     0.1636  -1.695   0.0901 .  
#   biomass_tree_sc     -0.1873     0.1118  -1.674   0.0941 . 

#checking model fit
DHARMa::simulateResiduals(cherelle_dam2, plot = TRUE)#awful
DHARMa::testDispersion(cherelle_dam2,alternative="greater",plot=TRUE)#almost overdispersed
DHARMa::testDispersion(cherelle_dam2,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(cherelle_dam2, plot = TRUE)#n.s.
DHARMa::testOutliers(cherelle_dam2,plot = TRUE)#significant

##plotting final model
##Treatment
#ggefects works better when I have interactions but want only a single term
effec_cherelle_dam <- data.frame(ggeffects::ggpredict(cherelle_dam2, terms = "Treatment"))

plot_cherelle_dam <- ggplot()+
  geom_point(poddata,mapping=aes(x=Treatment,y=prop_CherelleWilt),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_cherelle_dam,mapping=aes(x=x ,y=predicted ),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_cherelle_dam,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Proportion",x="Treatment", title="Cherelle Wilt .")+
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
plot_cherelle_dam

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_damage_cherelle_treatment.png", plot = plot_cherelle_dam, width =7 , height = 5) #To save the plot

#biomass_tree_sc
effec_cherelle_dam1 <- data.frame(ggeffects::ggpredict(cherelle_dam2,"biomass_tree_sc")) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_cherelle_dam1$biomass_tree <- effec_cherelle_dam1$x * attr(poddata$biomass_tree_sc, 'scaled:scale') + attr(poddata$biomass_tree_sc, 'scaled:center')

plot_cherelle_dam1 <- ggplot()+
  geom_line(effec_cherelle_dam1,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_cherelle_dam1,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(poddata,mapping=aes(x=biomass_tree,y=prop_CherelleWilt),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Proportion",x="Cacao tree biomass", title="Cherelle Wilt .")+
  theme_bw() + 
  expand_limits(y=0)+
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
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
plot_cherelle_dam1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_damage_cherelle_biomass.png", plot = plot_cherelle_dam1, width =7 , height = 5) #To save the plot

#' ## i. prop_IR
###checking for outliers - I already remove them when organizing the data
hist(poddata$prop_IR)
plot(poddata$prop_IR)

#modeling with binomial distribution
IR_dam<-glmmTMB(prop_IR ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),weights=PodSmallAll,family=binomial,data=poddata)
summary(IR_dam)

#checking model fit
DHARMa::simulateResiduals(IR_dam, plot = TRUE)#awful
DHARMa::testDispersion(IR_dam,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(IR_dam,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(IR_dam, plot = TRUE)#significant
DHARMa::testOutliers(IR_dam,plot = TRUE)#significant

#plotting only significant results
eff_IR_dam<-data.frame(ggeffects::ggpredict(IR_dam,"Treatment"))
eff_IR_dam$response <- "IR"

##model selection
drop1(IR_dam)#Treatment:shade_treelevel_sc                           
IR_dam1 <- update(IR_dam,. ~ . -Treatment:shade_treelevel_sc)
anova(IR_dam,IR_dam1)#keep 1 - p=0.7205
drop1(IR_dam1)#Treatment:shade_treelevel_sc                          
IR_dam2 <- update(IR_dam1,. ~ . -biomass_tree_sc)
anova(IR_dam1,IR_dam2)#keep 2 - p=0.5559 
drop1(IR_dam2)#shade_treelevel_sc                          
IR_dam3 <- update(IR_dam2,. ~ . -shade_treelevel_sc)
anova(IR_dam2,IR_dam3)#keep 3 - p=0.1303  
drop1(IR_dam3)#Treatment
IR_dam4 <- update(IR_dam3,. ~ . -Treatment)
anova(IR_dam3,IR_dam4)#KEEP 3 - p=0.02788   

##final model
summary(IR_dam3)#Treatment
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)         -2.5664     0.2215 -11.589   <2e-16 ***
#   TreatmentExclosure  -0.7281     0.3114  -2.338   0.0194 *  

#checking model fit
DHARMa::simulateResiduals(IR_dam3, plot = TRUE)#awful
DHARMa::testDispersion(IR_dam3,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(IR_dam3,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(IR_dam3, plot = TRUE)#significant
DHARMa::testOutliers(IR_dam3,plot = TRUE)#significant

##plotting final model
##Treatment
#ggefects works better when I have interactions but want only a single term
effec_IR_dam <- data.frame(ggeffects::ggpredict(IR_dam3, terms = "Treatment"))

plot_IR_dam <- ggplot()+
  geom_point(poddata,mapping=aes(x=Treatment,y=prop_IR),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_IR_dam,mapping=aes(x=x ,y=predicted),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_IR_dam,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Proportion",x="Treatment", title="IR *")+
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
plot_IR_dam

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_IR_treatment.png", plot = plot_IR_dam, width =7 , height = 5) #To save the plot

#' ## i. prop_BPD
###checking for outliers - I already remove them when organizing the data
hist(poddata$prop_BPD)
plot(poddata$prop_BPD)

#modeling with NB distribution
BPD_dam<-glmmTMB(prop_BPD ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),weights=TotalPods,family=binomial,data=poddata)
summary(BPD_dam)

#checking model fit
DHARMa::simulateResiduals(BPD_dam, plot = TRUE)#awful
DHARMa::testDispersion(BPD_dam,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(BPD_dam,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(BPD_dam, plot = TRUE)#significant
DHARMa::testOutliers(BPD_dam,plot = TRUE)#significant

#plotting only significant results
eff_BPD_dam<-data.frame(ggeffects::ggpredict(BPD_dam,"Treatment"))
eff_BPD_dam$response <- "BPD"

##model selection
drop1(BPD_dam)#Treatment:shade_treelevel_sc                           
BPD_dam1 <- update(BPD_dam,. ~ . -Treatment:shade_treelevel_sc)
anova(BPD_dam,BPD_dam1)#keep 1 - p=0.373
drop1(BPD_dam1)#shade_treelevel_sc                          
BPD_dam2 <- update(BPD_dam1,. ~ . -shade_treelevel_sc)
anova(BPD_dam1,BPD_dam2)#keep 2 - p=0.8108 
drop1(BPD_dam2)#Treatment                          
BPD_dam3 <- update(BPD_dam2,. ~ . -Treatment)
anova(BPD_dam2,BPD_dam3)#keep 3 - p=0.6429  
drop1(BPD_dam3)#biomass_tree_sc                            
BPD_dam4 <- update(BPD_dam3,. ~ . -biomass_tree_sc)
anova(BPD_dam3,BPD_dam4)#keep 2 - p=0.1346  
#keep nul model

##final model
summary(BPD_dam4)#NULL MODEL
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -2.1690     0.1573  -13.79   <2e-16 ***

#checking model fit
DHARMa::simulateResiduals(BPD_dam4, plot = TRUE)#awful
DHARMa::testDispersion(BPD_dam4,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(BPD_dam4,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(BPD_dam4, plot = TRUE)#significant
DHARMa::testOutliers(BPD_dam4,plot = TRUE)#significant

#' ## i. prop_PestFeeding
###checking for outliers - I already remove them when organizing the data
hist(poddata$prop_PestFeeding)
plot(poddata$prop_PestFeeding)

#modeling with NB distribution
pest_dam<-glmmTMB(prop_PestFeeding ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),weights=TotalPods,family=binomial,data=poddata)
summary(pest_dam)

#checking model fit
DHARMa::simulateResiduals(pest_dam, plot = TRUE)#awful
DHARMa::testDispersion(pest_dam,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(pest_dam,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(pest_dam, plot = TRUE)#significant
DHARMa::testOutliers(pest_dam,plot = TRUE)#significant

#plotting only significant results
eff_pest_dam<-data.frame(ggeffects::ggpredict(pest_dam,"Treatment"))
eff_pest_dam$response <- "Pest feeding"

##model selection
drop1(pest_dam)#biomass_tree_sc                           
pest_dam1 <- update(pest_dam,. ~ . -biomass_tree_sc)
anova(pest_dam,pest_dam1)#keep 1 - p=0.9406
drop1(pest_dam1)#Treatment:shade_treelevel_sc                          
pest_dam2 <- update(pest_dam1,. ~ . -Treatment:shade_treelevel_sc)
anova(pest_dam1,pest_dam2)#keep 2 - p=0.2844 
drop1(pest_dam2)#Treatment                          
pest_dam3 <- update(pest_dam2,. ~ . -Treatment)
anova(pest_dam2,pest_dam3)#keep 3 - p=0.302  
drop1(pest_dam3)#shade_treelevel_sc                          
pest_dam4 <- update(pest_dam3,. ~ . -shade_treelevel_sc)
anova(pest_dam3,pest_dam4)#KEEP 3 - p=0.01003   

##final model
summary(pest_dam3)#shade_treelevel_sc 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)   
#   (Intercept)         -0.4078     0.1408  -2.896  0.00378 **
#   shade_treelevel_sc  -0.3322     0.1231  -2.698  0.00697 **

#checking model fit
DHARMa::simulateResiduals(pest_dam3, plot = TRUE)#bad
DHARMa::testDispersion(pest_dam3,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(pest_dam3,alternative ="less",plot = TRUE)#n.s. 
DHARMa::testZeroInflation(pest_dam3, plot = TRUE)#n.s.
DHARMa::testOutliers(pest_dam3,plot = TRUE)#n.s.

##plotting final model
#shade_treelevel_sc
effec_pest_dam <- data.frame(ggeffects::ggpredict(pest_dam3,c("shade_treelevel_sc"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_pest_dam$shade_treelevel <- effec_pest_dam$x * attr(poddata$shade_treelevel_sc, 'scaled:scale') + attr(poddata$shade_treelevel_sc, 'scaled:center')

plot_pest_dam <- ggplot()+
  geom_line(effec_pest_dam,mapping=aes(x=shade_treelevel ,y=predicted))+
  geom_ribbon(effec_pest_dam,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(poddata,mapping=aes(x=shade_treelevel,y=prop_PestFeeding),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Proportion",x="Cacao tree shade cover", title="Pest feeding **")+
  theme_bw() + 
  expand_limits(y=0)+
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
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
plot_pest_dam

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_pest_dam_shadetree.png", plot = plot_pest_dam, width =7 , height = 5) #To save the plot

#' ## i. prop_DBT
###checking for outliers - I already remove them when organizing the data
hist(poddata$prop_DBT)
plot(poddata$prop_DBT)

#modeling with NB distribution
DBT_dam<-glmmTMB(prop_DBT ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree),weights=TotalPods,family=binomial,data=poddata)
summary(pest_dam)

#checking model fit
DHARMa::simulateResiduals(DBT_dam, plot = TRUE)#okay
DHARMa::testDispersion(DBT_dam,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(DBT_dam,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(DBT_dam, plot = TRUE)#n.s.
DHARMa::testOutliers(DBT_dam,plot = TRUE)#n.s.

#plotting only significant results
eff_DBT_dam<-data.frame(ggeffects::ggpredict(DBT_dam,"Treatment"))
eff_DBT_dam$response <- "DBT"

##model selection
drop1(DBT_dam)#biomass_tree_sc                           
DBT_dam1 <- update(DBT_dam,. ~ . -biomass_tree_sc)
anova(DBT_dam,DBT_dam1)#keep 1 - p=0.3429
drop1(DBT_dam1)#Treatment:shade_treelevel_sc                          
DBT_dam2 <- update(DBT_dam1,. ~ . -Treatment:shade_treelevel_sc)
anova(DBT_dam1,DBT_dam2)#keep 2 - p=0.1938 
drop1(DBT_dam2)#Treatment                          
DBT_dam3 <- update(DBT_dam2,. ~ . -Treatment)
anova(DBT_dam2,DBT_dam3)#keep 3 - p=0.2276  
drop1(DBT_dam3)#shade_treelevel_sc                          
DBT_dam4 <- update(DBT_dam3,. ~ . -shade_treelevel_sc)
anova(DBT_dam3,DBT_dam4)#KEEP 3 - p=0.05154    

##final model
summary(DBT_dam3)#shade_treelevel_sc 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)         -4.8357     0.3236 -14.942   <2e-16 ***
#   shade_treelevel_sc  -0.5767     0.3002  -1.921   0.0547 . 

#checking model fit
DHARMa::simulateResiduals(DBT_dam3, plot = TRUE)#okay
DHARMa::testDispersion(DBT_dam3,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(DBT_dam3,alternative ="less",plot = TRUE)#n.s. 
DHARMa::testZeroInflation(DBT_dam3, plot = TRUE)#n.s.
DHARMa::testOutliers(DBT_dam3,plot = TRUE)#n.s.

##plotting final model
#shade_treelevel_sc
effec_DBT_dam <- data.frame(ggeffects::ggpredict(DBT_dam3,c("shade_treelevel_sc"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_DBT_dam$shade_treelevel <- effec_DBT_dam$x * attr(poddata$shade_treelevel_sc, 'scaled:scale') + attr(poddata$shade_treelevel_sc, 'scaled:center')

plot_DBT_dam <- ggplot()+
  geom_line(effec_DBT_dam,mapping=aes(x=shade_treelevel ,y=predicted))+
  geom_ribbon(effec_DBT_dam,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(poddata,mapping=aes(x=shade_treelevel,y=prop_DBT),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Proportion",x="Cacao tree shade cover", title="DBT .")+
  theme_bw() + 
  expand_limits(y=0)+
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
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
plot_DBT_dam

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_DBT_dam_shadetree.png", plot = plot_DBT_dam, width =7 , height = 5) #To save the plot

####'Plotting
#'Combining all data for plotting
eff_pods <- rbind(eff_small_dam,eff_cherelle_dam,eff_IR_dam,eff_BPD_dam,eff_pest_dam,eff_DBT_dam)

#reorder levels
library(forcats)
eff_pods$response <- as.factor(eff_pods$response)

#reorder and rename levels
eff_pods$response <- factor(eff_pods$response, levels = c("Cherelle Wilt & IR","Cherelle Wilt","IR","BPD","Pest feeding","DBT"))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.5)

plot <- ggplot(eff_pods,aes(x=x, y=predicted , colour=x))+
  geom_errorbar(aes(ymin=predicted-std.error, ymax=predicted+std.error),width=.2,position=pd)+
  geom_line(position=pd)+
  geom_point(aes(shape=x),position=pd,size=6)+
  facet_wrap(~response,scales="free_y",nrow=5,ncol=3)+
  #scale_shape(solid=F)+ # no fill for points
  xlab("")+ ylab("Predicted proportions")+
  theme_bw() + 
  scale_colour_colorblind()+
  expand_limits(y=0)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x=element_text(size=20,vjust=-1))+
  theme(axis.title.y=element_text(size=30,angle=90,vjust=2))+
  theme(axis.text.x=element_text(size=25,face="bold",vjust=0.5))+
  theme(axis.text.y=element_text(size=25))+
  theme(legend.text=element_text(size=30),legend.title = element_blank(),legend.position="bottom",legend.key = element_blank())+ # Remove only the legend title
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5),"cm"))+
  theme(strip.text.x = element_text(face="italic",size=30))+
  theme(strip.background = element_rect(fill = 'grey'))+
  geom_vline(xintercept=c(1.5),color="red", linetype="dotted",size=1)#create lines between sone variables
plot

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_fullmodel_poddamage.png", plot = plot, width =15 , height = 8) #To save the plot
