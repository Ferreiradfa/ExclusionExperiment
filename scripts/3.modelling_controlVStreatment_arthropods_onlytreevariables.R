library(dplyr)
library(glmmTMB)
library(ggplot2)
library(ggthemes)


###importing arthropods data
arthropods <- read.csv2("outputs/data/arthropods_data_final.csv")
head(arthropods)
dim(arthropods)

#' ### STATISTICAL ANALYSES
#checking what arthropods groups I'll model
colnames(arthropods)

#total abundance
apply(arthropods[8:26],2,sum, na.rm=TRUE)
#Keeping: 
#Hemiptera  Lepidoptera  Hymenoptera   Neuroptera    Diptera   Coleoptera #62265          702         1027          310          8065       3183 
#Orthoptera    Arachnida   Formicidae Brown.capsid  Mealybugs   Blatodae 
#250              3779        66948        91          5338       785 
#Mantodae 
#166
#Removing:
#Odonata   Dermaptera    Diplopoda  Gasteropoda  Haplotaxida Squamata 
#15           11           27           17            9         1

#values different from zero
nrow(arthropods)
#375
apply(arthropods[8:26]>0,2,sum, na.rm=TRUE)
#Keeping:
#Hemiptera  Lepidoptera  Hymenoptera   Diptera   Coleoptera  Orthoptera
#346          223          272           357        321        148
#Arachnida   Formicidae     Mealybugs     Mantodae
#338          369             167            112
#Maybe:
#Neuroptera
#76
#Removing:
#Brown.capsid   Blatodae  Odonata   Dermaptera    Diplopoda  Gasteropoda
#18               29        9            7           20           13 
#Haplotaxida    Squamata     
#5                1 

###Doing models for Hemiptera, Lepidoptera, Hymenoptera, Diptera, Coleoptera, Orthoptera, Arachnida, Formicidae, Mealybugs, Mantodae, Neuroptera,Brown.capsid,Blatodae
#76
##creating data frame with selected predictors plus non correlated veg
colnames(arthropods)
model_arth <- arthropods[c("Hemiptera","Lepidoptera","Hymenoptera","Diptera","Coleoptera","Orthoptera","Arachnida","Formicidae","Mealybugs","Mantodae","Neuroptera","Brown.capsid","Blatodae","Farm","Tree","Treatment","Visit","shade_cover","shade_treelevel","biomass_tree","DiameterBranches")]#we know that capsid depend on sun pockets in shaded farms - so shade per tree should be more important than overall shade cover of farm

##need to scale due to warning in model (only happens when I use lme4 package)
#2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :Model is nearly unidentifiable: very large eigenvalue - Rescale variables?
model_arth$shade_cover_sc <- scale(model_arth$shade_cover, center = TRUE, scale = TRUE)
model_arth$shade_treelevel_sc <- scale(model_arth$shade_treelevel, center = TRUE, scale = TRUE)
model_arth$biomass_tree_sc <- scale(model_arth$biomass_tree, center = TRUE, scale = TRUE)
model_arth$DiameterBranches_sc <- scale(model_arth$DiameterBranches, center = TRUE, scale = TRUE)
head(model_arth)

##removing 1st visit from all trees - adjustment time
model_arth <- model_arth%>%filter(Visit>1)
head(model_arth)

model_arth$Treatment <- as.factor(model_arth$Treatment)
model_arth$Farm <- as.factor(model_arth$Farm)
model_arth$Tree <- as.factor(model_arth$Tree)

#' ## i. Hemiptera
###checking for outliers - I already remove them when organising the data
plot(model_arth$Hemiptera)

###Function to check model fitness for selected insect groups
source("scripts/model_fit_21.06.25.R")

#model fit for Hemiptera
formula_hem <- as.formula("Hemiptera ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_hem,model_arth)#Poisson awful, almost overdispersed, zero inflated and has some outliers; best model: GP (okay - AIC = 3640.3) and then NB(bad and zero-inlfated - AIC = 3717.8); results are different between distributions

#modeling with GP distribution
hem<-glmmTMB(formula_hem,family=genpois,data=model_arth)
summary(hem)

##model selection
drop1(hem)#biomass_tree_sc                             
hem1 <- update(hem,. ~ . -biomass_tree_sc)
anova(hem,hem1)#keep 1 - p=0.6503
drop1(hem1)#Treatment:shade_treelevel_sc                           
hem2 <- update(hem1,. ~ . -Treatment:shade_treelevel_sc)
anova(hem1,hem2)#KEEP 1 - p=0.08089 

##final model
summary(hem1)#Treatment and shade_treelevel_sc
# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            4.86874    0.20278  24.010  < 2e-16 ***
# TreatmentExclosure                     0.48771    0.12504   3.900  9.6e-05 ***
# shade_treelevel_sc                     0.07339    0.10447   0.703   0.4823    
# TreatmentExclosure:shade_treelevel_sc -0.25885    0.14442  -1.792   0.0731 .

#checking model fit
DHARMa::simulateResiduals(hem1, plot = TRUE)#good
DHARMa::testDispersion(hem1,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(hem1,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(hem1, plot = TRUE)#n.s.
DHARMa::testOutliers(hem1,plot = TRUE)#n.s.

#plotting Treatment effect for all response variable - see end of script
#https://cran.r-project.org/web/packages/ggeffects/vignettes/ggeffects.html
eff_hem<-data.frame(ggeffects::ggpredict(hem1,"Treatment"))
eff_hem$response <- "Hemiptera ***"
eff_hem$diet <- "Phytophagous"

##plotting final model
#Treatment
effec_hem <- data.frame(ggeffects::ggpredict(hem1,"Treatment"))

plot_hem <- ggplot()+
  geom_point(model_arth,mapping=aes(x=Treatment,y=Hemiptera),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_hem,mapping=aes(x=x ,y=predicted),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_hem,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Treatment", title="Hemiptera ***")+
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
plot_hem

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_hem_treatment.png", plot = plot_hem, width =7 , height = 5) #To save the plot

#Treatment*shade_treelevel_sc
effec_hem1 <- data.frame(ggeffects::ggpredict(hem1,c("shade_treelevel_sc","Treatment"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_hem1$shade_treelevel <- effec_hem1$x * attr(model_arth$shade_treelevel_sc, 'scaled:scale') + attr(model_arth$shade_treelevel_sc, 'scaled:center')

effec_hem1$response <- "Hemiptera (*)"

plot_hem1 <- ggplot()+
  geom_line(effec_hem1,mapping=aes(x=shade_treelevel ,y=predicted,colour=group))+
  geom_ribbon(effec_hem1,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error,fill=group),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=shade_treelevel,y=Hemiptera,colour=Treatment),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree shade cover", title="Hemiptera (*)")+
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
plot_hem1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_hem_shadeXtreatment.png", plot = plot_hem1, width =7 , height = 5) #To save the plot


#' ## i. Lepidoptera
###checking for outliers - I already remove them when organising the data
plot(model_arth$Lepidoptera)

#model fit for Lepidoptera
formula_lep <- as.formula("Lepidoptera~Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_lep,model_arth)#Poisson is bad, overdispersed and a bit zero inflated; best model: NB (good and a bit overdispersed - AIC = 1248.7) and then Zero NB(bad and a bit overdispersed - AIC = 1250.7); results similar between distributions

#modeling with NB distribution
lep<-glmmTMB(formula_lep,family=nbinom2,data=model_arth)
summary(lep)

##model selection
drop1(lep)#Treatment:shade_treelevel_sc                          
lep1 <- update(lep,. ~ . -Treatment:shade_treelevel_sc)
anova(lep,lep1)#keep 1 - p=0.2421
drop1(lep1)#shade_treelevel_sc                     
lep2 <- update(lep1,. ~ . -shade_treelevel_sc)
anova(lep1,lep2)#keep 2 - p=0.6041
drop1(lep2)#Treatment                              
lep3 <- update(lep2,. ~ . -Treatment)
anova(lep2,lep3)#keep 3 - p=0.4358
drop1(lep3)#biomass_tree_sc                     
lep4 <- update(lep3,. ~ . -biomass_tree_sc)
anova(lep3,lep4)#KEEP 3 - p=0.02588 
                                 
##final model
summary(lep3)#biomass_tree_sc  
# Conditional model:
# Estimate Std. Error z value Pr(>|z|)  
# (Intercept)       0.4464     0.1795   2.486   0.0129 *
# biomass_tree_sc  -0.2959     0.1364  -2.169   0.0301 *

#checking model fit
DHARMa::simulateResiduals(lep3, plot = TRUE)#good
DHARMa::testDispersion(lep3,alternative="greater",plot=TRUE)#a bit overdispersed
DHARMa::testDispersion(lep3,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(lep3, plot = TRUE)#n.s.
DHARMa::testOutliers(lep3,plot = TRUE)#n.s.

#adding treatment to model to plot it
lep_treat <- update(lep3,. ~ . +Treatment)
eff_lep<-data.frame(ggeffects::ggpredict(lep_treat,"Treatment"))
eff_lep$response <- "Lepidoptera"
eff_lep$diet <- "Phytophagous"

##plotting final model
#biomass
effec_lep <- data.frame(ggeffects::ggpredict(lep3,"biomass_tree_sc"))

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_lep$biomass_tree <- effec_lep$x * attr(model_arth$biomass_tree_sc, 'scaled:scale') + attr(model_arth$biomass_tree_sc, 'scaled:center')

effec_lep$response <- "Lepidoptera *"

plot_lep <- ggplot()+
  geom_line(effec_lep,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_lep,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=biomass_tree,y=Lepidoptera),alpha=.3)+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree biomass", title="Lepidoptera *")+
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
plot_lep

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_lep_biomass.png", plot = plot_lep, width =7 , height = 5) #To save the plot


#' ## i. Hymenoptera
###checking for outliers - I already remove them when organising the data
plot(model_arth$Hymenoptera)

#model fit for Hymenoptera
formula_hym <- as.formula("Hymenoptera ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_hym,model_arth)#poisson is bad: overdispersed, zero inflated and with outliers; best model: NB (okay - AIC = 1528.9) and then GP(good - AIC = 1530.1); results are similar between distributions

#modeling with NB distribution
hym<-glmmTMB(formula_hym,family=nbinom2,data=model_arth)
summary(hym)

##model selection
drop1(hym)#Treatment:shade_treelevel_sc                              
hym1 <- update(hym,. ~ . -Treatment:shade_treelevel_sc)
anova(hym,hym1)#keep 1 - p=0.4738
drop1(hym1)#shade_treelevel_sc                     
hym2 <- update(hym1,. ~ . -shade_treelevel_sc)
anova(hym1,hym2)#keep 2 - p=0.1499
drop1(hym2)#Treatment                                   
hym3 <- update(hym2,. ~ . -Treatment)
anova(hym2,hym3)#KEEP 2 - p=0.06778 

##final model
summary(hym2)#biomass_tree_sc and Treatment 
# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.13401    0.12547   9.038  < 2e-16 ***
# TreatmentExclosure -0.24248    0.13269  -1.827  0.06763 .  
# biomass_tree_sc    -0.23181    0.08005  -2.896  0.00378 **

#checking model fit
DHARMa::simulateResiduals(hym2, plot = TRUE)#good
DHARMa::testDispersion(hym2,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(hym2,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(hym2, plot = TRUE)#n.s.
DHARMa::testOutliers(hym2,plot = TRUE)#n.s.

#plotting Treatment effect for all response variables
eff_hym<-data.frame(ggeffects::ggpredict(hym2,"Treatment"))
eff_hym$response <- "Hymenoptera (*)"
eff_hym$diet <- "Other"

##plotting final model
#biomass
effec_hym <- data.frame(ggeffects::ggpredict(hym2,"biomass_tree_sc"))

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_hym$biomass_tree <- effec_hym$x * attr(model_arth$biomass_tree_sc, 'scaled:scale') + attr(model_arth$biomass_tree_sc, 'scaled:center')

effec_hym$response <- "Hymenoptera **"

plot_hym <- ggplot()+
  geom_line(effec_hym,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_hym,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=biomass_tree,y=Hymenoptera),alpha=.3)+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree biomass", title="Hymenoptera **")+
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
plot_hym

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_hym_biomass.png", plot = plot_hym, width =7 , height = 5) #To save the plot

#Treatment
effec_hym1 <- data.frame(ggeffects::ggpredict(hym2,"Treatment"))

plot_hym1 <- ggplot()+
  geom_point(model_arth,mapping=aes(x=Treatment,y=Hymenoptera),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_hym1,mapping=aes(x=x ,y=predicted),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_hym1,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Treatment", title="Hymenoptera (*)")+
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
plot_hym1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_hym_treatment.png", plot = plot_hym1, width =7 , height = 5) #To save the plot


#' ## i. Diptera
###checking for outliers - I already remove them when organising the data
plot(model_arth$Diptera)

#model fit for Diptera
formula_dip <- as.formula("Diptera ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_dip,model_arth)#poisson is bad: overdispersed, zero inflated and with outliers; best model: NB (bad, overdispersed and a bit zero-inlfated - AIC = 2817.3) and then GP (good but a bit overdispersed - AIC = 2819.3); results vary a bit between distributions - so using the GP because model seems to fit better

#modeling with GP distribution
dip<-glmmTMB(formula_dip,family=genpois,data=model_arth)
summary(dip)

##model selection
drop1(dip)#Treatment:shade_treelevel_sc                              
dip1 <- update(dip,. ~ . -Treatment:shade_treelevel_sc)
anova(dip,dip1)#keep 1 - p=0.8418
drop1(dip1)#Treatment                     
dip2 <- update(dip1,. ~ . -Treatment)
anova(dip1,dip2)#keep 2 - p=0.9401
drop1(dip2)#shade_treelevel_sc                         
dip3 <- update(dip2,. ~ . -shade_treelevel_sc)
anova(dip2,dip3)#keep 3 - p=0.2693
drop1(dip3)#biomass_tree_sc                          
dip4 <- update(dip3,. ~ . -biomass_tree_sc)
anova(dip3,dip4)#KEEP 3 - p=0.0004147 

##final model
summary(dip3)#biomass_tree_sc 
# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      3.13430    0.06378   49.15  < 2e-16 ***
# biomass_tree_sc -0.16121    0.04340   -3.71 0.000204 ***

#checking model fit
DHARMa::simulateResiduals(dip3, plot = TRUE)#bad
DHARMa::testDispersion(dip3,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(dip3,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(dip3, plot = TRUE)#n.s.
DHARMa::testOutliers(dip3,plot = TRUE)#n.s.

#adding treatment to model to plot it
dip_treat <- update(dip3,. ~ . +Treatment)
eff_dip<-data.frame(ggeffects::ggpredict(dip_treat,"Treatment"))
eff_dip$response <- "Diptera"
eff_dip$diet <- "Pollinator"

##plotting final model
#biomass
effec_dip <- data.frame(ggeffects::ggpredict(dip3,"biomass_tree_sc"))

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_dip$biomass_tree <- effec_dip$x * attr(model_arth$biomass_tree_sc, 'scaled:scale') + attr(model_arth$biomass_tree_sc, 'scaled:center')

effec_dip$response <- "Diptera ***"

plot_dip <- ggplot()+
  geom_line(effec_dip,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_dip,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=biomass_tree,y=Diptera),alpha=.3)+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree biomass", title="Diptera ***")+
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
plot_dip

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_dip_biomass.png", plot = plot_dip, width =7 , height = 5) #To save the plot

#' ## i. Coleoptera
###checking for outliers - I already remove them when organising the data
plot(model_arth$Coleoptera)

#model fit for Coleoptera
formula_col <- as.formula("Coleoptera ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_col,model_arth)#poisson is bad: overdispersed and zero inflated; best model: GP (bad and overdispersed - AIC = 2151.4) and then NB(bad: almost overdispersed and zero-inflated - AIC = 2193.6); results vary a bit between distributions

#modeling with GP distribution
col<-glmmTMB(formula_col,family=genpois,data=model_arth)
summary(col)

##model selection
drop1(col)#Treatment:shade_treelevel_sc                              
col1 <- update(col,. ~ . -Treatment:shade_treelevel_sc)
anova(col,col1)#keep 1 - p=0.2932
drop1(col1)#Treatment:                     
col2 <- update(col1,. ~ . -Treatment)
anova(col1,col2)#keep 2 - p=0.7977
drop1(col2)#biomass_tree_sc                         
col3 <- update(col2,. ~ . -biomass_tree_sc)
anova(col2,col3)#KEEP 2 - p=0.03505 
  
##final model
summary(col2)#shade_treelevel_sc and biomass_tree_sc 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)         2.17449    0.09901  21.961   <2e-16 ***
#   shade_treelevel_sc  0.14763    0.05769   2.559   0.0105 *  
#   biomass_tree_sc    -0.12868    0.06044  -2.129   0.0333 * 

#checking model fit
DHARMa::simulateResiduals(col2, plot = TRUE)#bad
DHARMa::testDispersion(col2,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(col2,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(col2, plot = TRUE)#n.s.
DHARMa::testOutliers(col2,plot = TRUE)#n.s.

#adding treatment to model to plot it
col_treat <- update(col2,. ~ . +Treatment)
eff_col<-data.frame(ggeffects::ggpredict(col_treat,"Treatment"))
eff_col$response <- "Coleoptera"
eff_col$diet <- "Phytophagous"

##plotting final model
#biomass
effec_col <- data.frame(ggeffects::ggpredict(col2,"biomass_tree_sc"))

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_col$biomass_tree <- effec_col$x * attr(model_arth$biomass_tree_sc, 'scaled:scale') + attr(model_arth$biomass_tree_sc, 'scaled:center')

effec_col$response <- "Coleoptera *"

plot_col <- ggplot()+
  geom_line(effec_col,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_col,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=biomass_tree,y=Coleoptera),alpha=.3)+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree biomass", title="Coleoptera *")+
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
plot_col

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_col_biomass.png", plot = plot_col, width =7 , height = 5) #To save the plot

##shade_treelevel_sc  
effec_col1 <- data.frame(ggeffects::ggpredict(col2,"shade_treelevel_sc"))

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_col1$shade_treelevel <- effec_col1$x * attr(model_arth$shade_treelevel_sc, 'scaled:scale') + attr(model_arth$shade_treelevel_sc, 'scaled:center')

effec_col1$response <- "Coleoptera *"

plot_col1 <- ggplot()+
  geom_line(effec_col1,mapping=aes(x=shade_treelevel ,y=predicted))+
  geom_ribbon(effec_col1,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=shade_treelevel,y=Coleoptera),alpha=.3)+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree shade cover", title="Coleoptera *")+
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
plot_col1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_col_shadetree.png", plot = plot_col1, width =7 , height = 5) #To save the plot

#' ## i. Orthoptera
###checking for outliers - I already remove them when organising the data
plot(model_arth$Orthoptera)

#model fit for Orthoptera
formula_ort <- as.formula("Orthoptera ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_ort,model_arth)#poisson is n.s. for everthing but looks bad (AIC = 793.2); best model: NB (okay - AIC = 771.9) and then GP (okay - AIC = 772.5); results are similar between distributions; makes sense to use poisson then

#modeling with Poisson distribution
ort<-glmmTMB(formula_ort,family=poisson,data=model_arth)
summary(ort)

##model selection
drop1(ort)#Treatment:shade_treelevel_sc                              
ort1 <- update(ort,. ~ . -Treatment:shade_treelevel_sc)
anova(ort,ort1)#KEEP initial  model - p=0.08095 

##final model
summary(ort)#Treatment, shade_treelevel_sc and biomass_tree_sc
# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           -0.52955    0.23619  -2.242   0.0250 *  
# TreatmentExclosure                    -0.01044    0.16970  -0.062   0.9510    
# shade_treelevel_sc                    -0.61274    0.15385  -3.983 6.81e-05 ***
# biomass_tree_sc                       -0.24651    0.10519  -2.343   0.0191 *  
# TreatmentExclosure:shade_treelevel_sc  0.33196    0.18615   1.783   0.0745 .  

#checking model fit
DHARMa::simulateResiduals(ort, plot = TRUE)#bad
DHARMa::testDispersion(ort,alternative="greater",plot=TRUE)#n.s
DHARMa::testDispersion(ort,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(ort, plot = TRUE)#n.s.
DHARMa::testOutliers(ort,plot = TRUE)#n.s.

#plotting Treatment effect to plot all response variables together
eff_ort<-data.frame(ggeffects::ggpredict(ort,"Treatment"))
eff_ort$response <- "Orthoptera"
eff_ort$diet <- "Phytophagous"

##plotting final model
#Treatment:shade_treelevel_sc
effec_ort <- data.frame(ggeffects::ggpredict(ort,c("shade_treelevel_sc","Treatment")))

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_ort$shade_treelevel <- effec_ort$x * attr(model_arth$shade_treelevel_sc, 'scaled:scale') + attr(model_arth$shade_treelevel_sc, 'scaled:center')

effec_ort$response <- "Orthoptera (*)"

plot_ort <- ggplot()+
  geom_line(effec_ort,mapping=aes(x=shade_treelevel ,y=predicted,colour=group))+
  geom_ribbon(effec_ort,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error,fill=group),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=shade_treelevel,y=Orthoptera,colour=Treatment),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree shade cover", title="Orthoptera (*)")+
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
plot_ort

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_ort_shadeXtreatment.png", plot = plot_ort, width =7 , height = 5) #To save the plot

##Treatment
#ggefects works better when I have interactions but want only a single term
effec_ort1 <- data.frame(ggeffects::ggpredict(ort, terms = "Treatment"))

plot_ort1 <- ggplot()+
  geom_point(model_arth,mapping=aes(x=Treatment,y=Orthoptera),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_ort1,mapping=aes(x=x ,y=predicted ),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_ort1,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Treatment", title="Orthoptera")+
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
plot_ort1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_ort_treatment.png", plot = plot_ort1, width =7 , height = 5) #To save the plot

##shade_treelevel_sc   
effec_ort2 <- data.frame(ggeffects::ggpredict(ort,"shade_treelevel_sc"))

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_ort2$shade_treelevel <- effec_ort2$x * attr(model_arth$shade_treelevel_sc, 'scaled:scale') + attr(model_arth$shade_treelevel_sc, 'scaled:center')

effec_ort2$response <- "Orthoptera ***"

plot_ort2 <- ggplot()+
  geom_line(effec_ort2,mapping=aes(x=shade_treelevel ,y=predicted))+
  geom_ribbon(effec_ort2,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=shade_treelevel,y=Orthoptera),alpha=.3)+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree shade cover", title="Orthoptera ***")+
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
plot_ort2

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_ort_shadetree.png", plot = plot_ort2, width =7 , height = 5) #To save the plot

#biomass_tree_sc
effec_ort3 <- data.frame(ggeffects::ggpredict(ort,c("biomass_tree_sc"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_ort3$biomass_tree <- effec_ort3$x * attr(model_arth$biomass_tree_sc, 'scaled:scale') + attr(model_arth$biomass_tree_sc, 'scaled:center')

effec_ort3$response <- "Orthoptera *"

plot_ort3 <- ggplot()+
  geom_line(effec_ort3,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_ort3,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=biomass_tree,y=Orthoptera),alpha=.3)+
  labs(y="Counts",x="Cacao tree biomass", title="Orthoptera *")+
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
plot_ort3

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_ort_biomass.png", plot = plot_ort3, width =7 , height = 5) #To save the plot

#' ## i. Arachnida
###checking for outliers - I already remove them when organising the data
plot(model_arth$Arachnida)

#model fit for Arachnida
formula_ara <- as.formula("Arachnida ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_ara,model_arth)#poisson is bad: overdispersed, zero inflated and with ouliers; best model: GP (very bad, overdispersed and a bit zero-inflated - AIC = 2254.367) and then NB(very bad, overdispersed and zero-inflated - AIC = 2265.94); results are the same between distributions

#modeling with GP distribution
ara<-glmmTMB(formula_ara,family=genpois,data=model_arth)
summary(ara)

##model selection
drop1(ara)#Treatment:shade_treelevel_sc                              
ara1 <- update(ara,. ~ . -Treatment:shade_treelevel_sc)
anova(ara,ara1)#keep 1 - p=0.6596
drop1(ara1)#shade_treelevel_sc                     
ara2 <- update(ara1,. ~ . -shade_treelevel_sc)
anova(ara1,ara2)#keep 2 - p=0.2606
drop1(ara2)#biomass_tree_sc                          
ara3 <- update(ara2,. ~ . -biomass_tree_sc)
anova(ara2,ara3)#KEEP 2 - p=0.06854 

##final model
summary(ara2)#Treatment and biomass_tree_sc
# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.18263    0.11141  19.591  < 2e-16 ***
# TreatmentExclosure  0.34143    0.09918   3.443 0.000576 ***
# biomass_tree_sc    -0.11611    0.06796  -1.709 0.087536 . 

#checking model fit
DHARMa::simulateResiduals(ara2, plot = TRUE)#very bad
DHARMa::testDispersion(ara2,alternative="greater",plot=TRUE)#significant
DHARMa::testDispersion(ara2,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(ara2, plot = TRUE)#almost significant 
DHARMa::testOutliers(ara2,plot = TRUE)#significant

#plotting Treatment effect from main model
eff_ara<-data.frame(ggeffects::ggpredict(ara2,"Treatment"))
eff_ara$response <- "Araneae ***"
eff_ara$diet <- "Predator"

##plotting final model
#biomass_tree_sc
effec_ara <- data.frame(ggeffects::ggpredict(ara2,"biomass_tree_sc")) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_ara$biomass_tree <- effec_ara$x * attr(model_arth$biomass_tree_sc, 'scaled:scale') + attr(model_arth$biomass_tree_sc, 'scaled:center')

effec_ara$response <- "Araneae *"

plot_ara <- ggplot()+
  geom_line(effec_ara,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_ara,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=biomass_tree,y=Arachnida),alpha=.3)+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree biomass", title="Araneae *")+
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
plot_ara

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_ara_biomass.png", plot = plot_ara, width =7 , height = 5) #To save the plot

##Treatment
effec_ara1 <- data.frame(ggeffects::ggpredict(ara2, terms = "Treatment"))

plot_ara1 <- ggplot()+
  geom_point(model_arth,mapping=aes(x=Treatment,y=Arachnida),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_ara1,mapping=aes(x=x ,y=predicted  ),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_ara1,mapping=aes(x=x,ymin=predicted  -std.error,ymax=predicted  +std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Treatment", title="Araneae ***")+
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
plot_ara1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_ara_treatment.png", plot = plot_ara1, width =7 , height = 5) #To save the plot


#' ## i. Formicidae
###checking for outliers - I already remove them when organising the data
plot(model_arth$Formicidae)

#model fit for Formicidae
formula_for <- as.formula("Formicidae ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_for,model_arth)#poisson is zero inflated and with outliers; best model: Quasi (okay - AIC = 4042.3) and then GP(okay - AIC = 4052.3); results are the same between distributions; using GP because overall it looked the same and doesn't make sense to use other distributions (already using Poisson, NB and GP)

#modeling with GP distribution
form<-glmmTMB(formula_for,family=genpois,data=model_arth)
summary(form)

##model selection
drop1(form)#Treatment:shade_treelevel_sc                              
form1 <- update(form,. ~ . -Treatment:shade_treelevel_sc)
anova(form,form1)#keep 1 - p=
drop1(form1)#shade_treelevel_sc                     
form2 <- update(form1,. ~ . -shade_treelevel_sc)
anova(form1,form2)#keep 2 - p=0.5053
drop1(form2)#Treatment                         
form3 <- update(form2,. ~ . -Treatment)
anova(form2,form3)#keep 3 - p=0.3372
drop1(form3)#biomass_tree_sc                       
form4 <- update(form3,. ~ . -biomass_tree_sc)
anova(form3,form4)#KEEP 3 - p=0.008522  

##final model
summary(form3)#biomass_tree_sc   
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)       4.8131     0.2064   23.32  < 2e-16 ***
#   biomass_tree_sc   0.3846     0.1384    2.78  0.00544 ** 

#checking model fit
DHARMa::simulateResiduals(form3, plot = TRUE)#okay
DHARMa::testDispersion(form3,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(form3,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(form3, plot = TRUE)#n.s. 
DHARMa::testOutliers(form3,plot = TRUE)#n.s.

#adding treatment to model to plot it
form_treat <- update(form3,. ~ . +Treatment)
eff_form<-data.frame(ggeffects::ggpredict(form_treat,"Treatment"))
eff_form$response <- "Formicidae"
eff_form$diet <- "Other"

##plotting final model
#biomass_tree_sc
effec_form <- data.frame(ggeffects::ggpredict(form3,"biomass_tree_sc")) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_form$biomass_tree <- effec_form$x * attr(model_arth$biomass_tree_sc, 'scaled:scale') + attr(model_arth$biomass_tree_sc, 'scaled:center')

effec_form$response <- "Formicidae **"

plot_form <- ggplot()+
  geom_line(effec_form,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_form,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=biomass_tree,y=Formicidae),alpha=.3)+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree biomass", title="Formicidae **")+
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
plot_form

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_form_biomass.png", plot = plot_form, width =7 , height = 5) #To save the plot

#' ## i. Mealybugs
###checking for outliers - I already remove them when organising the data
plot(model_arth$Mealybugs)

#model fit for Mealybugs
formula_mea <- as.formula("Mealybugs ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_mea,model_arth)#poisson is bad: overdispersed, zero inflated and with outliers; bet model: NB (good - AIC = 1701.7) and then Zero NB (good - AIC = 1702.9); results vary a bit between distributions

#modeling with NB distribution
mea<-glmmTMB(formula_mea,family=nbinom2,data=model_arth)
summary(mea)

##model selection
drop1(mea)#Treatment:shade_treelevel_sc                              
mea1 <- update(mea,. ~ . -Treatment:shade_treelevel_sc)
anova(mea,mea1)#keep 1 - p=0.6879
drop1(mea1)#shade_treelevel_sc                     
mea2 <- update(mea1,. ~ . -shade_treelevel_sc)
anova(mea1,mea2)#keep 2 - p=0.5552
drop1(mea2)#biomass_tree_sc                           
mea3 <- update(mea2,. ~ . -biomass_tree_sc)
anova(mea2,mea3)#keep 3 - p=0.1388
drop1(mea3)#Treatment                       
mea4 <- update(mea3,. ~ . -Treatment)
anova(mea3,mea4)#KEEP3 4 - p=2.266e-09 

##final model
summary(mea3)#Treatment
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)          1.0462     0.2482   4.215  2.5e-05 ***
#   TreatmentExclosure   2.2359     0.3256   6.866  6.6e-12 ***

#checking model fit
DHARMa::simulateResiduals(mea3, plot = TRUE)#good
DHARMa::testDispersion(mea3,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(mea3,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(mea3, plot = TRUE)#n.s. 
DHARMa::testOutliers(mea3,plot = TRUE)#n.s.

#plotting Treatment effect from main model
eff_mea<-data.frame(ggeffects::ggpredict(mea3,"Treatment"))
eff_mea$response <- "Mealybugs ***"
eff_mea$diet <- "Phytophagous"

##plotting final model
##Treatment
effec_mea <- data.frame(ggeffects::ggpredict(mea3, terms = "Treatment"))

plot_mea <- ggplot()+
  geom_point(model_arth,mapping=aes(x=Treatment,y=Mealybugs),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_mea,mapping=aes(x=x ,y=predicted),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_mea,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted +std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Treatment", title="Mealybugs ***")+
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
plot_mea

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_mea_treatment.png", plot = plot_mea, width =7 , height = 5) #To save the plot


#' ## i. Mantodae
###checking for outliers - I already remove them when organizing the data
plot(model_arth$Mantodae)

#model fit for Mantodae
formula_man <- as.formula("Mantodae ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_man,model_arth)#poisson is good (AIC = 629.3) but almost overdispersed; best model: GP (good - AIC = 614.8) and then Quasi(good - AIC = 615.3); results are the same between distributions

#modeling with GP distribution
man<-glmmTMB(formula_man,family=genpois,data=model_arth)
summary(man)

##model selection
drop1(man)#biomass_tree_sc                                             
man1 <- update(man,. ~ . -biomass_tree_sc)
anova(man,man1)#keep 1 - p=0.6375
drop1(man1)#Treatment:shade_treelevel_sc                     
man2 <- update(man1,. ~ . -Treatment:shade_treelevel_sc)
anova(man1,man2)#keep 2 - p=0.4365
drop1(man2)#shade_treelevel_sc                                        
man3 <- update(man2,. ~ . -shade_treelevel_sc)
anova(man2,man3)#keep 3 - p=0.1804
drop1(man3)#Treatment                        
man4 <- update(man3,. ~ . -Treatment)
anova(man3,man4)#KEEP 3 - p=3.497e-06 

##final model
summary(man3)#Treatment
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)         -1.4987     0.2153  -6.961 3.38e-12 ***
#   TreatmentExclosure   1.1281     0.2055   5.488 4.06e-08 ***

#checking model fit
DHARMa::simulateResiduals(man3, plot = TRUE)#good
DHARMa::testDispersion(man3,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(man3,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(man3, plot = TRUE)#n.s. 
DHARMa::testOutliers(man3,plot = TRUE)#n.s.

#plotting Treatment effect fro plottinh together with other response variables
eff_man<-data.frame(ggeffects::ggpredict(man,"Treatment"))
eff_man$response <- "Mantodea ***"
eff_man$diet <- "Predator"

##plotting final model
##Treatment
effec_man <- data.frame(ggeffects::ggpredict(man3, terms = "Treatment"))

plot_man <- ggplot()+
  geom_point(model_arth,mapping=aes(x=Treatment,y=Mantodae),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_man,mapping=aes(x=x ,y=predicted),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_man,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted +std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Treatment", title="Mantodea ***")+
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
plot_man

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_man_treatment.png", plot = plot_man, width =7 , height = 5) #To save the plot

#' ## i. Neuroptera
###checking for outliers - I already remove them when organizing the data
plot(model_arth$Neuroptera)

#model fit for Neuroptera
formula_neu <- as.formula("Neuroptera ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_neu,model_arth)#poisson is almost overdispersed and a almost zero inflated; best model: NB (good - AIC = 499.1) and then Zero NB(good - AIC = 501.0);results are the same between distributions 

#modeling with NB distribution
neu<-glmmTMB(formula_neu,family=nbinom2,data=model_arth)
summary(neu)

##model selection
drop1(neu)#biomass_tree_sc                                             
neu1 <- update(neu,. ~ . -biomass_tree_sc)
anova(neu,neu1)#keep 1 - p=0.8359
drop1(neu1)#Treatment:shade_treelevel_sc                  
neu2 <- update(neu1,. ~ . -Treatment:shade_treelevel_sc)
anova(neu1,neu2)#keep 2 - p=0.7437
drop1(neu2)#Treatment                                     
neu3 <- update(neu2,. ~ . -Treatment)
anova(neu2,neu3)#keep 3 - p=0.2394
drop1(neu3)#shade_treelevel_sc                        
neu4 <- update(neu3,. ~ . -shade_treelevel_sc)
anova(neu3,neu4)#KEEP 3 - p=0.04707  


##final model
summary(neu3)#shade_treelevel_sc
# Conditional model:
# Estimate Std. Error z value Pr(>|z|)   
# (Intercept)         -1.3475     0.4678  -2.880  0.00397 **
# shade_treelevel_sc   0.4918     0.2427   2.026  0.04273 * 

#checking model fit
DHARMa::simulateResiduals(neu3, plot = TRUE)#good
DHARMa::testDispersion(neu3,alternative="greater",plot=TRUE)#n.s
DHARMa::testDispersion(neu3,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(neu3, plot = TRUE)#n.s.
DHARMa::testOutliers(neu3,plot = TRUE)#n.s.

#adding treatment to model to plot it
neu_treat <- update(neu3,. ~ . +Treatment)
eff_neu<-data.frame(ggeffects::ggpredict(neu_treat,"Treatment"))
eff_neu$response <- "Neuroptera"
eff_neu$diet <- "Predator"

##plotting final model
##shade_treelevel_sc   
effec_neu <- data.frame(ggeffects::ggpredict(neu3,"shade_treelevel_sc"))

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_neu$shade_treelevel <- effec_neu$x * attr(model_arth$shade_treelevel_sc, 'scaled:scale') + attr(model_arth$shade_treelevel_sc, 'scaled:center')

effec_neu$response <- "Neuroptera *"

plot_neu <- ggplot()+
  geom_line(effec_neu,mapping=aes(x=shade_treelevel ,y=predicted))+
  geom_ribbon(effec_neu,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(model_arth,mapping=aes(x=shade_treelevel,y=Neuroptera),alpha=.3)+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Counts",x="Cacao tree shade cover", title="Neuroptera *")+
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
plot_neu

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_neu_shadetree.png", plot = plot_neu, width =7 , height = 5) #To save the plot

#' ## i. Brown.capsid
###checking for outliers - I already remove them when organising the data
plot(model_arth$Brown.capsid)

#model fit for Brown.capsid
formula_cap <- as.formula("Brown.capsid ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_cap,model_arth)#poisson okay (AIC = 483.5) and n.s. for all; best model: GP (okay - AIC = 215.0) and then NB (good - AIC = 215.5); results are similar between distributions; using GP because Poisson has a a way bigger AIC

#modeling with GP distribution
cap<-glmmTMB(formula_cap,family=genpois,data=model_arth)
summary(cap)

##model selection
drop1(cap)#Treatment:shade_treelevel_sc                              
cap1 <- update(cap,. ~ . -Treatment:shade_treelevel_sc)
anova(cap,cap1)#keep 1 - p=0.6245
drop1(cap1)#shade_treelevel_sc                     
cap2 <- update(cap1,. ~ . -shade_treelevel_sc)
anova(cap1,cap2)#keep 2 - p=0.4232
drop1(cap2)#Treatment
cap3 <- update(cap2,. ~ . -Treatment)
anova(cap2,cap3)#keep 3 - p=0.443
drop1(cap3)#biomass_tree_sc                        
cap4 <- update(cap3,. ~ . -biomass_tree_sc)
anova(cap3,cap4)#keep 4 - p=0.1608 
#Keep null model                           
 
##final model
summary(cap4)#No variables
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)  
#   (Intercept)  -2.2579     0.9222  -2.449   0.0143 *

#checking model fit
DHARMa::simulateResiduals(cap4, plot = TRUE)#good
DHARMa::testDispersion(cap4,alternative="greater",plot=TRUE)#n.s
DHARMa::testDispersion(cap4,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(cap4, plot = TRUE)#n.s.
DHARMa::testOutliers(cap4,plot = TRUE)#n.s.

#adding treatment to model to plot it
cap_treat <- update(cap4,. ~ . +Treatment)
eff_cap<-data.frame(ggeffects::ggpredict(cap_treat,"Treatment"))
eff_cap$response <- "Brown Capsid"
eff_cap$diet <- "Phytophagous"

#' ## i. Blatodae
###checking for outliers - I already remove them when organising the data
plot(model_arth$Blatodae)

#model fit for Blatodae
formula_bla <- as.formula("Blatodae ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_bla,model_arth)#poisson is a bit overdispersed and zero-inflated; best model: NB (good - AIC = 319.2) and then Zero NB (good - AIC = 321.0); results are similar between distributions

#modeling with NB distribution
bla<-glmmTMB(formula_bla,family=nbinom2,data=model_arth)
summary(bla)

##model selection
drop1(bla)#biomass_tree_sc                                                
bla1 <- update(bla,. ~ . -biomass_tree_sc)
anova(bla,bla1)#keep 1 - p=0.7662
drop1(bla1)#Treatment:shade_treelevel_sc                  
bla2 <- update(bla1,. ~ . -Treatment:shade_treelevel_sc)
anova(bla1,bla2)#keep 2 - p=0.2819
drop1(bla2)#shade_treelevel_sc                            
bla3 <- update(bla2,. ~ . -shade_treelevel_sc)
anova(bla2,bla3)#keep 3 - p=0.6842
drop1(bla3)#Treatment                             
bla4 <- update(bla3,. ~ . -Treatment)
anova(bla3,bla4)#keep 4 - p=0.266 
#Keep nul model                           

##final model
summary(bla4)#No variables 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)  
#   (Intercept)  -1.2808     0.6529  -1.962   0.0498 *

#checking model fit
DHARMa::simulateResiduals(bla4, plot = TRUE)#good
DHARMa::testDispersion(bla4,alternative="greater",plot=TRUE)#n.s
DHARMa::testDispersion(bla4,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(bla4, plot = TRUE)#n.s.
DHARMa::testOutliers(bla4,plot = TRUE)#n.s.

#adding treatment to model to plot it
bla_treat <- update(bla4,. ~ . +Treatment)
eff_bla<-data.frame(ggeffects::ggpredict(bla_treat,"Treatment"))
eff_bla$response <- "Blattodae"
eff_bla$diet <- "Phytophagous"

#'##Combining all data for plotting
eff_all <- rbind(eff_bla,eff_col,eff_hem,eff_lep,eff_ort,eff_cap,eff_mea,eff_dip,eff_hym,eff_ara,eff_man,eff_neu,eff_form)

#reorder levels
library(forcats)
eff_all$response <- as.factor(eff_all$response)
eff_all$diet <- as.factor(eff_all$diet)

#reorder and rename levels
eff_all$response <- factor(eff_all$response, levels = c("Blattodae","Coleoptera","Hemiptera ***","Lepidoptera","Orthoptera","Brown Capsid","Mealybugs ***","Diptera","Hymenoptera (*)","Formicidae","Araneae ***","Mantodea ***","Neuroptera"))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.5)

plot_arth <- ggplot(eff_all[!eff_all$response %in% "Orthoptera",],aes(x=x, y=predicted, colour=x))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),width=.2,position=pd)+
  geom_line(position=pd)+
  geom_point(aes(shape=x),position=pd,size=6)+
  facet_wrap(~response,scales="free_y",nrow=5,ncol=3)+
  #scale_shape(solid=F)+ # no fill for points
  xlab("")+ ylab("Abundance (tree/month)")+
  theme_bw() + 
  scale_colour_colorblind()+
  expand_limits(y=0)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x=element_text(size=25,vjust=-1))+
  theme(axis.title.y=element_text(size=25,angle=90,vjust=2))+
  theme(axis.text.x=element_text(size=25,face="bold",vjust=0.5))+
  theme(axis.text.y=element_text(size=25))+
  theme(legend.text=element_text(size=25),legend.title = element_blank(),legend.position="bottom",legend.key = element_blank())+ #Remove only the legend title
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5),"cm"))+
  theme(strip.text.x = element_text(size=30))+
  theme(strip.background = element_rect(fill = 'grey'))+
  geom_vline(xintercept=c(1.5),color="red", linetype="dotted",size=1)#create lines between some variables
plot_arth

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_treatment_arthropods.png", plot = plot_arth, width =15 , height = 15) #To save the plot

#'##Combining groups that had biomass in the final model
eff_biomass <- rbind(effec_lep,effec_hym,effec_dip,effec_col,effec_ort3,effec_ara,effec_form)

eff_biomass$response <- as.factor(eff_biomass$response)
table(eff_biomass$response)

write.csv2(eff_biomass,"outputs/data/arthropods_biomass_plot.csv",row.names = FALSE)

#'##Combining groups that had shade in the final model
eff_shade <- rbind(effec_col1,effec_ort2,effec_neu)

eff_shade$response <- as.factor(eff_shade$response)
table(eff_shade$response)

write.csv2(eff_shade,"outputs/data/arthropods_shade_plot.csv",row.names = FALSE)

#'##Combining groups that had shadeXtreatment in the final model
eff_shadeXtreat <- rbind(effec_hem1,effec_ort)

eff_shadeXtreat$response <- as.factor(eff_shadeXtreat$response)
table(eff_shadeXtreat$response)

write.csv2(eff_shadeXtreat,"outputs/data/arthropods_shadeXtreat_plot.csv",row.names = FALSE)


