library(glmmTMB)
library(ggplot2)
library(ggthemes)
library(dplyr)

###importing pods and flowers data
pods <- read.csv2("outputs/data/pods_flowers_data_final.csv")
head(pods)
dim(pods)

#' ### STATISTICAL ANALYSES
#' #Pods
##creating data frame with selected predictors
colnames(pods)
poddata<-pods[c("Flowers","PodSmallAll","PodMedium","PodBig","PodBigMed","Farm","Tree","Treatment","Visit","shade_cover","shade_treelevel","biomass_tree","DiameterBranches")]#We end up using shade_treelevel because we have more data points (one per tree, while for farm shade cover we have only 8). Also, insects abundance (especially capsids) is dependent on pockets of sun in the farm. However, there are differences in the results. When using farm shade cover non of the interactions between treatment and shade is significant - but we have a negative relations of shade with pods. 
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

#' ## i. Flowers
###checking for outliers - I already remove them when organising the data
plot(poddata$Flowers)

###Function to check model fitness for selected insect groups
source("scripts/model_fit_21.06.25.R")

#model fit for Flowers
formula_flo <- as.formula("Flowers ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_flo,poddata)#poisson bad: almost overdispersed, zero-inflated and with outliers; best model: Quasi (bad and a bir overdispersed - AIC = 2420.4), then Zero-NB (okay - AIC = 2437.5), and  NB (okay - AIC = 2453.3); results vary a bit; using NB because there is no need to add other distributions and results are very similar

#modeling with NB distribution
flo<-glmmTMB(formula_flo,family=nbinom2,data=poddata)
summary(flo)

##model selection
drop1(flo)#biomass_tree_sc                           
flo1 <- update(flo,. ~ . -biomass_tree_sc)
anova(flo,flo1)#keep 1 - p=0.8302
drop1(flo1)#Treatment:shade_treelevel_sc                                
flo2 <- update(flo1,. ~ . -Treatment:shade_treelevel_sc)
anova(flo1,flo2)#KEEP 1 - p=0.07497 

##final model
summary(flo1)#Treatment and shade_treelevel_sc 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                             3.1126     0.2759  11.281   <2e-16 ***
#   TreatmentExclosure                     -0.0758     0.3367  -0.225   0.8219    
#   shade_treelevel_sc                      0.2111     0.2652   0.796   0.4260    
#   TreatmentExclosure:shade_treelevel_sc  -0.7793     0.4318  -1.805   0.0711 .  

#checking model fit
DHARMa::simulateResiduals(flo1, plot = TRUE)#okay
DHARMa::testDispersion(flo1,alternative="greater",plot=TRUE)#n.s
DHARMa::testDispersion(flo1,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(flo1, plot = TRUE)#almost zero infalted
DHARMa::testOutliers(flo1,plot = TRUE)#n.s.

#plotting treatment with other responses variables in the end
eff_flo<-data.frame(ggeffects::ggpredict(flo1,"Treatment"))
eff_flo$response <- "Flowers"

##plotting final model
##Treatment
#ggefects works better when I have interactions but want only a single term
effec_flo <- data.frame(ggeffects::ggpredict(flo1, terms = "Treatment"))

plot_flo <- ggplot()+
  geom_point(poddata,mapping=aes(x=Treatment,y=Flowers),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_flo,mapping=aes(x=x ,y=predicted ),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_flo,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Predicted nº of flowers tree/month",x="Treatment", title="Flowers")+
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
plot_flo

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_flo_treatment.png", plot = plot_flo, width =7 , height = 5) #To save the plot

#Treatment:shade_treelevel_sc
effec_flo1 <- data.frame(ggeffects::ggpredict(flo1,c("shade_treelevel_sc","Treatment"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_flo1$shade_treelevel <- effec_flo1$x * attr(poddata$shade_treelevel_sc, 'scaled:scale') + attr(poddata$shade_treelevel_sc, 'scaled:center')

effec_flo1$response <- "Flowers (*)"

plot_flo1 <- ggplot()+
  geom_line(effec_flo1,mapping=aes(x=shade_treelevel ,y=predicted,colour=group))+
  geom_ribbon(effec_flo1,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error,fill=group),alpha=.2)+
  geom_point(poddata,mapping=aes(x=shade_treelevel,y=Flowers,colour=Treatment),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Predicted nº of flowers tree/month",x="Cacao tree shade cover", title="Flowers (*)")+
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
plot_flo1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_flo_shadeXtreatment.png", plot = plot_flo1, width =7 , height = 5) #To save the plot

#' ## i. PodSmallAll
###checking for outliers - I already remove them when organising the data
plot(poddata$PodSmallAll)

#model fit for PodSmallAll
formula_psmall <- as.formula("PodSmallAll ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_psmall,poddata)#poisson bad: zero inflated; best model: GP (good - AIC = 1969.5) and then NB(okay - AIC = 1972.9); results are teh same between distributions 

#modeling with GP distribution
psmall<-glmmTMB(formula_psmall,family=genpois,data=poddata)
summary(psmall)

##model selection
drop1(psmall)#Treatment:shade_treelevel_sc                           
psmall1 <- update(psmall,. ~ . -Treatment:shade_treelevel_sc)
anova(psmall,psmall1)#keep 1 - p=0.3379
drop1(psmall1)#shade_treelevel_sc                          
psmall2 <- update(psmall1,. ~ . -shade_treelevel_sc)
anova(psmall1,psmall2)#keep 2 - p=0.9653
drop1(psmall2)#Treatment                                
psmall3 <- update(psmall2,. ~ . -Treatment)
anova(psmall2,psmall3)#keep 3 - p=0.7285
drop1(psmall3)#biomass_tree_sc                             
psmall4 <- update(psmall3,. ~ . -biomass_tree_sc)
anova(psmall3,psmall4)#keep 4 - p=0.3179  
drop1(psmall4)#shade_treelevel_sc                             
#Null model won

##final model
summary(psmall4)#Null model - no variables
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.2351     0.2227   10.04   <2e-16 ***

#checking model fit
DHARMa::simulateResiduals(psmall4, plot = TRUE)#good
DHARMa::testDispersion(psmall4,alternative="greater",plot=TRUE)#n.s
DHARMa::testDispersion(psmall4,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(psmall4, plot = TRUE)#n.s.
DHARMa::testOutliers(psmall4,plot = TRUE)#n.s.

#adding treatment to model to plot it
psmall_treat <- update(psmall4,. ~ . +Treatment)
eff_psmall<-data.frame(ggeffects::ggpredict(psmall_treat,"Treatment"))
eff_psmall$response <- "Small pods"

#' ## i. PodMedium
###checking for outliers - I already remove them when organising the data
plot(poddata$PodMedium)

#model fit for PodMedium
formula_pmed <- as.formula("PodMedium ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_pmed,poddata)#poisson is bad: zero inflated; best model: Zero NB (good - AIC = 1056.6) and then NB(okay - AIC = 1058.9) - using NB because results are basically the same

#modeling with NB distribution
pmed<-glmmTMB(formula_pmed,family=nbinom2,data=poddata)
summary(pmed)

##model selection
drop1(pmed)#biomass_tree_sc                           
pmed1 <- update(pmed,. ~ . -biomass_tree_sc)
anova(pmed,pmed1)#KEEP Initial model - p=0.07732 

##final model
summary(pmed)#Treatment, shade_treelevel_sc and biomass_tree_sc
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                             0.2627     0.2720   0.966   0.3342  
#   TreatmentExclosure                     -0.3128     0.3443  -0.909   0.3635  
#   shade_treelevel_sc                      0.2441     0.2880   0.848   0.3967  
#   biomass_tree_sc                         0.3471     0.1978   1.754   0.0793 .
#   TreatmentExclosure:shade_treelevel_sc  -0.8473     0.3704  -2.288   0.0222 *

#checking model fit
DHARMa::simulateResiduals(pmed, plot = TRUE)#good
DHARMa::testDispersion(pmed,alternative="greater",plot=TRUE)#n.s
DHARMa::testDispersion(pmed,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(pmed, plot = TRUE)#n.s.
DHARMa::testOutliers(pmed,plot = TRUE)#n.s.

#plotting treatment with other responses variables in the end
eff_pmed<-data.frame(ggeffects::ggpredict(pmed,"Treatment"))
eff_pmed$response <- "Medium pods"

##plotting final model
##Treatment
#ggefects works better when I have interactions but want only a single term
effec_pmed <- data.frame(ggeffects::ggpredict(pmed, terms = "Treatment"))

plot_pmed <- ggplot()+
  geom_point(poddata,mapping=aes(x=Treatment,y=PodMedium),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_pmed,mapping=aes(x=x ,y=predicted ),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_pmed,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Predicted nº of pods per tree/month",x="Treatment", title="Medium size pods")+
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
plot_pmed

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_pmed_treatment.png", plot = plot_pmed, width =7 , height = 5) #To save the plot

#Treatment:shade_treelevel_sc
effec_pmed1 <- data.frame(ggeffects::ggpredict(pmed,c("shade_treelevel_sc","Treatment"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_pmed1$shade_treelevel <- effec_pmed1$x * attr(poddata$shade_treelevel_sc, 'scaled:scale') + attr(poddata$shade_treelevel_sc, 'scaled:center')

effec_pmed1$response <- "Medium pods *"

plot_pmed1 <- ggplot()+
  geom_line(effec_pmed1,mapping=aes(x=shade_treelevel ,y=predicted,colour=group))+
  geom_ribbon(effec_pmed1,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error,fill=group),alpha=.2)+
  geom_point(poddata,mapping=aes(x=shade_treelevel,y=PodMedium,colour=Treatment),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Predicted nº of pods per tree/month",x="Cacao tree shade cover", title="Medium size pods *")+
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
plot_pmed1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_pmed_shadeXtreatment.png", plot = plot_pmed1, width =7 , height = 5) #To save the plot

#biomass_tree_sc
effec_pmed2 <- data.frame(ggeffects::ggpredict(pmed,c("biomass_tree_sc"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_pmed2$biomass_tree <- effec_pmed2$x * attr(poddata$biomass_tree_sc, 'scaled:scale') + attr(poddata$biomass_tree_sc, 'scaled:center')

effec_pmed2$response <- "Medium pods (*)"

plot_pmed2 <- ggplot()+
  geom_line(effec_pmed2,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_pmed2,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(poddata,mapping=aes(x=biomass_tree,y=PodMedium),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Predicted nº of pods per tree/month",x="Cacao tree biomass", title="Medium size pods (*)")+
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
plot_pmed2

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_pmed_biomass.png", plot = plot_pmed2, width =7 , height = 5) #To save the plot


#' ## i. PodBig
###checking for outliers - I already remove them when organising the data
plot(poddata$PodBig)

#model fit for PodBig
formula_pbig <- as.formula("PodBig ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_pbig,poddata)#poisson is zero inflated and with outliers; best model: Zero NB (okay - AIC = 1230.0), Quasi (good - AIC = 1239.0) and then NB (okay - AIC = 1293.5) - using NB because results are basically the same to zero NB

#modeling with NB distribution
pbig<-glmmTMB(formula_pbig,family=nbinom2,data=poddata)
summary(pbig)

##model selection
drop1(pbig)#Treatment:shade_treelevel_sc                          
pbig1 <- update(pbig,. ~ . -Treatment:shade_treelevel_sc)
anova(pbig,pbig1)#keep 1 - p=
drop1(pbig1)#Treatment                               
pbig2 <- update(pbig1,. ~ . -Treatment)
anova(pbig1,pbig2)#keep 2 - p=0.9652
drop1(pbig2)#shade_treelevel_sc                          
pbig3 <- update(pbig2,. ~ . -shade_treelevel_sc)
anova(pbig2,pbig3)#keep 3 - p=0.5323
drop1(pbig3)#biomass_tree_sc                          
pbig4 <- update(pbig3,. ~ . -biomass_tree_sc)
anova(pbig3,pbig4)#KEEP 3 - p=0.001849 
                                  
##final model
summary(pbig3)#biomass_tree_sc
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)       0.5272     0.3836   1.375 0.169268    
#   biomass_tree_sc   0.4946     0.1469   3.367 0.000759 ***

#checking model fit
DHARMa::simulateResiduals(pbig3, plot = TRUE)#okay
DHARMa::testDispersion(pbig3,alternative="greater",plot=TRUE)#n.s
DHARMa::testDispersion(pbig3,alternative ="less",plot = TRUE)#n.s.
DHARMa::testZeroInflation(pbig3, plot = TRUE)#n.s.
DHARMa::testOutliers(pbig3,plot = TRUE)#n.s.

#adding treatment to model to plot it
pbig_treat <- update(pbig3,. ~ . +Treatment)
eff_pbig<-data.frame(ggeffects::ggpredict(pbig_treat,"Treatment"))
eff_pbig$response <- "Big pods"

##plotting final model
#biomass_tree_sc
effec_pbig <- data.frame(ggeffects::ggpredict(pbig3,c("biomass_tree_sc"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_pbig$biomass_tree <- effec_pbig$x * attr(poddata$biomass_tree_sc, 'scaled:scale') + attr(poddata$biomass_tree_sc, 'scaled:center')

effec_pbig$response <- "Big pods ***"

plot_pbig <- ggplot()+
  geom_line(effec_pbig,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_pbig,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(poddata,mapping=aes(x=biomass_tree,y=PodBig),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Predicted nº of pods per tree/month",x="Cacao tree biomass", title="Big size pods ***")+
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
plot_pbig

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_pbig_biomass.png", plot = plot_pbig, width =7 , height = 5) #To save the plot

#' ## i. PodBigMed
###checking for outliers - I already remove them when organising the data
plot(poddata$PodBigMed)

#model fit for PodBigMed
formula_pall <- as.formula("PodBigMed ~ Treatment*shade_treelevel_sc+biomass_tree_sc+(1|Farm/Tree)")
model_fit(formula_pall,poddata)#poisson model is zero-inflated and has outliers; best model: Zero NB (okay - AIC = 1558.2), Quasi (okay - AOC = 1564.1) and then NB(bad but n.s. - AIC = 1654.3); results are basically the same so using NB

#modeling with NB distribution
pall<-glmmTMB(formula_pall,family=nbinom2,data=poddata)
summary(pall)


##model selection
drop1(pall)#Treatment:shade_treelevel_sc                           
pall1 <- update(pall,. ~ . -Treatment:shade_treelevel_sc)
anova(pall,pall1)#KEEP Initial model - p=0.04659 

##final model
summary(pall)#Treatment, shade_treelevel_sc and biomass_tree_sc
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                             1.1875     0.2797   4.246 2.18e-05 ***
#   TreatmentExclosure                     -0.1295     0.2744  -0.472  0.63712    
#   shade_treelevel_sc                      0.2439     0.2289   1.065  0.28676    
#   biomass_tree_sc                         0.4208     0.1605   2.622  0.00875 ** 
#   TreatmentExclosure:shade_treelevel_sc  -0.6520     0.3200  -2.038  0.04158 *

#checking model fit
DHARMa::simulateResiduals(pall, plot = TRUE)#bad
DHARMa::testDispersion(pall,alternative="greater",plot=TRUE)#n.s.
DHARMa::testDispersion(pall,alternative ="less",plot = TRUE)#n.s. 
DHARMa::testZeroInflation(pall, plot = TRUE)#n.s.
DHARMa::testOutliers(pall,plot = TRUE)#n.s.

#plotting treatment with other responses variables in the end
eff_pall<-data.frame(ggeffects::ggpredict(pall,"Treatment"))
eff_pall$response <- "Large pods"

##plotting final model
##Treatment
#ggefects works better when I have interactions but want only a single term
effec_pall <- data.frame(ggeffects::ggpredict(pall, terms = "Treatment"))

plot_pall <- ggplot()+
  geom_point(poddata,mapping=aes(x=Treatment,y=PodBigMed),alpha=.6,colour="grey",fill="grey",shape=21)+
  geom_point(effec_pall,mapping=aes(x=x ,y=predicted ),position=position_dodge(width = 0.5),size=4,shape=19)+
  geom_errorbar(effec_pall,mapping=aes(x=x,ymin=predicted-std.error,ymax=predicted+std.error),width=0.1,position=position_dodge(width = 0.5))+
  #scale_color_colorblind()+
  #scale_fill_colorblind()+
  labs(y="Predicted nº of pods per tree/month",x="Treatment", title="Medium/big size pods")+
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
plot_pall

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_pall_treatment.png", plot = plot_pall, width =7 , height = 5) #To save the plot

#biomass_tree_sc
effec_pall1 <- data.frame(ggeffects::ggpredict(pall,c("biomass_tree_sc"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_pall1$biomass_tree <- effec_pall1$x * attr(poddata$biomass_tree_sc, 'scaled:scale') + attr(poddata$biomass_tree_sc, 'scaled:center')

effec_pall1$response <- "Large pods **"

plot_pall1 <- ggplot()+
  geom_line(effec_pall1,mapping=aes(x=biomass_tree ,y=predicted))+
  geom_ribbon(effec_pall1,mapping=aes(x=biomass_tree,ymin=predicted-std.error,ymax=predicted+std.error),alpha=.2)+
  geom_point(poddata,mapping=aes(x=biomass_tree,y=PodBigMed),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Predicted nº of pods per tree/month",x="Cacao tree biomass", title="Medium/big size pods**")+
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
plot_pall1

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_pall_biomass.png", plot = plot_pall1, width =7 , height = 5) #To save the plot

#Treatment:shade_treelevel_sc
effec_pall2 <- data.frame(ggeffects::ggpredict(pall,c("shade_treelevel_sc","Treatment"))) 

#backtransform `scale()` for plotting
#We can use the attributes to unscale
effec_pall2$shade_treelevel <- effec_pall2$x * attr(poddata$shade_treelevel_sc, 'scaled:scale') + attr(poddata$shade_treelevel_sc, 'scaled:center')

effec_pall2$response <- "Large pods *"

plot_pall2 <- ggplot()+
  geom_line(effec_pall2,mapping=aes(x=shade_treelevel ,y=predicted,colour=group))+
  geom_ribbon(effec_pall2,mapping=aes(x=shade_treelevel,ymin=predicted-std.error,ymax=predicted+std.error,fill=group),alpha=.2)+
  geom_point(poddata,mapping=aes(x=shade_treelevel,y=PodBigMed,colour=Treatment),alpha=.3)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(y="Predicted nº of pods per tree/month",x="Cacao tree shade cover", title="Medium/big size pods *")+
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
plot_pall2

ggsave("outputs/plots/model_results/predict_onlytreevariables/predict_pall_shadeXtreatment.png", plot = plot_pall2, width =7 , height = 5) #To save the plot


#'Plotting
#'Combining all data for plotting
eff_pods <- rbind(eff_flo,eff_psmall,eff_pmed,eff_pbig,eff_pall)

#reorder levels
library(forcats)
eff_pods$response <- as.factor(eff_pods$response)

#reorder and rename levels
eff_pods$response <- factor(eff_pods$response, levels = c("Flowers","Small pods","Medium pods","Big pods","Large pods"))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.5)

#only including flowers, small and large pods
plot <- ggplot(eff_pods[eff_pods$response %in% c("Small pods"),],aes(x=x, y=predicted , colour=x))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),width=.2,position=pd)+
  geom_line(position=pd)+
  geom_point(aes(shape=x),position=pd,size=6)+
  facet_wrap(~response,scales="free_y",nrow=2,ncol=3)+
  #scale_shape(solid=F)+ # no fill for points
  xlab("")+ ylab("Counts (tree/month)")+
  theme_bw() + 
  scale_colour_colorblind()+
  expand_limits(y=0)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x=element_text(size=25,vjust=-1))+
  theme(axis.title.y=element_text(size=25,angle=90,vjust=2))+
  theme(axis.text.x=element_text(size=25,face="bold",vjust=0.5))+
  theme(axis.text.y=element_text(size=25))+
  theme(legend.text=element_text(size=25),legend.title = element_blank(),legend.position="bottom",legend.key = element_blank())+ # Remove only the legend title
  theme(plot.margin = unit(c(1,8,0,8),"cm"))+
  theme(strip.text.x = element_text(size=30))+
  theme(strip.background = element_rect(fill = 'grey'))+
  geom_vline(xintercept=c(1.5),color="red", linetype="dotted",size=1)#create lines between sone variables
plot

ggsave("outputs/plots/model_results/predict_onlytreevariables/effect_treatment_flowers&pods.png", plot = plot, width =15 , height = 6) #To save the plot

#'##Combining groups that had biomass in the final model
eff_biomass <- rbind(effec_pmed2,effec_pbig,effec_pall1)

eff_biomass$response <- as.factor(eff_biomass$response)
table(eff_biomass$response)

write.csv2(eff_biomass,"outputs/data/pods_biomass_plot.csv",row.names = FALSE)

#'##Combining groups that had shadeXtreatment in the final model
eff_shadeXtreat <- rbind(effec_flo1,effec_pmed1,effec_pall2)

eff_shadeXtreat$response <- as.factor(eff_shadeXtreat$response)
table(eff_shadeXtreat$response)

write.csv2(eff_shadeXtreat,"outputs/data/pods_shadeXtreat_plot.csv",row.names = FALSE)

