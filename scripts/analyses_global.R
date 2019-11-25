library(lme4)
library(ggplot2)
library(MuMIn)
library(RColorBrewer)
library(ggpubr)

## globals

vecColors <- brewer.pal(5,"PuBu")
vecColors2 <- c(brewer.pal(11,"PuOr")[c(1,4,10)],"darkgreen")

lev <- c("Diversity","Asynchrony","Combined")
myColors <- c("#4daf4a",vecColors[5], "#ff7f00")
names(myColors) <- factor(lev,levels=lev)



###### Global

## read DF
dfGlobal <- read.csv("datasetsDerived/dataFinal_global.csv")
# remove countries listed by Renard & Tilman 2019
dfGlobal <- dfGlobal[-which(dfGlobal$Area%in%c("Korea, Democratic People's Republic of", "Guinea", "Kenya","Mozambique",
                                               "Zambia","Ireland","New Zealand","Netherlands")),] # note that Ireland was not included anyway
length(unique(dfGlobal$Area))
names(dfGlobal)
table(dfGlobal$IncomeGroup)

#### 1: explore realtionship between diversity and asynchrony
dfDiversityAsynchronyGlobal <- dfGlobal[,c("Area","timePeriod","IncomeGroup","asynchrony","diversity")]
dfDiversityAsynchronyGlobal$timePeriod <- factor(dfDiversityAsynchronyGlobal$timePeriod,levels = c(1968,1978,1988,1998,2008))
dfDiversityAsynchronyGlobal$IncomeGroup <- factor(dfDiversityAsynchronyGlobal$IncomeGroup,levels = c("High income","Upper middle income","Lower middle income","Low income"))

cor.test(dfDiversityAsynchronyGlobal$diversity,dfDiversityAsynchronyGlobal$asynchrony,method='s')

# test if correlation between diversity and asynchrony decreases over time
modDiversityTimeGlobal <- lmer(asynchrony ~ diversity + (1+diversity|timePeriod), data = dfDiversityAsynchronyGlobal)
modDiversityLMGlobal <- lm(asynchrony ~ diversity,data = dfDiversityAsynchronyGlobal)
summary(modDiversityLMGlobal)
anova(modDiversityTimeGlobal,modDiversityLMGlobal) 

modDiversityTimeFixedGlobal=fixef(modDiversityTimeGlobal)
r.squaredGLMM(modDiversityTimeGlobal) 
modDiversityTimeGroupGlobal <-   coef(modDiversityTimeGlobal)$timePeriod

## plot
a1 <- ggplot(dfDiversityAsynchronyGlobal, aes(x=diversity, y=asynchrony, color = timePeriod)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors, labels = c("1961-1970","1971-1980","1981-1990","1991-2000","2001-2010")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityTimeGroupGlobal[,1],slope = modDiversityTimeGroupGlobal[,2],color=vecColors)+
  geom_abline(intercept = summary(modDiversityLMGlobal)$coefficients[1,1],slope = summary(modDiversityLMGlobal)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("") +
  ylab("Asynchrony") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) + 
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 


## test if correlation between diversity and asynchrony differs between income group
modDiversityIncomeGlobal <- lmer(asynchrony ~ diversity + (1+diversity|IncomeGroup), data = dfDiversityAsynchronyGlobal)

anova(modDiversityIncomeGlobal,modDiversityLMGlobal) 

modDiversityIncomeFixedGlobal=fixef(modDiversityIncomeGlobal)
r.squaredGLMM(modDiversityIncomeGlobal) 
modDiversityIncomeGroupGlobal <-   coef(modDiversityIncomeGlobal)$IncomeGroup

## plot
c1 <- ggplot(dfDiversityAsynchronyGlobal, aes(x=diversity, y=asynchrony, color = IncomeGroup)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Income group",values = vecColors2, labels = c("High income","Upper middle income","Lower middle income","Low income")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityIncomeGroupGlobal[,1],slope = modDiversityIncomeGroupGlobal[,2],color=vecColors2)+
  geom_abline(intercept = summary(modDiversityLMGlobal)$coefficients[1,1],slope = summary(modDiversityLMGlobal)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("Diversity") +
  ylab("Asynchrony") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 



#### 2: regression analyses
# transform variables
dfLogGlobal=with(dfGlobal,data.frame(Area,
                                     stability = log(stability),
                                     diversity,
                                     asynchrony, 
                                     meanIrrigation_share=sqrt(meanIrrigation_share),
                                     meanNitrogen_t_ha=sqrt(meanNitrogen_t_ha),
                                     instabilityTemp,instabilityPrec,
                                     meanWarfare,
                                     timePeriod
))
names(dfLogGlobal)
head(dfLogGlobal)

## scale predictors for standardized regression
dfPredictorsGlobal=sapply(dfLogGlobal[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterGlobal=data.frame(Area=dfLogGlobal[,1],stability=dfLogGlobal[,2],dfPredictorsGlobal)
head(dfCenterGlobal)

cor(dfCenterGlobal$diversity,dfCenterGlobal$asynchrony)

# models
modAsynchronyGlobal <- lm(stability~asynchrony+meanIrrigation_share+meanNitrogen_t_ha+meanWarfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modAsynchronyGlobal)

modDiversityGlobal <- lm(stability~diversity+meanIrrigation_share+meanNitrogen_t_ha+meanWarfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modDiversityGlobal)

modTotalGlobal <- lm(stability~diversity+asynchrony+meanIrrigation_share+meanNitrogen_t_ha+meanWarfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modTotalGlobal)

# barplot of effects: combine coefficents of both modesl
dfDiversity <- data.frame(summary(modDiversityGlobal)$coefficients)[2:8,c(1,2,4)]
names(dfDiversity) <- c("Effect","SE","pVal")
colnames(dfDiversity)
dfDiversity$nam <- c("Diversity","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfDiversity <- rbind(dfDiversity[1,],data.frame(Effect=0,SE=0,pVal=NA,nam="Asynchrony"),dfDiversity[2:7,])
dfDiversity$Model <- "Diversity"

dfAsynchrony <- data.frame(summary(modAsynchronyGlobal)$coefficients)[2:8,c(1,2,4)]
names(dfAsynchrony) <- c("Effect","SE","pVal")
colnames(dfAsynchrony)
dfAsynchrony$nam <- c("Asynchrony","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfAsynchrony <- rbind(data.frame(Effect=0,SE=0,pVal=NA,nam="Diversity"),dfAsynchrony)
dfAsynchrony$Model <-  "Asynchrony"

dfTotal <- data.frame(summary(modTotalGlobal)$coefficients)[2:9,c(1,2,4)]
names(dfTotal) <- c("Effect","SE","pVal")
colnames(dfTotal)
dfTotal$nam <- c("Diversity","Asynchrony","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfTotal$Model <- "Combined"

dfCombined <- rbind(dfDiversity,dfAsynchrony,dfTotal)
dfCombined$Model <- factor(dfCombined$Model, levels = unique(dfCombined$Model))
dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
dfCombined$labHeight <- dfCombined$Effect + 0.05
dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"] - 0.05
dfCombined$lab <- ""
dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))

dfCombined <- dfCombined[unlist(lapply(1:8,function(i)seq(i,24,8))),]

dfText <- data.frame(xpos=sort(c(1:8-0.3,1:8,1:8+0.3)),ypos=dfCombined$labHeight,lab=dfCombined$lab,Model=dfCombined$Model)

a2 <- ggplot(data=dfCombined, aes(x=nam, y=Effect, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.6,0.6, by = 0.1),1),limits=c(-0.6,0.6)) +
  # scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Model",values = myColors)+
  geom_hline(yintercept=0)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 


rm(list=ls())

