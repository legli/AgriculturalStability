library(lme4)
library(ggplot2)
library(MuMIn)
library(RColorBrewer)
library(ggpubr)
library(car)
library(plyr)
library(countrycode)
library(grid)
library(rgdal)

## globals

vecColors <- brewer.pal(5,"PuBu")
lev <- c("Diversity","Asynchrony","Combined")
myColors <- c("#4daf4a",vecColors[5], "#ff7f00")
names(myColors) <- factor(lev,levels=lev)

###### Global

## read DF
dfGlobal <- read.csv("datasetsDerived/dataFinal_global_June2020.csv")
names(dfGlobal)


## check minimum richness (should be at least 2 for asynchrony to be meaningful)
min(dfGlobal$richness)

nrow(dfGlobal) # 590 data points
length(unique(dfGlobal$Country)) # 136 countries

names(dfGlobal)

### correlatons
cor.test(dfGlobal$productionStability,dfGlobal$yieldStability,method='s')


#### 1: explore realtionship between diversity and asynchrony
dfDiversityAsynchronyGlobal <- dfGlobal[,c("Country","timePeriod","productionAsynchrony","diversity")]
names(dfDiversityAsynchronyGlobal)[3] <- "asynchrony"
dfDiversityAsynchronyGlobal$timePeriod <- factor(dfDiversityAsynchronyGlobal$timePeriod,levels = c(1961,1971,1981,1991,2001))

cor.test(dfDiversityAsynchronyGlobal$diversity,dfDiversityAsynchronyGlobal$asynchrony,method='s')

# test if correlation between diversity and asynchrony decreases over time
modDiversityTimeGlobal <- lmer(asynchrony ~ diversity + (1+diversity|timePeriod), data = dfDiversityAsynchronyGlobal)
modDiversityLMGlobal <- lm(asynchrony ~ diversity,data = dfDiversityAsynchronyGlobal)
summary(modDiversityLMGlobal)
anova(modDiversityTimeGlobal,modDiversityLMGlobal)  # AIC: -336.59, -340.22

modDiversityTimeFixedGlobal=fixef(modDiversityTimeGlobal)
r.squaredGLMM(modDiversityTimeGlobal) 
modDiversityTimeGroupGlobal <-   coef(modDiversityTimeGlobal)$timePeriod

## plot
a1 <- ggplot(dfDiversityAsynchronyGlobal, aes(x=diversity, y=asynchrony, color = timePeriod)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors, labels = c("1961-1970","1971-1980","1981-1990","1991-2000","2001-2010")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityTimeGroupGlobal[,1],slope = modDiversityTimeGroupGlobal[,2],color=vecColors)+
  # geom_abline(intercept = summary(modDiversityLMGlobal)$coefficients[1,1],slope = summary(modDiversityLMGlobal)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("Diversity") +
  ylab("Asynchrony") + 
  theme(axis.title.x = element_text(size=8,vjust = 33)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) + 
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 


#### 2: predictors of production stability
# transform variables
dfLogGlobal=with(dfGlobal,data.frame(Country,
                                     stability = log(productionStability),
                                     diversity,
                                     asynchrony=productionAsynchrony, 
                                     irrigation=sqrt(irrigation_share),
                                     nitrogen=sqrt(nitrogen_ha),
                                     instabilityTemp,instabilityPrec,
                                     warfare,
                                     timePeriod
))
names(dfLogGlobal)
head(dfLogGlobal)

## scale predictors for standardized regression
dfPredictorsGlobal=sapply(dfLogGlobal[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterGlobal=data.frame(Country=dfLogGlobal[,1],stability=dfLogGlobal[,2],dfPredictorsGlobal)
head(dfCenterGlobal)

cor(dfCenterGlobal$diversity,dfCenterGlobal$asynchrony)

# models
modAsynchronyGlobal <- lm(stability~asynchrony+irrigation+nitrogen+warfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modAsynchronyGlobal) # R2: 0.5974
AIC(modAsynchronyGlobal) # AIC: 819.6671
vif(modAsynchronyGlobal) # vif < 2

modDiversityGlobal <- lm(stability~diversity+irrigation+nitrogen+warfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modDiversityGlobal) # 0.2757
AIC(modDiversityGlobal) # AIC: 1166.13
vif(modDiversityGlobal) # vif < 2

modTotalGlobal <- lm(stability~diversity+asynchrony+irrigation+nitrogen+warfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modTotalGlobal) # R2: 0.6119
AIC(modTotalGlobal) # AIC: 800.0929
vif(modTotalGlobal) # vif < 2


# check colinearity using both orders
modTotalD <- lm(stability~diversity+asynchrony+irrigation+nitrogen+warfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
modTotalA <- lm(stability~asynchrony+diversity+irrigation+nitrogen+warfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)

anovaD <- anova(modTotalD)
anovaA <- anova(modTotalA)
# asynchrony: F(1,589) = 650.4092 , P < 0.05; F(1,589) = 738.9372, P < 0.05
# diversity : F(1,589) =  99.0602, P < 0.05; F(1,589) =  10.5322, P < 0.05

## barplot of effects: combine coefficents of both modesl
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

b1 <- ggplot(data=dfCombined, aes(x=nam, y=Effect, fill=Model)) +
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


################# Fig 1: regression results
jpeg("results/Fig1_June2020.jpeg", width = 16.9, height = 16.9*2/3, units = 'cm', res = 600)

ggarrange(a1,b1,
          labels = letters[1:2],font.label=list(size=8),
          ncol=2,nrow = 1,widths =  c(1,1.5), align="h")

dev.off()




################# Fig 2: worldmap
## read shape
ctryMapOriginal <- readOGR("spatial/countries_global.shp")
ctryMapOriginal$Country <-  countrycode(ctryMapOriginal$Area, 'country.name', 'iso3c')

## extract most recent period
dfGroups <- dfGlobal[which(dfGlobal$timePeriod==2001),]
sort(as.character(setdiff(dfGroups$Country,ctryMapOriginal$Country)))

## calculate quantiles
hist(dfGroups$productionAsynchrony)
hist(dfGroups$productionStability)
grd <- rbind(data.frame(dim2=3,dim1=3,color="#3F2949"),
             data.frame(dim2=2,dim1=3,color="#435786"),
             data.frame(dim2=1,dim1=3,color="#4885C1"),
             data.frame(dim2=3,dim1=2,color="#77324C"),
             data.frame(dim2=2,dim1=2,color="#806A8A"),
             data.frame(dim2=1,dim1=2,color="#89A1C8"),
             data.frame(dim2=3,dim1=1,color="#AE3A4E"),
             data.frame(dim2=2,dim1=1,color="#BC7C8F"),
             data.frame(dim2=1,dim1=1,color="#CABED0"))

grd$color <- as.character(grd$color)

trintAsynchrony <- as.numeric(quantile(dfGroups$productionAsynchrony,probs=seq(0,1,length.out = 4)))
trintStability <- as.numeric(quantile(dfGroups$productionStability,probs=seq(0,1,length.out = 4)))

dfGroups$dim1 <-car::recode(dfGroups$productionAsynchrony,"trintAsynchrony[1]:trintAsynchrony[2]=1; trintAsynchrony[2]:trintAsynchrony[3]=2; trintAsynchrony[3]:trintAsynchrony[4]=3;")
dfGroups$dim2 <-car::recode(dfGroups$productionStability,"trintStability[1]:trintStability[2]=1; trintStability[2]:trintStability[3]=2; trintStability[3]:trintStability[4]=3;")

## join to map
dfGroupsFinal <- merge(dfGroups[,c("Country", "dim1","dim2")],grd)
head(dfGroupsFinal)
dfGroupsFinal$id <- as.character(dfGroupsFinal$Country)
mapsBivariate <- fortify(ctryMapOriginal,region="Country")
mapsBivariate = join(mapsBivariate, dfGroupsFinal[,c("id","color")], by="id")

## create legend
g.legend <- ggplot(grd, aes(dim1,dim2,fill=factor(1:9)))+
  geom_tile()+
  scale_fill_manual(values=grd$color)+
  theme_void()+
  theme(legend.position="none",axis.title=element_text(size=5),
        panel.background=element_blank(),plot.margin=margin(t=10,b=10,l=10))+
  theme(axis.title=element_text(color="black",size=8),
        axis.title.y = element_text(angle = 90))+
  labs(x="Asynchrony",y="Stability") 
# theme(axis.title=element_text(size=8))

vp<-viewport(width=0.24,height=0.4,x=0.12,y=0.3)

## plot
jpeg("results/Fig2_June2020.jpeg", width = 8, height = 5, units = 'cm', res = 600)

ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")
#http://lenkiefer.com/2017/04/24/bivariate-map/

print(g.legend,vp=vp)

dev.off()


##### Contribution of production

# get proportional production per time period
dfProdProp <- dfGlobal[which(dfGlobal$timePeriod==2001),]
dfProdProp$productionProp <- (dfProdProp$production/(sum(dfProdProp$production)))*100


dfLow <- dfProdProp[which(dfProdProp$productionAsynchrony<trintAsynchrony[2]&dfProdProp$productionStability<trintStability[2]),]
dfLowFinal <- dfLow[order(dfLow$productionProp,decreasing = T),c("Country","productionProp")]
dfLowFinal$sum <- cumsum(dfLowFinal$productionProp) # Russia, Argentinia and Australia (total 6.870877% contribution) are the highest unstable contributors; totally 29 countries contribute to 11.785819% of global calorie production

# dfMedium <- dfProdProp[which(dfProdProp$productionAsynchrony>trintAsynchrony[2]&dfProdProp$productionAsynchrony<trintAsynchrony[3]&dfProdProp$productionStability>trintStability[2]&dfProdProp$productionStability<trintStability[3]),]
# dfMediumFinal <- dfMedium[order(dfMedium$productionProp,decreasing = T),c("Area","productionProp")]
# dfMediumFinal$sum <- cumsum(dfMediumFinal$productionProp)


######## SUPPLEMENTARY MATERIALS

## Extended Data Fig. 2 
dfPredict <- data.frame(diversity=rep(0,1000),asynchrony=0,instabilityTemp=0,instabilityPrec=0,irrigation=0,nitrogen=0,warfare=0,timePeriod=0)


funPlot <- function(predictor,predictorOrig,trans,xlabel,ylabel,modD,modA, posX,posY){
  
  dfPredictNew <- dfPredict
  dfPredictNew[,predictor] <-  seq(min(dfCenterGlobal[,predictor]), max(dfCenterGlobal[,predictor]), length.out = 1e3)
  
  # combined model
  pred <- exp(data.frame(predict(modTotalGlobal,newdata = dfPredictNew, 
                                 interval = "confidence")))    
  
  if (trans==""){
    pred$variable <- dfPredictNew[,predictor]*sd(dfLogGlobal[,predictor])+mean(dfLogGlobal[,predictor])
    
  }
  if (trans=="sqrt"){
    pred$variable <- (dfPredictNew[,predictor]*sd(dfLogGlobal[,predictor])+mean(dfLogGlobal[,predictor]))^2
  }    
  
  pred$Model <- factor("Combined",levels=lev)
  
  
  if (modD){
    predDiversity <- exp(data.frame(predict(modDiversityGlobal,newdata = dfPredictNew, 
                                            interval = "confidence")))    
    
    if (trans==""){
      predDiversity$variable <- dfPredictNew[,predictor]*sd(dfLogGlobal[,predictor])+mean(dfLogGlobal[,predictor])
      
    }
    if (trans=="sqrt"){
      predDiversity$variable <- (dfPredictNew[,predictor]*sd(dfLogGlobal[,predictor])+mean(dfLogGlobal[,predictor]))^2
    }    
    predDiversity$Model <- factor("Diversity",levels=lev)
    pred <- rbind(pred,predDiversity)
  }
  
  if (modA){
    predAsynchrony <- exp(data.frame(predict(modAsynchronyGlobal,newdata = dfPredictNew, 
                                             interval = "confidence")))
    
    if (trans==""){
      predAsynchrony$variable <- dfPredictNew[,predictor]*sd(dfLogGlobal[,predictor])+mean(dfLogGlobal[,predictor])
      
    }
    if (trans=="sqrt"){
      predAsynchrony$variable <- (dfPredictNew[,predictor]*sd(dfLogGlobal[,predictor])+mean(dfLogGlobal[,predictor]))^2
    }  
    predAsynchrony$Model <- factor("Asynchrony",levels=lev)
    pred <- rbind(pred,predAsynchrony)
  }
  
  dfOriginal <- dfGlobal
  dfOriginal$variable <- dfOriginal[,predictorOrig]
  dfOriginal$Model <- ""
  ggplot(data = dfOriginal, aes(x = variable, y = stability, color=Model)) +
    # geom_point() +
    geom_line(data = pred, aes(y = fit,color=Model),size=0.5)+
    geom_ribbon(data = pred, aes(y = fit, ymin = lwr, ymax = upr, fill = Model), alpha = 0.5,colour=NA) +
    theme_classic() +
    theme(axis.title=element_text(size=6),axis.text=element_text(size=6)) +
    xlab(xlabel)+
    ylab(ylabel)+
    ylim(0,110)+
    scale_colour_manual(name = "Model",values = myColors)+
    scale_fill_manual(name = "Model",values = myColors)+
    theme(legend.position = c(posX, posY))+
    theme(legend.title = element_text(size = 6),
          legend.text = element_text(size = 6))+    
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))
}

a <- funPlot(predictor="diversity",predictorOrig="diversity",trans="",xlabel="Diversity",ylabel=expression(paste("Production stability (",mu,"/",sigma,")")),modD=T,modA=F,posX=-9999,posY=-9999)
b <- funPlot(predictor = "asynchrony", predictorOrig ="productionAsynchrony",trans="",xlabel="Asynchrony",ylabel="",modD=F,modA=T,posX=-9999,posY=-9999)
c <- funPlot(predictor="irrigation",predictorOrig="irrigation_share",trans="sqrt",xlabel="Irrigation (%)",ylabel="",modD=T,modA=T,posX=-9999,posY=-9999)
d <- funPlot(predictor="nitrogen",predictorOrig="nitrogen_ha",trans="sqrt",xlabel="N use intensity (t/ha)",ylabel=expression(paste("Production stability (",mu,"/",sigma,")")),modD=T,modA=T,posX=-9999,posY=-9999)
e <- funPlot(predictor="instabilityTemp",predictorOrig="instabilityTemp",trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="",modD=T,modA=T,posX=-9999,posY=-9999)
f <- funPlot(predictor="instabilityPrec",predictorOrig="instabilityPrec",trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",modD=T,modA=T,posX=-9999,posY=-9999)
g <- funPlot(predictor="warfare",predictorOrig="warfare",trans="",xlabel="Warfare",ylabel=expression(paste("Production stability (",mu,"/",sigma,")")),modD=T,modA=T,posX=-9999,posY=-9999)
h <- funPlot(predictor="timePeriod",predictorOrig="timePeriod",trans="",xlabel="Time",ylabel="",modD=T,modA=T,posX=-9999,posY=-9999)
leg <- funPlot(predictor="instabilityPrec",predictorOrig="instabilityPrec",trans="",xlabel="",ylabel="",modD=T,modA=T,posX=0.5,posY=0.5)
legend <- cowplot::get_legend(leg)

jpeg("results/ExtendedDataFig1_June2020.jpeg", width = 16.9, height = 16.9, units = 'cm', res = 600)

ggarrange(a, b, c, d, e,f,g,h,legend,
          labels = c(letters[1:8]),font.label=list(size=6),
          ncol = 3, nrow = 3,heights = c(1,1))

dev.off()



## Extended Data Table 2: regression results of combined model
dfDiversity <- data.frame(summary(modDiversityGlobal)$coefficients)
indLow <- which(dfDiversity$Pr...t..<0.0001)
dfDiversity <- round(dfDiversity,2)
row.names(dfDiversity) <- c("(Intercept)","Diversity","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfDiversity$Estimate <- paste0(dfDiversity$Estimate," (",dfDiversity$Std..Error,")")
dfDiversity[indLow,4] <- "<0.0001"
dfDiversity <- rbind(dfDiversity[1:2,],data.frame(Estimate=NA,Std..Error=NA,t.value=NA,Pr...t..=NA),dfDiversity[3:8,]) 
colnames(dfDiversity) <- c("Estimate (SE)","SE","T","p-value") 
rownames(dfDiversity)[3] <- "Asynchrony"


dfAsynchrony <- data.frame(summary(modAsynchronyGlobal)$coefficients)
indLow <- which(dfAsynchrony$Pr...t..<0.0001)
dfAsynchrony <- round(dfAsynchrony,2)
row.names(dfAsynchrony) <- c("(Intercept)","Asynchrony","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfAsynchrony$Estimate <- paste0(dfAsynchrony$Estimate," (",dfAsynchrony$Std..Error,")")
dfAsynchrony[indLow,4] <- "<0.0001"
dfAsynchrony <- rbind(dfAsynchrony[1,],data.frame(Estimate=NA,Std..Error=NA,t.value=NA,Pr...t..=NA),dfAsynchrony[2:8,]) 
colnames(dfAsynchrony) <- c("Estimate (SE)","SE","T","p-value") 
rownames(dfAsynchrony)[2] <- "Diversity"

dfTotal <- data.frame(summary(modTotalGlobal)$coefficients)
indLow <- which(dfTotal$Pr...t..<0.0001)
dfTotal <- round(dfTotal,2)
row.names(dfTotal) <- c("(Intercept)","Diversity","Asynchrony","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfTotal$Estimate <- paste0(dfTotal$Estimate," (",dfTotal$Std..Error,")")
colnames(dfTotal) <- c("Estimate (SE)","SE","T","p-value") 
dfTotal[indLow,4] <- "<0.0001"

dfFinalTable <- cbind(dfDiversity[,c(1,3,4)],dfAsynchrony[,c(1,3,4)],dfTotal[,c(1,3,4)])

write.csv(dfFinalTable,"results/ExtendedDataTable1_June2020.csv")


rm(list=ls())

