library(rgdal)
library(tidyr)
library(vegan)
library(countrycode)
library(rworldmap) 
library(codyn)

######## DATA PREPARATION

#### currently exisiting regions  
levelMap <- getMap()
plot(levelMap)
vecLevel <- levelMap@data$ISO3 

#### cropland data in focal time period
dfCropland <- read.csv("datasets/cropland_global.csv")
dfCropland <- dfCropland[which(dfCropland$Year%in%1961:2010),]


# adapt region names
dfCropland$Level <- countrycode(dfCropland$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfCropland$Area) <- c(levels(dfCropland$Area),"Swaziland")
dfCropland[which(grepl("Eswatini",dfCropland$Area)),"Area"] <- "Swaziland"
dfCropland$Level <- countrycode(dfCropland$Area, 'country.name', 'iso3c') # no important regions missing
dfCropland<-dfCropland[which(dfCropland$Area!="China, mainland"),]
sort(as.character(setdiff(vecLevel,dfCropland$Level)))
dfCropland <- dfCropland[which(dfCropland$Level%in%vecLevel),] # only keep current regions

# keep necessary columns
dfCropland <- dfCropland[,c("Level","Year","Value")]
dfCropland$croplandArea <- dfCropland$Value*1000 # convert to ha
dfCropland <- dfCropland[,c("Level","Year","croplandArea")]

# only keep the regions covering 99.9% of the total cropland area 
sum(is.na(dfCropland))
dfCroplandMean <- aggregate(croplandArea~Level,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$croplandArea/sum(dfCroplandMean$croplandArea)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]
dfCropland <- dfCropland[which(dfCropland$Level%in%dfCroplandMean[1:ind,"Level"]),] # this are the target regions
vecLevel <- unique(dfCropland$Level) ## target countries
nrow(unique(dfCropland[,c("Level","Year")])) == nrow(dfCropland) # check duplicates


#### agricultural production data
dfProduction <- read.csv("datasets/agriculturalProduction_global.csv")
names(dfProduction)

# adapt region names
dfProduction$Level <- countrycode(dfProduction$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfProduction$Area) <- c(levels(dfProduction$Area),"Swaziland")
dfProduction[which(grepl("Eswatini",dfProduction$Area)),"Area"] <- "Swaziland"
dfProduction$Level <- countrycode(dfProduction$Area, 'country.name', 'iso3c') # no important regions missing
dfProduction<-dfProduction[which(dfProduction$Area!="China, mainland"),]
sort(as.character(setdiff(vecLevel,dfProduction$Level)))
dfProduction <- dfProduction[which(dfProduction$Level%in%vecLevel),] # only keep current regions

# only keep area harvested and production, relevant columns and target years
names(dfProduction)
dfProduction <- dfProduction[which(dfProduction$Element=="Area harvested" | dfProduction$Element == "Production"),c(65,4,6,8:57)]
head(dfProduction)
names(dfProduction)[4:53] <- 1961:2010 # change colnames to years

# change dataset structure (frow wide to long)
dfProduction <- dfProduction %>% gather(Year, Value, "1961":"2010")
head(dfProduction)
dfProduction$Year <- as.numeric(dfProduction$Year)
# remove NA
dfProduction <- na.omit(dfProduction) # to ensure unique keys
# split area harvested and production in two columns (from long to wide)
dfProduction <- dfProduction %>% spread(Element, Value)
head(dfProduction)
names(dfProduction)[4] <- "AreaHarvested"

# set NA for harvested data for which there is no production data and vice versa 
dfProduction[is.na(dfProduction$Production),"AreaHarvested"] <- NA
dfProduction[is.na(dfProduction$AreaHarvested),"Production"] <- NA
dfProduction <- dfProduction[which(!is.na(dfProduction$AreaHarvested) & !is.na(dfProduction$Production)),]
# remove zero areas and production
dfProduction <- dfProduction[-which(dfProduction$AreaHarvested==0 |  dfProduction$Production==0),]

## add calories and make crops consistent with target crop file
dfCalories <- read.csv("datasets/cropCalories_global.csv")
head(dfCalories) 
dfCalories <- dfCalories[which(!is.na(dfCalories$Calories_kcal_t)),]
setdiff(dfProduction$Item,dfCalories$Item) # missing calorie values

# subset crops without calories
dfProduction <- merge(dfProduction,dfCalories[,c("Item","Calories_kcal_t")],by="Item")
names(dfProduction)

# change production to calories
dfProduction$Production <- dfProduction$Production*dfProduction$Calories_kcal_t
# calculate individual yields and remove very unrealistic values
dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
hist(dfProduction$Yield)
# dfProduction<-dfProduction[which(dfProduction$Yield<1e+09),]

# keep necessary columns only 
dfProduction <- dfProduction[,c("Level","Item","Year","AreaHarvested","Production")]

# only keep crops with 10 entries per time period
dfProduction$timePeriod=0
dfProduction[dfProduction$Year%in%c(1961:1970),"timePeriod"] = 1961
dfProduction[dfProduction$Year%in%c(1971:1980),"timePeriod"] = 1971
dfProduction[dfProduction$Year%in%c(1981:1990),"timePeriod"] = 1981
dfProduction[dfProduction$Year%in%c(1991:2000),"timePeriod"] = 1991
dfProduction[dfProduction$Year%in%c(2001:2010),"timePeriod"] = 2001

sum(is.na(dfProduction))
dfProduction$sum <- 1
dfCount <- aggregate(sum~Level+timePeriod+Item,dfProduction,sum)
head(dfCount)

dfProduction <- merge(dfProduction[,c("Level","Item","Year","AreaHarvested","Production","timePeriod")],dfCount)
dfProduction <- dfProduction[which(dfProduction$sum==10),c("Level","Item","Year","AreaHarvested","Production")]
length(unique(dfProduction$Item)) ## 133 crops

# calculate crop specific yields
dfProduction$Yield <- dfProduction$Production/dfProduction$AreaHarvested

nrow(unique(dfProduction[,c("Level","Year","Item")])) == nrow(dfProduction) # check duplicates

#### calculate overall yields
sum(is.na(dfProduction))
dfYield <- aggregate(cbind(Production,AreaHarvested)~Level+Year,dfProduction,sum)
head(dfYield)
dfYield$Yield <- dfYield$Production/dfYield$AreaHarvested
nrow(unique(dfYield[,c("Level","Year")])) == nrow(dfYield) # check duplicates


#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AreaHarvested~Level+Year,dfProduction,function(x){exp(diversity(x,index="shannon"))})
head(dfShannon)
names(dfShannon)[3] <- "diversity"
nrow(dfShannon)==nrow(dfYield)
nrow(unique(dfShannon[,c("Level","Year")])) == nrow(dfShannon) # check duplicates


#### Fertilizer data
dfFertilizerArchive <- read.csv("datasets/fertilizerArchive_global.csv")
names(dfFertilizerArchive)[4] <- "Area"
dfFertilizerNew <- read.csv("datasets/fertilizerNew_global.csv")
dfFertilizer <- rbind(dfFertilizerArchive[,c("Area","Year","Item","Value")],dfFertilizerNew[,c("Area","Year","Item","Value")])
dfFertilizer <- dfFertilizer[which(dfFertilizer$Item=="Nitrogenous fertilizers"| dfFertilizer$Item=="Nutrient nitrogen N (total)"),]
dfFertilizer <- dfFertilizer[,c("Area","Year","Value")]
names(dfFertilizer)[3] <- "Nitrogen"
head(dfFertilizer)

# only keep target year
dfFertilizer <- dfFertilizer[which(dfFertilizer$Year%in%1961:2010),]

# adapt region names
dfFertilizer$Level <- countrycode(dfFertilizer$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfFertilizer$Area) <- c(levels(dfFertilizer$Area),"Swaziland")
dfFertilizer[which(grepl("Eswatini",dfFertilizer$Area)),"Area"] <- "Swaziland"
dfFertilizer$Level <- countrycode(dfFertilizer$Area, 'country.name', 'iso3c') # no important regions missing
dfFertilizer<-dfFertilizer[which(dfFertilizer$Area!="China, mainland"),]
sort(as.character(setdiff(vecLevel,dfFertilizer$Level)))
dfFertilizer <- dfFertilizer[which(dfFertilizer$Level%in%vecLevel),] # only keep current regions

# target columns
dfFertilizer <- dfFertilizer[,c("Level","Year","Nitrogen")]
nrow(unique(dfFertilizer[,c("Level","Year")])) == nrow(dfFertilizer) # check duplicates


## Irrigation
dfIrrigation <- read.csv("datasets/irrigationEquippedArea_global.csv")
head(dfIrrigation)

# only keep target year
dfIrrigation <- dfIrrigation[which(dfIrrigation$Year%in%1961:2010),]

# adapt region names
dfIrrigation$Level <- countrycode(dfIrrigation$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfIrrigation$Area) <- c(levels(dfIrrigation$Area),"Swaziland")
dfIrrigation[which(grepl("Eswatini",dfIrrigation$Area)),"Area"] <- "Swaziland"
dfIrrigation$Level <- countrycode(dfIrrigation$Area, 'country.name', 'iso3c') # no important regions missing
dfIrrigation<-dfIrrigation[which(dfIrrigation$Area!="China, mainland"),]
sort(as.character(setdiff(vecLevel,dfIrrigation$Level)))
dfIrrigation <- dfIrrigation[which(dfIrrigation$Level%in%vecLevel),] # only keep current regions
names(dfIrrigation)[12] <- "Irrigation"

# target columns
dfIrrigation <- dfIrrigation[,c("Level","Year","Irrigation")]
nrow(unique(dfIrrigation[,c("Level","Year")])) == nrow(dfIrrigation) # check duplicates

## Warfare
dfWarfare <- read.csv("datasets/warfare_global.csv")
names(dfWarfare)[3] <- "Area"

# only keep target year
dfWarfare <- dfWarfare[which(dfWarfare$YEAR%in%1961:2010),]
dfWarfare <- na.omit(dfWarfare) # remove NA

# add former divided Germany: always 0
dfWarfare[which(dfWarfare$Area=="Germany")[1],]
dfG <- data.frame(SCODE="GMY",CCODE = 255, Area="Germany",YEAR=1961:1990,ACTOTAL=0)
dfWarfare <- rbind(dfWarfare,dfG)

# combine north and south yemen
dfWarfare[which(dfWarfare$Area=="Yemen")[1],]
dfYemenN <- dfWarfare[which(dfWarfare$Area=="Yemen, North"&dfWarfare$YEAR%in%1961:1989),]
dfYemenN$SCODE <- "YEM"
dfYemenN$CCODE <- 679
dfYemenN$Area <- "Yemen"
dfYemenS <- dfWarfare[which(dfWarfare$Area=="Yemen, South"&dfWarfare$YEAR%in%1961:1989),]
dfYemenS$SCODE <- "YEM"
dfYemenS$CCODE <- 679
dfYemenS$Area <- "Yemen"
dfYemenT <- merge(dfYemenN,dfYemenS,by=c("SCODE","CCODE","Area","YEAR"),all=T)
head(dfYemenT)
dfYemenT$ACTOTAL <- rowSums(dfYemenT[,c("ACTOTAL.x","ACTOTAL.y")],na.rm=T)
# remove formerly divided countries
dfWarfare <- rbind(dfWarfare[-which(dfWarfare$Area%in%c("Germany East","Germany West","Yemen, North","Yemen, South","Vietnam North","Vietnam South","Vietnam, North","Vietnam, South")),],dfYemenT[,c(1:4,7)])

# replace Russria
levels(dfWarfare$Area) <- c(levels(dfWarfare$Area),"Russia")
dfWarfare[which(dfWarfare$Area=="RUSRia"),"Area"] <- "Russia"

# add iso 3 codes
dfWarfare$Level <- countrycode(dfWarfare$Area, 'country.name', 'iso3c') 
dfWarfare <- na.omit(dfWarfare)
sort(as.character(setdiff(vecLevel,dfWarfare$Level)))

# target columns
dfWarfare <- dfWarfare[,c("Level","YEAR","ACTOTAL")]
names(dfWarfare) <- c("Level","Year","warfare")
nrow(unique(dfWarfare[,c("Level","Year")])) == nrow(dfWarfare) # check duplicates


#### Climate
# intersect file (intersection of country borders and climate pixels)
shpClimateID <- readOGR("spatial","countriesClimateID_global")
head(shpClimateID@data)
dfClimateID <- shpClimateID@data
dfClimateIDsum <- aggregate(areaHA~cellID,dfClimateID,function(i){sum(i,na.rm=T)})
names(dfClimateIDsum)[2] <- "areaTot"
dfClimateID <- merge(dfClimateID,dfClimateIDsum,by="cellID")
head(dfClimateID)

# climate file
load("datasets/climate_global.RData")
names(dfClimateFinalPrint)
head(dfClimateFinalPrint)
dfClimateFinalPrint <- dfClimateFinalPrint[,c(1:101,116:121)]

# merge
dfClimateFinalArea <- merge(dfClimateID,dfClimateFinalPrint,by="cellID")
head(dfClimateFinalArea)
length(unique(dfClimateFinalArea$cellID)) / nrow(dfClimateFinalArea)
names(dfClimateFinalArea)
# get cropland by region
dfCroplandTot <- aggregate(cbind(cropland1970AD,cropland1980AD,cropland1990AD,cropland2000AD,cropland2010AD)~Area,dfClimateFinalArea,sum)
head(dfCroplandTot)
names(dfCroplandTot)[2:6] <- paste0(names(dfCroplandTot)[2:6],"Tot")
dfClimateFinalArea <- merge(dfClimateFinalArea,dfCroplandTot,by="Area")
head(dfClimateFinalArea)
names(dfClimateFinalArea)
## weighted average: get area of cropland attributed to a cell segment (i.e. cropland area multiplied by area share of the segment), divide it by total cropland area across region
dfClimateFinalArea$weight1 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1970AD)/dfClimateFinalArea$cropland1970ADTot)
dfClimateFinalArea$weight2 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1980AD)/dfClimateFinalArea$cropland1980ADTot)
dfClimateFinalArea$weight3 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1990AD)/dfClimateFinalArea$cropland1990ADTot)
dfClimateFinalArea$weight4 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2000AD)/dfClimateFinalArea$cropland2000ADTot)
dfClimateFinalArea$weight5 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2010AD)/dfClimateFinalArea$cropland2010ADTot)
hist(dfClimateFinalArea$weight5)

## multiply weight by climate values
names(dfClimateFinalArea)
dfClimateFinalArea[,7:26] <- dfClimateFinalArea[,7:26]*dfClimateFinalArea$weight1
dfClimateFinalArea[,27:46] <- dfClimateFinalArea[,27:46]*dfClimateFinalArea$weight2
dfClimateFinalArea[,47:66] <- dfClimateFinalArea[,47:66]*dfClimateFinalArea$weight3
dfClimateFinalArea[,67:86] <- dfClimateFinalArea[,67:86]*dfClimateFinalArea$weight4
dfClimateFinalArea[,87:106] <- dfClimateFinalArea[,87:106]*dfClimateFinalArea$weight5

# sum weighted values to get overall weighted average
names(dfClimateFinalArea)
dfClimateFinalAreaAgg <- aggregate(dfClimateFinalArea[,7:106],by=list(dfClimateFinalArea$Area),FUN=function(i){sum(i,na.rm=T)})
head(dfClimateFinalAreaAgg)
names(dfClimateFinalAreaAgg)[1] <- "Area"
min(dfClimateFinalAreaAgg[2:101]) # check for negative values -> would be problematic for instability calculation
sum(dfClimateFinalAreaAgg[2:101]<0) # remove negative values
dfClimateFinalAreaAgg$neg <- apply(dfClimateFinalAreaAgg[,2:101],1,function(r){sum(r<0)})
dfClimateFinalAreaAgg <- dfClimateFinalAreaAgg[which(dfClimateFinalAreaAgg$neg==0),1:101]

# change structure
names(dfClimateFinalAreaAgg)
dfClimateFinal <- dfClimateFinalAreaAgg %>% gather(climYear, Value, names(dfClimateFinalAreaAgg)[2:101])
head(dfClimateFinal)
dfClimateFinal$Year <- as.numeric(substr(dfClimateFinal$climYear,9,12))
dfClimateFinal$Element <- substr(dfClimateFinal$climYear,1,8)
dfClimateFinal <- dfClimateFinal[,c("Area","Year","Element","Value")]
dfClimateFinal <- dfClimateFinal %>% spread(Element, Value)
head(dfClimateFinal)
nrow(dfClimateFinal)

# adapt region names
dfClimateFinal$Level <- countrycode(dfClimateFinal$Area, 'country.name', 'iso3c') # no important regions missing
dfClimateFinal<-dfClimateFinal[which(dfClimateFinal$Area!="China, mainland"),]
sort(as.character(setdiff(vecLevel,dfClimateFinal$Level)))
dfClimateFinal <- dfClimateFinal[which(dfClimateFinal$Level%in%vecLevel),] # only keep current regions

# target columns
dfClimateFinal <- dfClimateFinal[,c("Level","Year","meanTemp","meanPrec")]
nrow(unique(dfClimateFinal[,c("Level","Year")])) == nrow(dfClimateFinal) # check duplicates




######## CALCULATE VARIABLES FOR THE 5 TIME PERIOS

# get regions across datasets
vecLevelFinal <- Reduce(intersect,list(dfCropland$Level,dfProduction$Level,dfYield$Level,dfShannon$Level,dfFertilizer$Level,dfIrrigation$Level,dfWarfare$Level,dfClimateFinal$Level))

# remove countries listed by Renard & Tilman 2019
vecLevelFinal <- vecLevelFinal[-which(vecLevelFinal%in%c("EGY","PRK", "GIN", "KEN","MOZ","ZMB","IRL","NLD","NZL"))] 



## summarize per time frame 
lsAll <- lapply(vecLevelFinal,function(reg){
  # detrend yields
  show(as.character(reg))
  lsAggregate <- lapply(c(1961,1971,1981,1991,2001),function(yearStart){
    
    sumLevel <- sum(dfYield$Level==reg&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9))
    if(sumLevel==10){
      # subset data for the target country
      dfProductionLevel <- dfProduction[which(dfProduction$Level==reg&dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
      dfYieldLevel <- dfYield[which(dfYield$Level==reg&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9)),]
      dfYieldLevel$YieldDet <- resid(lm(Yield ~ Year^2,data=dfYieldLevel))
      dfYieldLevel$YProductionDet <- resid(lm(Production ~ Year^2,data=dfYieldLevel))
      dfShannonLevel <- dfShannon[which(dfShannon$Level==reg&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
      dfCroplandLevel <- dfCropland[which(dfCropland$Level==reg&dfCropland$Year>=yearStart&dfCropland$Year<=(yearStart+9)),]
      dfFertilizerLevel <- dfFertilizer[which(dfFertilizer$Level==reg&dfFertilizer$Year>=yearStart&dfFertilizer$Year<=(yearStart+9)),]
      dfIrrigationLevel <- dfIrrigation[which(dfIrrigation$Level==reg&dfIrrigation$Year>=yearStart&dfIrrigation$Year<=(yearStart+9)),]
      dfClimateLevel <- dfClimateFinal[which(dfClimateFinal$Level==reg&dfClimateFinal$Year>=yearStart&dfClimateFinal$Year<=(yearStart+9)),]
      dfWarfareLevel <- dfWarfare[which(dfWarfare$Year>=yearStart&dfWarfare$Year<=(yearStart+9)),]
      
      dfSummary <- data.frame(Level=reg, timePeriod= yearStart)
      dfSummary$yieldStability <- mean(dfYieldLevel$Yield,na.rm=T)/sd(dfYieldLevel$YieldDet,na.rm=T)
      dfSummary$productionStability <- mean(dfYieldLevel$Production,na.rm=T)/sd(dfYieldLevel$YProductionDet,na.rm=T)
      dfSummary$yield <- mean(dfYieldLevel$Yield,na.rm=T)
      dfSummary$production <- mean(dfYieldLevel$Production,na.rm=T)
      dfSummary$areaHarvested <- mean(dfYieldLevel$AreaHarvested,na.rm=T)
      dfSummary$richness <- length(unique(dfProductionLevel$Item))
      dfSummary$diversity <- mean(dfShannonLevel$diversity,na.rm=T)
      
      ## crop specific detrended yield and production
      for (j in unique(dfProductionLevel$Item))
      {
        dfProductionLevel[which(dfProductionLevel$Item==j),"YieldDet"] = resid(lm(Yield ~ Year^2,data=dfProductionLevel[which(dfProductionLevel$Item==j),]))
        dfProductionLevel[which(dfProductionLevel$Item==j),"ProductionDet"] = resid(lm(Production ~ Year^2,data=dfProductionLevel[which(dfProductionLevel$Item==j),]))
      }      
      ## asynchrony cacluatioin
      dfSummary$yieldAsynchrony <- 1-synchrony(dfProductionLevel,time.var="Year",species.var="Item",abundance.var="YieldDet") 
      dfSummary$productionAsynchrony <- 1-synchrony(dfProductionLevel,time.var="Year",species.var="Item",abundance.var="ProductionDet") 
      
      dfSummary$meanCropland <- mean(dfCroplandLevel$croplandArea,na.rm=T)
      dfSummary$nitrogen <- mean(dfFertilizerLevel$Nitrogen,na.rm=T)
      dfSummary$irrigation_share <- mean(dfIrrigationLevel$Irrigation,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfClimateLevel$meanTemp,na.rm=T)/sd(dfClimateLevel$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfClimateLevel$meanPrec,na.rm=T)/sd(dfClimateLevel$meanPrec,na.rm=T))
      dfSummary$warfare <- mean(dfWarfareLevel$warfare,na.rm=T)
      
      na.omit(dfSummary)
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Level","timePeriod")])) == nrow(dfAll) # check duplicates

unique(dfAll$timePeriod)
head(dfAll)

## calculate nitrogen per ha
dfAll$nitrogen_ha <- dfAll$nitrogen/dfAll$meanCropland

## calculate total irrigation
dfAll$irrigation <- dfAll$irrigation_share*dfAll$meanCropland

# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
length(unique(dfAll$Level)) ## 136 countries

## export commodities
cropsFinal <- sort(as.character(unique(dfProduction[which(dfProduction$Level%in%unique(dfAll$Level)),"Item"])))
write.csv(data.frame(a=cropsFinal[1:33],b=cropsFinal[34:66],c=cropsFinal[67:99],d=c(cropsFinal[100:131],NA)),"results/SupplementaryTable2_Feb2020.csv",row.names=F)

## save dataframe
names(dfAll)[1] <- "Country"
dfAll <- dfAll[,c("Country","timePeriod",
                  "yieldStability","productionStability","yield","production",
                  "yieldAsynchrony","productionAsynchrony",
                  "richness","diversity","nitrogen","nitrogen_ha","irrigation","irrigation_share",
                  "instabilityTemp","instabilityPrec","warfare")]
write.csv(dfAll, "datasetsDerived/dataFinal_global_Feb2020.csv",row.names=F)


rm(list=ls())

