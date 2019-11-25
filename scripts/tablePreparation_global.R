library(rgdal)
library(maptools)
library(tidyr)
library(vegan)

################ DATA PREPARATION

#### cropland data
dfCropland <- read.csv("datasets/cropland_global.csv")
sort(as.character(unique(dfCropland$Area)))
head(dfCropland)

# harmonize cropland with spatial data 
ctryMap <- readOGR("spatial/countries_global.shp")
plot(ctryMap)
sort(as.character(setdiff(dfCropland$Area,ctryMap@data$Area)))

levels(dfCropland$Area) <- c(levels(dfCropland$Area),"Bolivia","Cote d'Ivoire","Czech Republic","Korea, Democratic People's Republic of",
                                                      "Swaziland","Ethiopia","Burma","Netherlands Antilles",
                                                      "Korea, Republic of","Reunion","Russia","Saint Helena","United States","Venezuela")

dfCropland[which(dfCropland$Area=="Bolivia (Plurinational State of)"),"Area"] <- "Bolivia"
dfCropland[which(dfCropland$Area=="CÃ´te d'Ivoire"|dfCropland$Area=="Côte d'Ivoire"|dfCropland$Area=="C?te d'Ivoire"),"Area"] <- "Cote d'Ivoire"
dfCropland[which(dfCropland$Area=="Czechia"),"Area"] <- "Czech Republic"
dfCropland[which(dfCropland$Area=="Democratic People's Republic of Korea"),"Area"] <- "Korea, Democratic People's Republic of"
dfCropland[which(dfCropland$Area=="Eswatini"),"Area"] <- "Swaziland"
dfCropland[which(dfCropland$Area=="Ethiopia PDR"),"Area"] <- "Ethiopia"
dfCropland[which(dfCropland$Area=="Myanmar"),"Area"] <- "Burma"
dfCropland[which(dfCropland$Area=="Netherlands Antilles (former)"),"Area"] <- "Netherlands Antilles"
dfCropland[which(dfCropland$Area=="Republic of Korea"),"Area"] <- "Korea, Republic of"
dfCropland[which(dfCropland$Area=="R?union"|dfCropland$Area=="Réunion"|dfCropland$Area=="RÃ©union"),"Area"] <- "Reunion"
dfCropland[which(dfCropland$Area=="Russian Federation"),"Area"] <- "Russia"
dfCropland[which(dfCropland$Area=="Saint Helena, Ascension and Tristan da Cunha"),"Area"] <- "Saint Helena"
dfCropland[which(dfCropland$Area=="United States of America"),"Area"] <- "United States"
dfCropland[which(dfCropland$Area=="Venezuela (Bolivarian Republic of)"),"Area"] <- "Venezuela"

sort(as.character(setdiff(dfCropland$Area,ctryMap@data$Area))) 

dfCropland <- dfCropland[which(dfCropland$Area%in%ctryMap@data$Area),] # only keep current countries
dfCropland <- dfCropland[,c("Area","Year","Value")]
dfCropland$croplandArea <- dfCropland$Value*1000 # convert to ha
dfCropland <- dfCropland[,c("Area","Year","croplandArea")]
nrow(unique(dfCropland[,c("Area","Year")])) == nrow(dfCropland) # check duplicates
## this are the target regions



#### agricultural production data

## read agricultural production file
dfProductionFull <- read.csv("datasets/agriculturalProduction_global.csv")
names(dfProductionFull)

## production data
names(dfProductionFull)
dfProductionFull <- dfProductionFull[which(dfProductionFull$Element=="Area harvested" | dfProductionFull$Element == "Production"),c(2,4,6,15:64)]
head(dfProductionFull)
names(dfProductionFull)[4:53] <- 1968:2017 # change colnames to years

## change dataset structure
dfProductionFullr <- dfProductionFull %>% gather(Year, Value, "1968":"2017")
head(dfProductionFullr)
dfProductionFullr$Year <- as.numeric(dfProductionFullr$Year)
dfProductionFullr <- dfProductionFullr %>% spread(Element, Value)
head(dfProductionFullr)
names(dfProductionFullr)[4] <- "AreaHarvested"


#### add calories and make crops consistent with target crop file
dfConsistency <- read.csv("datasets/targetCrops_global.csv")
head(dfConsistency) # Group2 is the target crop name

dfConsistency <- dfConsistency[which(!is.na(dfConsistency$Group2)&!is.na(dfConsistency$Calories)),]
setdiff(sort(unique(dfConsistency[which(!is.na(dfConsistency$harvestedAreaFAO)),"harvestedAreaFAO"])),sort(unique(dfProductionFull$Item)))

## subset crops without calories
dfProductionFullr <- merge(dfProductionFullr,dfConsistency[,c("harvestedAreaFAO","Group2","Calories")],by.x="Item",by.y="harvestedAreaFAO")
names(dfProductionFullr)
# change production to calories
dfProductionFullr$Production <- dfProductionFullr$Production*dfProductionFullr$Calories
# keep necessary columns only 
dfProductionFullr <- dfProductionFullr[,c("Area","Group2","Year","AreaHarvested","Production")]


## make area and production consistent
# set 0 areas to NA, set produciton in NA areas to NA
dfProductionFullr[which(dfProductionFullr$AreaHarvested==0),"AreaHarvested"] <- NA
dfProductionFullr[which(is.na(dfProductionFullr$AreaHarvested)),"Production"] <- NA
# set production to 0 where area is reported
dfProductionFullr[which(dfProductionFullr$AreaHarvested> 0 & is.na(dfProductionFullr$Production)),"Production"] <- 0

# subset NA
dfProductionFullr <- na.omit(dfProductionFullr)

## harmonize country names
sort(as.character(setdiff(dfProductionFullr$Area,dfCropland$Area)))

levels(dfProductionFullr$Area) <- c(levels(dfProductionFullr$Area),"Bolivia","Cote d'Ivoire","Czech Republic","Korea, Democratic People's Republic of",
                             "Swaziland","Ethiopia","Burma",
                             "Korea, Republic of","Reunion","Russia","North Macedonia","United States","Venezuela")

dfProductionFullr[which(dfProductionFullr$Area=="Bolivia (Plurinational State of)"),"Area"] <- "Bolivia"
dfProductionFullr[which(dfProductionFullr$Area=="CÃ´te d'Ivoire"|dfProductionFullr$Area=="C?te d'Ivoire"|dfProductionFullr$Area=="Côte d'Ivoire"),"Area"] <- "Cote d'Ivoire"
dfProductionFullr[which(dfProductionFullr$Area=="Czechia"),"Area"] <- "Czech Republic"
dfProductionFullr[which(dfProductionFullr$Area=="Democratic People's Republic of Korea"),"Area"] <- "Korea, Democratic People's Republic of"
dfProductionFullr[which(dfProductionFullr$Area=="Eswatini"),"Area"] <- "Swaziland"
dfProductionFullr[which(dfProductionFullr$Area=="Ethiopia PDR"),"Area"] <- "Ethiopia"
dfProductionFullr[which(dfProductionFullr$Area=="Myanmar"),"Area"] <- "Burma"
dfProductionFullr[which(dfProductionFullr$Area=="Republic of Korea"),"Area"] <- "Korea, Republic of"
dfProductionFullr[which(dfProductionFullr$Area=="R?union"|dfProductionFullr$Area=="Réunion"|dfProductionFullr$Area=="RÃ©union"),"Area"] <- "Reunion"
dfProductionFullr[which(dfProductionFullr$Area=="Russian Federation"),"Area"] <- "Russia"
dfProductionFullr[which(dfProductionFullr$Area=="The former Yugoslav Republic of Macedonia"),"Area"] <- "North Macedonia"
dfProductionFullr[which(dfProductionFullr$Area=="United States of America"),"Area"] <- "United States"
dfProductionFullr[which(dfProductionFullr$Area=="Venezuela (Bolivarian Republic of)"),"Area"] <- "Venezuela"
sort(as.character(setdiff(dfProductionFullr$Area,dfCropland$Area)))

nrow(unique(dfProductionFullr[,c("Area","Year","Group2")])) == nrow(dfProductionFullr) # check duplicates
unique(dfProductionFullr[which(duplicated(dfProductionFullr[,c("Area","Year","Group2")])),"Area"])


## reduce dataset: only include crops with at least 15 entries per region, remove 0 at the beginning and end of time series!; maximize  number of crops and years per time window
# iterate through countries
vecCountry <- unique(dfProductionFullr$Area)
lsProductionFull <- lapply(vecCountry,function(ctry){
  show(as.character(ctry))
  ## subset country
  dfCountry <- dfProductionFullr[which(dfProductionFullr$Area==ctry),]

  ## iterate through crops
  lsCrops <- lapply(unique(dfCountry$Group2),function(c){
    dfCrop <- merge(data.frame(Year=1968:2017,Area=ctry,Group2=c),dfCountry[which(dfCountry$Group2==c),],all.x=T) # crate full data frame
    # only consider crops with at least 15 data points for detrending
    if (sum(!is.na(dfCrop$Production))>=15&sum(dfCrop$Production>0,na.rm=T)>0){
      minYear <- min(dfCrop[which(dfCrop$Production>0),"Year"])
      maxYear <- max(dfCrop[which(dfCrop$Production>0),"Year"])
      dfCrop[which(dfCrop$Year<minYear),c("AreaHarvested","Production")] <- NA
      dfCrop[which(dfCrop$Year>maxYear),c("AreaHarvested","Production")] <- NA
      dfCrop[which(dfCrop$Year>minYear&is.na(dfCrop$Production)&dfCrop$time<maxYear),"Production"] <- 0
      # detrend crop-specific production
      if (sum(!is.na(dfCrop$Production))>=15){
        dfCrop$ProductionDet <- NA
        dfCrop[which(!is.na(dfCrop$Production)),"ProductionDet"] <-resid(loess(Production ~ Year,data= dfCrop))
        dfCrop
      }
    }
  })
  dfCrops <- do.call(rbind,lsCrops)
  if (!is.null(dfCrops))
  {
    lsWindow <- lapply(seq(1968,2008,10),function(yearStart){
    ## keep maximum number of crops if at least 5 years are covered
    # moving window 
    lsWidth <- lapply(5:10,function(w){ # w = width of time window (>=5)
      dfCropsR <- dfCrops[which(dfCrops$Year>=yearStart&dfCrops$Year<=(yearStart+9)),c("Year","Group2","Production")] %>% spread(Year,Production)
      by <- 10-(w-1)
      lsPos <- lapply(1:by,function(p){ # iterate through all possible windows by given width
        noCrop <- sum(apply(dfCropsR[,(1+p):(1+p+w-1)],1,function(r){sum(!is.na(r))==w}))
        data.frame(width = w, year = (yearStart+p-1), num = noCrop)
      })
      do.call(rbind,lsPos)
    })
    dfFreq <- do.call(rbind,lsWidth)
    # get time window with maximum number of crops, then largest time period and minimum starting year
    maxN <- max(dfFreq$num)
    maxW <- max(dfFreq[which(dfFreq$num==maxN),"width"])
    minYear <- min(dfFreq[which(dfFreq$num==maxN&dfFreq$width==maxW),"year"])
    maxYear <- minYear+maxW-1

    # remove crops with NA in focal time frame 
    dfFocal <- dfCrops[which(dfCrops$Year>=minYear&dfCrops$Year<=maxYear),]
    unfocalCrops <- unique(dfFocal[which(is.na(dfFocal$Production)),"Group2"])
    dfCropsTotal <- dfFocal[which(!dfFocal$Group2%in%unfocalCrops&!is.na(dfFocal$Production)),]
    dfCropsTotal
    })
    do.call(rbind,lsWindow)
  }
}) ## infinite if time period is not covered -> will be ignored
dfProductionFull_Final <- do.call(rbind,lsProductionFull)
head(dfProductionFull_Final)
sum(is.na(dfProductionFull_Final$AreaHarvested))
sum(is.na(dfProductionFull_Final$Production))

# check number of unique crops
length(unique(dfProductionFull_Final$Group2)) ## 129 crops

#### calculate total production per country and year
dfProductionCaloriesFinal <- aggregate(Production~Area+Year,dfProductionFull_Final,sum)
head(dfProductionCaloriesFinal)
nrow(dfProductionCaloriesFinal)==nrow(unique(dfProductionFull_Final[,c("Area","Year")]))

#### calculate total area per country and year
dfHarvestedAreaFinal <- aggregate(AreaHarvested~Area+Year,dfProductionFull_Final,sum)
head(dfHarvestedAreaFinal)
nrow(dfHarvestedAreaFinal)==nrow(dfProductionCaloriesFinal)

#### calculate yields
dfYieldCalories <- merge(dfProductionCaloriesFinal,dfHarvestedAreaFinal,by=c("Area","Year"))
head(dfYieldCalories)
dfYieldCalories$Yield <- dfYieldCalories$Production/dfYieldCalories$AreaHarvested
dfYieldCalories <- dfYieldCalories[,c("Area","Year","Yield")]
nrow(unique(dfYieldCalories[,c("Area","Year")])) == nrow(dfYieldCalories) # check duplicates

#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AreaHarvested~Area+Year,dfProductionFull_Final,function(x){exp(diversity(x,index="shannon"))})
head(dfShannon)
names(dfShannon)[3] <- "diversity"
nrow(dfShannon)==nrow(dfProductionCaloriesFinal)
nrow(unique(dfShannon[,c("Area","Year")])) == nrow(dfShannon) # check duplicates


################### Other Predictors

#### Fertilizer
dfFertilizerArchive <- read.csv("datasets/fertilizerArchive_global.csv")
names(dfFertilizerArchive)[4] <- "Area"
dfFertilizerNew <- read.csv("datasets/fertilizerNew_global.csv")
dfFertilizer <- rbind(dfFertilizerArchive[,c("Area","Year","Item","Value")],dfFertilizerNew[,c("Area","Year","Item","Value")])
dfFertilizer <- dfFertilizer[which(dfFertilizer$Item=="Nitrogenous fertilizers"| dfFertilizer$Item=="Nutrient nitrogen N (total)"),]
dfFertilizer <- dfFertilizer[,c("Area","Year","Value")]
names(dfFertilizer)[3] <- "Nitrogen"
head(dfFertilizer)


# harmonzie country names
sort(as.character(setdiff(dfFertilizer$Area,dfCropland$Area)))

levels(dfFertilizer$Area) <- c(levels(dfFertilizer$Area),"Bolivia","Cote d'Ivoire","Czech Republic","Korea, Democratic People's Republic of",
                             "Swaziland","Ethiopia","Burma","Netherlands Antilles",
                             "Reunion","Korea, Republic of","Russia","North Macedonia","United States","Venezuela")

dfFertilizer[which(dfFertilizer$Area=="Bolivia (Plurinational State of)"),"Area"] <- "Bolivia"
dfFertilizer[which(dfFertilizer$Area=="CÃ´te d'Ivoire"|dfFertilizer$Area=="C?te d'Ivoire"|dfFertilizer$Area=="Côte d'Ivoire"),"Area"] <- "Cote d'Ivoire"
dfFertilizer[which(dfFertilizer$Area=="Czechia"),"Area"] <- "Czech Republic"
dfFertilizer[which(dfFertilizer$Area=="Democratic People's Republic of Korea"),"Area"] <- "Korea, Democratic People's Republic of"
dfFertilizer[which(dfFertilizer$Area=="Eswatini"),"Area"] <- "Swaziland"
dfFertilizer[which(dfFertilizer$Area=="Ethiopia PDR"),"Area"] <- "Ethiopia"
dfFertilizer[which(dfFertilizer$Area=="Myanmar"),"Area"] <- "Burma"
dfFertilizer[which(dfFertilizer$Area=="Netherlands Antilles (former)"),"Area"] <- "Netherlands Antilles"
dfFertilizer[which(dfFertilizer$Area=="R?union"|dfFertilizer$Area=="Réunion"|dfFertilizer$Area=="RÃ©union"),"Area"] <- "Reunion"
dfFertilizer[which(dfFertilizer$Area=="Republic of Korea"),"Area"] <- "Korea, Republic of"
dfFertilizer[which(dfFertilizer$Area=="Russian Federation"),"Area"] <- "Russia"
dfFertilizer[which(dfFertilizer$Area=="The former Yugoslav Republic of Macedonia"),"Area"] <- "North Macedonia"
dfFertilizer[which(dfFertilizer$Area=="United States of America"),"Area"] <- "United States"
dfFertilizer[which(dfFertilizer$Area=="Venezuela (Bolivarian Republic of)"),"Area"] <- "Venezuela"

sort(as.character(setdiff(dfFertilizer$Area,dfCropland$Area)))

nrow(unique(dfFertilizer[,c("Area","Year")])) == nrow(dfFertilizer) # check duplicates


## Irrigation
dfIrrigation <- read.csv("datasets/irrigationEquippedArea_global.csv")

# harmonzie country names
sort(as.character(setdiff(dfIrrigation$Area,dfCropland$Area)))
levels(dfIrrigation$Area) <- c(levels(dfIrrigation$Area),"Bolivia","Cote d'Ivoire","Czech Republic","Korea, Democratic People's Republic of",
                               "Swaziland","Ethiopia","Burma","Reunion","Korea, Republic of","Russia","United States","Venezuela")

dfIrrigation[which(dfIrrigation$Area=="Bolivia (Plurinational State of)"),"Area"] <- "Bolivia"
dfIrrigation[which(dfIrrigation$Area=="CÃ´te d'Ivoire"|dfIrrigation$Area=="C?te d'Ivoire"|dfIrrigation$Area=="Côte d'Ivoire"),"Area"] <- "Cote d'Ivoire"
dfIrrigation[which(dfIrrigation$Area=="Czechia"),"Area"] <- "Czech Republic"
dfIrrigation[which(dfIrrigation$Area=="Democratic People's Republic of Korea"),"Area"] <- "Korea, Democratic People's Republic of"
dfIrrigation[which(dfIrrigation$Area=="Eswatini"),"Area"] <- "Swaziland"
dfIrrigation[which(dfIrrigation$Area=="Ethiopia PDR"),"Area"] <- "Ethiopia"
dfIrrigation[which(dfIrrigation$Area=="Myanmar"),"Area"] <- "Burma"
dfIrrigation[which(dfIrrigation$Area=="R?union"|dfIrrigation$Area=="Réunion"|dfIrrigation$Area=="RÃ©union"),"Area"] <- "Reunion"
dfIrrigation[which(dfIrrigation$Area=="Republic of Korea"),"Area"] <- "Korea, Republic of"
dfIrrigation[which(dfIrrigation$Area=="Russian Federation"),"Area"] <- "Russia"
dfIrrigation[which(dfIrrigation$Area=="United States of America"),"Area"] <- "United States"
dfIrrigation[which(dfIrrigation$Area=="Venezuela (Bolivarian Republic of)"),"Area"] <- "Venezuela"
sort(as.character(setdiff(dfIrrigation$Area,dfCropland$Area)))

dfIrrigation <- dfIrrigation[,c("Area","Year","Value")]
names(dfIrrigation)[3] <- "Irrigation%"
nrow(unique(dfIrrigation[,c("Area","Year")])) == nrow(dfIrrigation) # check duplicates


###### Warfare
dfWarfare <- read.csv("datasets/warfare_global.csv")
names(dfWarfare)[3] <- "Area"

dfWarfare <- na.omit(dfWarfare) # remove NA
# add former divided Germany: always 0
dfWarfare[which(dfWarfare$Area=="Germany")[1],]
dfG <- data.frame(SCODE="GMY",CCODE = 255, Area="Germany",YEAR=1961:1990,ACTOTAL=0)
dfWarfare <- rbind(dfWarfare,dfG)

# combine north and south yemen
dfWarfare[which(dfWarfare$Area=="Yemen")[1],]
dfYemenN <- dfWarfare[which(dfWarfare$Area=="Yemen, North"),]
dfYemenN$SCODE <- "YEM"
dfYemenN$CCODE <- 679
dfYemenN$Area <- "Yemen"
dfYemenS <- dfWarfare[which(dfWarfare$Area=="Yemen, South"),]
dfYemenS$SCODE <- "YEM"
dfYemenS$CCODE <- 679
dfYemenS$Area <- "Yemen"
dfYemenT <- merge(dfYemenN,dfYemenS,by=c("SCODE","CCODE","Area","YEAR"),all=T)
head(dfYemenT)
dfYemenT$ACTOTAL <- rowSums(dfYemenT[,c("ACTOTAL.x","ACTOTAL.y")],na.rm=T)
dfWarfare <- rbind(dfWarfare,dfYemenT[,c(1:4,7)])

# harmonize country names
sort(as.character(setdiff(dfWarfare$Area,dfCropland$Area)))

levels(dfWarfare$Area) <- c(levels(dfWarfare$Area),"Iran (Islamic Republic of)","Korea, Democratic People's Republic of","Korea, Republic of",
                            "Lao People's Democratic Republic","North Macedonia","Republic of Moldova","Burma","Russia","Syrian Arab Republic",
                            "United Republic of Tanzania","Viet Nam")

dfWarfare[which(dfWarfare$Area=="Iran"),"Area"] <-  "Iran (Islamic Republic of)"
dfWarfare[which(dfWarfare$Area=="Korea North"),"Area"] <- "Korea, Democratic People's Republic of"
dfWarfare[which(dfWarfare$Area=="Korea South"),"Area"] <- "Korea, Republic of"
dfWarfare[which(dfWarfare$Area=="Laos"),"Area"] <-  "Lao People's Democratic Republic"
dfWarfare[which(dfWarfare$Area=="Macedonia"),"Area"] <-  "North Macedonia"
dfWarfare[which(dfWarfare$Area=="Moldova"),"Area"] <- "Republic of Moldova"
dfWarfare[which(dfWarfare$Area=="Myanmar (Burma)"),"Area"] <-  "Burma"
dfWarfare[which(dfWarfare$Area=="RUSRia"),"Area"] <- "Russia"
dfWarfare[which(dfWarfare$Area=="Syria"),"Area"] <- "Syrian Arab Republic"
dfWarfare[which(dfWarfare$Area=="Tanzania"),"Area"] <-"United Republic of Tanzania"
dfWarfare[which(dfWarfare$Area=="Vietnam"),"Area"] <- "Viet Nam"
sort(as.character(setdiff(dfWarfare$Area,dfCropland$Area)))
names(dfWarfare)[4:5] <- c("Year","warfare")
dfWarfare <- dfWarfare[,c("Area","Year","warfare")]
hist(dfWarfare$warfare)


###### climate
# intersect file (intersection of country borders and climate pixels)
shpClimateID <- readOGR("spatial","countriesClimateID_global")
head(shpClimateID@data)
dfClimateID <- shpClimateID@data
dfClimateIDsum <- aggregate(areaHA~cellID,dfClimateID,function(i){sum(i,na.rm=T)})
names(dfClimateIDsum)[2] <- "areaTot"
dfClimateID <- merge(dfClimateID,dfClimateIDsum,by="cellID")
head(dfClimateID)

# climate file
load("datasetsDerived/climate_global.RData")
names(dfClimateFinalPrint)
head(dfClimateFinalPrint)
dfClimateFinal <- dfClimateFinalPrint[,c(1,16:114)]

# merge
dfClimateFinalArea <- merge(dfClimateID,dfClimateFinal,by="cellID")
head(dfClimateFinalArea)
length(unique(dfClimateFinalArea$cellID)) / nrow(dfClimateFinalArea)
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
dfClimateFinalArea[,87:100] <- dfClimateFinalArea[,87:100]*dfClimateFinalArea$weight5

# sum weighted values to get overall weighted average
dfClimateFinalAreaAgg <- aggregate(dfClimateFinalArea[,7:100],by=list(dfClimateFinalArea$Area),FUN=function(i){sum(i,na.rm=T)})
head(dfClimateFinalAreaAgg)
names(dfClimateFinalAreaAgg)[1] <- "Area"
min(dfClimateFinalAreaAgg[2:95]) # check for negative values -> would be problematic for instability calculation

# change structure
names(dfClimateFinalAreaAgg)
dfClimateFinalr <- dfClimateFinalAreaAgg %>% gather(climYear, Value, names(dfClimateFinalAreaAgg)[2:95])
head(dfClimateFinalr)
dfClimateFinalr$Year <- as.numeric(substr(dfClimateFinalr$climYear,9,12))
dfClimateFinalr$Element <- substr(dfClimateFinalr$climYear,1,8)
dfClimateFinalr <- dfClimateFinalr[,c("Area","Year","Element","Value")]
dfClimateFinalr <- dfClimateFinalr %>% spread(Element, Value)
head(dfClimateFinalr)
nrow(dfClimateFinalr)
nrow(unique(dfClimateFinalr[,c("Area","Year")])) == nrow(dfClimateFinalr) # check duplicates


#### calculate all variables for the 5 time periods
vecCountryFinal <- Reduce(intersect,list(dfYieldCalories$Area,dfProductionFull_Final$Area,dfShannon$Area,dfCropland$Area,dfFertilizer$Area,dfIrrigation$Area,dfClimateFinalr$Area,dfWarfare$Area))
lsAll <- lapply(vecCountryFinal,function(ctry){
  # detrend yields
  show(as.character(ctry))
  dfYieldCtry <- dfYieldCalories[which(dfYieldCalories$Area==ctry),]
  dfYieldCtry$YieldDet <- resid(loess(Yield ~ Year,data=dfYieldCtry))
  
  # subset data for the target country
  dfProductionCtry <- dfProductionFull_Final[which(dfProductionFull_Final$Area==ctry),c("Area","Group2","Year","ProductionDet")]
  dfShannonCtry <- dfShannon[which(dfShannon$Area==ctry),]
  dfCroplandCtry <- dfCropland[which(dfCropland$Area==ctry),]
  dfFertilizerCtry <- dfFertilizer[which(dfFertilizer$Area==ctry),]
  dfIrrigationCtry <- dfIrrigation[which(dfIrrigation$Area==ctry),]
  dfClimateCtry <- dfClimateFinalr[which(dfClimateFinalr$Area==ctry),]

  lsAggregate <- lapply(c(1968,1978,1988,1998,2008),function(yearStart){
    # print(yearStart)
    dfSummary <- data.frame(Area=ctry, timePeriod= yearStart)
    dfSummary$stability <- mean(dfYieldCtry[which(dfYieldCtry$Year>=yearStart&dfYieldCtry$Year<=(yearStart+9)),"Yield"],na.rm=T)/sd(dfYieldCtry[which(dfYieldCtry$Year>=yearStart&dfYieldCtry$Year<=(yearStart+9)),"YieldDet"],na.rm=T)
    dfSummary$diversity <- mean(dfShannonCtry[which(dfShannonCtry$Year>=yearStart&dfShannonCtry$Year<=(yearStart+9)),"diversity"],na.rm=T)
    
    # asynchrony (Code snippet from Mehrabi & Ramankutty 2019, supplement p. 29)
    dfProductionCtryTime <- dfProductionCtry[which(dfProductionCtry$Year>=yearStart&dfProductionCtry$Year<=(yearStart+9)),]
    noCrop <- length(unique(dfProductionCtryTime$Group2))
    dfSummary$asynchrony <- NA
    if (noCrop == 1) {  dfSummary$asynchrony <- 0}
    if (noCrop > 1) {
      dfProductionCtryTimer <- dfProductionCtryTime %>% spread(Group2, ProductionDet)
      det.w.dec <- dfProductionCtryTimer[,3:ncol(dfProductionCtryTimer)]
      diff.cov <- cov(data.frame(det.w.dec))
      diag.cov <- diag(diff.cov)
      sqrt.diag.cov <- sqrt(diag.cov)
      diff.var.glob <-  sum(diff.cov,na.rm=T)
      diff.sd.local <- sum(sqrt.diag.cov,na.rm=T)
      diff.async <- 1-diff.var.glob/(diff.sd.local^2) #asynchrony
      dfSummary$asynchrony <- diff.async
    }
    dfSummary$meanCropland <- mean(dfCroplandCtry[which(dfCroplandCtry$Year>=yearStart&dfCroplandCtry$Year<=(yearStart+9)),"croplandArea"],na.rm=T)
    dfSummary$meanNitrogen <- mean(dfFertilizerCtry[which(dfFertilizerCtry$Year>=yearStart&dfFertilizerCtry$Year<=(yearStart+9)),"Nitrogen"],na.rm=T)
    dfSummary$meanIrrigation_share <- mean(dfIrrigationCtry[which(dfIrrigationCtry$Year>=yearStart&dfIrrigationCtry$Year<=(yearStart+9)),"Irrigation%"],na.rm=T)
    dfSummary$instabilityTemp <- -(mean(dfClimateCtry[which(dfClimateCtry$Year>=yearStart&dfClimateCtry$Year<=(yearStart+9)),"meanTemp"],na.rm=T)/sd(dfClimateCtry[which(dfClimateCtry$Year>=yearStart&dfClimateCtry$Year<=(yearStart+9)),"meanTemp"],na.rm=T))
    dfSummary$instabilityPrec <- -(mean(dfClimateCtry[which(dfClimateCtry$Year>=yearStart&dfClimateCtry$Year<=(yearStart+9)),"meanPrec"],na.rm=T)/sd(dfClimateCtry[which(dfClimateCtry$Year>=yearStart&dfClimateCtry$Year<=(yearStart+9)),"meanPrec"],na.rm=T))
    dfSummary$meanWarfare <- mean(dfWarfare[which(dfWarfare$Year>=yearStart&dfWarfare$Year<=(yearStart+9)),"warfare"],na.rm=T)
    
    na.omit(dfSummary)
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Area","timePeriod")])) == nrow(dfAll) # check duplicates

unique(dfAll$timePeriod)
head(dfAll)

## calculate nitrogen per ha
dfAll$meanNitrogen_t_ha <- dfAll$meanNitrogen/dfAll$meanCropland
names(dfAll)
dfAll <- dfAll[,c("Area","timePeriod","stability","diversity","asynchrony","meanNitrogen_t_ha","meanIrrigation_share","meanWarfare","instabilityTemp","instabilityPrec")]
names(dfAll)  

# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
hist(dfAll$stability)
length(unique(dfAll$Area)) ## 149 countries

 
 
## add income groups
dfCountryCodes <- read.csv("datasets/incomeGroups_global.csv")
names(dfCountryCodes)
unique(dfCountryCodes$Income.group)

# harmonize country names
missing <- unique(dfAll[-which(dfAll$Area%in%dfCountryCodes$Economy),"Area"])
levels(dfCountryCodes$Economy) <- c(levels(dfCountryCodes$Economy),as.character(missing))
sort(as.character(dfCountryCodes$Economy))
dfCountryCodes[which(dfCountryCodes$Economy=="CÃ´te d'Ivoire"|dfCountryCodes$Economy=="C?te d'Ivoire"|dfCountryCodes$Economy=="Côte d'Ivoire"),"Economy"] <- "Cote d'Ivoire"
dfCountryCodes[which(dfCountryCodes$Economy=="Egypt, Arab Rep."),"Economy"] <- "Egypt"
dfCountryCodes[which(dfCountryCodes$Economy=="Gambia, The"),"Economy"] <- "Gambia"
dfCountryCodes[which(dfCountryCodes$Economy=="Iran, Islamic Rep."),"Economy"] <- "Iran (Islamic Republic of)"
dfCountryCodes[which(dfCountryCodes$Economy=="Korea, Dem. People's Rep."),"Economy"] <- "Korea, Democratic People's Republic of"
dfCountryCodes[which(dfCountryCodes$Economy=="Korea, Rep."),"Economy"] <- "Korea, Republic of"
dfCountryCodes[which(dfCountryCodes$Economy=="Kyrgyz Republic"),"Economy"] <- "Kyrgyzstan"
dfCountryCodes[which(dfCountryCodes$Economy=="Lao PDR"),"Economy"] <- "Lao People's Democratic Republic"
dfCountryCodes[which(dfCountryCodes$Economy=="Myanmar"),"Economy"] <- "Burma"
dfCountryCodes[which(dfCountryCodes$Economy=="Moldova"),"Economy"] <- "Republic of Moldova"
dfCountryCodes[which(dfCountryCodes$Economy=="Russian Federation"),"Economy"] <- "Russia"
dfCountryCodes[which(dfCountryCodes$Economy=="Slovak Republic"),"Economy"] <- "Slovakia"
dfCountryCodes[which(dfCountryCodes$Economy=="Eswatini"),"Economy"] <- "Swaziland"
dfCountryCodes[which(dfCountryCodes$Economy=="Tanzania"),"Economy"] <- "United Republic of Tanzania"
dfCountryCodes[which(dfCountryCodes$Economy=="Venezuela, RB"),"Economy"] <- "Venezuela"
dfCountryCodes[which(dfCountryCodes$Economy=="Vietnam"),"Economy"] <- "Viet Nam"
dfCountryCodes[which(dfCountryCodes$Economy=="Yemen, Rep."),"Economy"] <- "Yemen"
unique(dfAll[-which(dfAll$Area%in%dfCountryCodes$Economy),"Area"])
dfAll <- merge(dfAll,dfCountryCodes, by.x="Area",by.y="Economy")
names(dfAll)
dfAll <- dfAll[,c(1,12:13,2:10)]
names(dfAll)[3] <- "IncomeGroup"


## save dataframe
write.csv(dfAll, "datasetsDerived/dataFinal_global.csv",row.names=F)




rm(list=ls())
