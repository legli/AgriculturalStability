library(ncdf4)
library(raster)
library(rgdal)
library(countrycode)
library(tidyr)


## Coordinates
dfID <- data.frame(x=rep(seq(-179.75,179.75,0.5),360),y=as.numeric(sapply(seq(89.75,-89.75,-0.5),function(i){rep(i,720)})))
dfID$cellID <- 1:nrow(dfID)

## Shapes
# national
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso3c')


#### create spatial mask from cropland layer
rasterCropland <- raster::aggregate(raster("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/cropland/cropland2000AD.asc"),fact=6,fun=sum,na.rm=T)
dfCropland <- as.data.frame(rasterCropland)
dfCropland <- cbind(dfID,dfCropland)
head(dfCropland)
names(dfCropland)[4] <- "cropland"
# only keep cells with at least 1km2 cropland
dfCropland <- dfCropland[which(dfCropland$cropland>1),]
dfCroplandRaw <- dfCropland # keep raw values for weighting
dfCropland$cropland <- 1 # mask


#### create temporal mask from growing calendar

wd <- "C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/ALL_CROPS_ArcINFO_0.5deg_filled"
strFolders <- dir(wd)

lsCrops <- lapply(strFolders, function(folder){
  crop <- strsplit(folder,".",fixed=T)[[1]][1]
  print(crop)
  plantAgg=as.data.frame(raster(paste0(wd,"/",folder,"/plant.start.asc")))  # aggregate to resolution of climate data
  totAgg=as.data.frame(raster(paste0(wd,"/",folder,"/tot.days.asc"))) # aggregate to resolution of climate data
  dfGrowing <- cbind(plantAgg,totAgg)
  names(dfGrowing) <- paste0(names(dfGrowing),crop)
  dfGrowing
})
dfCrops <- do.call(cbind,lsCrops)
dfCrops <- cbind(dfID[,c("x","y")],dfCrops)
head(dfCrops)
names(dfCrops)

# get months for which any crop can be grown
vecYear1 <- c(31,59,90,120,151,181,212,243,273,304,334,365)
vecYear2 <- c(365,396,424,455,485,516,546,577,608,638,669,699)
m <- 0 
repeat{
  m <- m+1
  print(m)
  dfCrops[,paste0("month",m)] <- apply(dfCrops,1,function(r){
    res <- NA
    if (sum(r[seq(3,52,2)]<=vecYear1[m],na.rm=T)>0|sum((r[seq(3,52,2)]+r[seq(4,52,2)])>vecYear2[m],na.rm=T)>0)
    {res <- 1}
    res
  })
  if (m==12)
    break
}
sapply(53:64,function(i){table(dfCrops[,i])}) 

names(dfCrops)
dfCrops <- dfCrops[,c(1:2,53:64)]


#### extract climate data by spatial and temporal mask
## oben ncdf-files
ncTempAbs <- nc_open("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/climate/air.mon.mean.v501.nc")
ncPrecAbs <- nc_open("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/climate/precip.mon.total.v501.nc")

## create dataframes with correct coordinate (start with x = 0.25 to correct for origin)
dfClimateXY <- data.frame(x=rep(c(seq(0.25,179.75,0.5),seq(-179.75,-0.25,0.5)),360),y=as.numeric(sapply(seq(89.75,-89.75,-0.5),function(i){rep(i,720)})))


lsClimate <- lapply(61:117,function(y){
  print(y)
  dfTemp <- dfClimateXY
  dfPrec <- dfClimateXY
  
  for(m in 1:12){
    temp <- ncvar_get(ncTempAbs, attributes(ncTempAbs$var)$names[1],start=c(1,1,(y*12+m)),count=c(720,360,1))
    dfTemp[,paste0("m",m)] <- as.numeric(temp)
    prec <- ncvar_get(ncPrecAbs, attributes(ncPrecAbs$var)$names[1],start=c(1,1,(y*12+m)),count=c(720,360,1))
    dfPrec[,paste0("m",m)] <- as.numeric(prec)
  }
  dfTemp <- merge(dfTemp,dfCropland,by=c("x","y"))
  dfTemp <- merge(dfTemp,dfCrops,by=c("x","y"))
  dfTemp[,3:14] <- dfTemp[,3:14]*dfTemp$cropland*dfTemp[,17:28]
  dfTemp[,paste0("temp",1900+y)] <- rowMeans(dfTemp[,3:14],na.rm=T)
  
  dfPrec <- merge(dfPrec,dfCropland,by=c("x","y"))
  dfPrec <- merge(dfPrec,dfCrops,by=c("x","y"))
  dfPrec[,3:14] <- dfPrec[,3:14]*dfPrec$cropland*dfPrec[,17:28]
  dfPrec[,paste0("prec",1900+y)] <- rowMeans(dfPrec[,3:14],na.rm=T)
  
  merge(dfTemp[,c("cellID",paste0("temp",1900+y))],  dfPrec[,c("cellID",paste0("prec",1900+y))],by="cellID")
})
dfClimate <- Reduce(merge, lsClimate)
names(dfClimate)

# add absolute cropland area 
dfClimate <- merge(dfClimate,dfCroplandRaw[,c("cellID","cropland")])


#### create intersection of cellID and level of organization
## add cellID
spg <- dfID
head(spg)
coordinates(spg) <- ~ x + y  
gridded(spg) <- TRUE
rasterDF <- raster(spg)
crs(rasterDF) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
plot(rasterDF)
## convert to polygon
polygonID <- rasterToPolygons(rasterDF)


## intersect grid cells and levels of organization
# national
intersectCountryID <- intersect(mapCountry,polygonID)
intersectCountryID$area_sqkm <- area(intersectCountryID) / 1000000
writeOGR(intersectCountryID, dsn = 'spatial', layer = 'countriesClimateID', driver = "ESRI Shapefile",overwrite_layer = T)
# intersectCountryID <- readOGR("spatial/countriesClimateID.shp")


functionWeigthedClimate <- function(dfIntersect,lev){

  # add total area per cellID
  dfClimateID <- dfIntersect
  
  names(dfClimateID)[which(names(dfClimateID)==lev)] <- "level"
  
  # get total area per cell
  dfClimateIDsum <- aggregate(area_sqkm~cellID,dfClimateID,function(i){sum(i,na.rm=T)})
  names(dfClimateIDsum)[2] <- "areaTot"
  dfClimateID <- merge(dfClimateID,dfClimateIDsum,by="cellID")
  head(dfClimateID)
  dfClimateID$propArea <- dfClimateID$area_sqkm/dfClimateID$areaTot

  ## add climate data
  dfClimateIDFinal <- merge(dfClimateID,dfClimate,by="cellID")

  ## get cropland per segment (weigthed by area proportion)
  dfClimateIDFinal$croplandSegment <- dfClimateIDFinal$propArea*dfClimateIDFinal$cropland
  
  ## get total cropland by level of organization
  dfCroplandTot <- aggregate(croplandSegment~level,dfClimateIDFinal,sum)
  names(dfCroplandTot)[2] <- "croplandSegmentTot"
  dfClimateIDFinal <- merge(dfClimateIDFinal,dfCroplandTot,by="level")
  
  ## weight each cell by proportion of cropland
  dfClimateIDFinal$weight <- dfClimateIDFinal$croplandSegment/dfClimateIDFinal$croplandSegmentTot
  
  
  ## weighted average: get area of cropland attributed to a cell segment (i.e. cropland area multiplied by area share of the segment), divide it by total cropland area across region
  ## calcualte weighted mean and sd for climate variables for each year and unit
  lsClimateFinalAgg <- lapply(unique(dfClimateIDFinal$level),function(l){
    print(l)
    dfClimateCountry <- dfClimateIDFinal[which(dfClimateIDFinal$level==l),]
    lsYears <- lapply(1961:2017,function(y){
      meanT <- weighted.mean(dfClimateCountry[,paste0("temp",y)],dfClimateCountry$weight,na.rm=T)
      sdT <- sum(dfClimateCountry$weight * (dfClimateCountry[,paste0("temp",y)] - meanT)^2,na.rm=T)
      meanP <- weighted.mean(dfClimateCountry[,paste0("prec",y)],dfClimateCountry$weight,na.rm=T)
      sdP <- sum(dfClimateCountry$weight * (dfClimateCountry[,paste0("prec",y)] - meanP)^2,na.rm=T)
      data.frame(Year=y,meanTemp=meanT,sdTemp=sdT,meanPrec=meanP,sdPrec=sdP)
    })
    dfYears <- do.call(rbind,lsYears)
    dfYears$Level <- l
    dfYears[,c("Level","Year","meanTemp","sdTemp","meanPrec","sdPrec")]
  })
  dfClimateFinalAgg <- do.call(rbind,lsClimateFinalAgg)
  na.omit(dfClimateFinalAgg)
}

#### extract masked climate data
# global
dfClimateGlobal <- functionWeigthedClimate(intersectCountryID@data,"Country")
head(dfClimateGlobal)
write.csv(dfClimateGlobal,"datasetsDerived/climate_national.csv",row.names = F)



rm(list=ls())
