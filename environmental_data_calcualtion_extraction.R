#############################################################################
# Climate extraction for Jonathan Cumming Hi-Low Lines
# Author: Jared Streich
# Created Feb 12 2021
# Vesion 0.1.0
# email: ju0@ornl.gov, if not at ornl streich.jared@gmail.com
##############################################################################

##############################################################################
############################## Load libraries ################################
##############################################################################
##### Install SPEI if needed
install.packages("SPEI")

##### Libraries
library(raster)
library(SPEI)


##############################################################################
############################# Read in input data #############################
##############################################################################
##### Set directory to start input files
setwd("~/Desktop/")

##### read in Boardman Hi/Low Lines
# nms <- read.table("Boardman_HiLoLineNames.txt")
# nms <- cbind(nms, nms)
# rownames(nms) <- nms[,1]
# nms[1:10,]


### Alternative coordinates
coords.lat.long <- read.delim("Boardman_HiLow_2022-02-11.txt", header = T)
rownames(coords.lat.long) <- coords.lat.long[,1]

### Bring over names, apparently
rownames(coords.lat.long) <- coords.lat.long[,1]
coords.lat.long[1:10,]
latlong <- coords.lat.long[,2:3]
latlong[1:10,]

##### Read in coordinates data
# latlong <- read.table("p_trichocarpa_1529_LatLong.txt")
# rownames(latlong) <- latlong[,1]
# latlong

##### Prune coordinates to hi-low lines
names.match <- match(rownames(latlong), rownames(nms))
names.combn <- cbind(names.match, latlong)
names.match <- names.combn[complete.cases(names.combn[,1]), ]
dim(names.match)
latlong <- names.match[,3:4]
latlong <- matrix.numeric(latlong)
dim(latlong)

matrix.numeric <- function(x){
  for(i in 1:ncol(x))
    x.i <- as.numeric(as.character(x[,i]))
    if(i == 1){
      x.i.p <- x.i
    }
    else{
      x.i.p <- cbind(x.i.p, x.i)
    }
  rownames(x.i.p) <- rownames(x)
  colnames(x.i.p) <- colnames(x)
  return(x.i.p)
}


##### Final input points for data extraction
dim(latlong)

##############################################################################
####################### Start Processing Input Data ##########################
##############################################################################


##### MAT Mean Annual Temp, aka Bio1
setwd("/Volumes/Smithers/Rasters/2_5m_mean_00s/")
mat <- raster("2_5m_mean_00s_bio01.tif")
mat.latlong <- extract(mat,latlong[,2:1])
mat.latlong <- mat.latlong*0.1
mat.latlong <- cbind(latlong, mat.latlong)


##### M W M T Mean Warmest Month Temp
i <- 1
j <- 1
setwd("/Volumes/Smithers/Rasters/")
files <- c("wc2.0_30s_tmax_06.tif", "wc2.0_30s_tmax_07.tif", "wc2.0_30s_tmax_08.tif")
for(i in 1:nrow(latlong)){
  for(j in 1:length(files)){
    mwmt.rast <- raster(files[j])
    mwmt.rast.j <- extract(mwmt.rast, latlong[,2:1])
    if(j == 1){
      mwmt.rast.p <- mwmt.rast.j
    }
    else{
      mwmt.rast.p <- c(mwmt.rast.p, mwmt.rast.j)
    }
  }
  mwmt.rast.i <- max(mwmt.rast.p)
  if(i == 1){
    mwmt.rast.o <- mwmt.rast.i
  }
  else{
    mwmt.rast.o <- c(mwmt.rast.o, mwmt.rast.i)
  }
}
mwmt.latlong <- cbind(mwmt.rast.o, latlong)


##### MAP Mean Annual Precipitation
setwd("/Volumes/Smithers/Rasters/2_5m_mean_00s/")
bio12 <- raster("2_5m_mean_00s_bio12.tif")
map.latlong <- extract(bio12, latlong[,2:1])
map.latlong <- cbind(latlong, map.latlong)


##### MSP = May to September precipitation (mm), 5 + 6 + 7 + 8 + 9
i <- 1
j <- 1
setwd("/Volumes/Smithers/Rasters/")
files <- c("wc2.0_30s_prec_05.tif", "wc2.0_30s_prec_06.tif", "wc2.0_30s_prec_07.tif", 
           "wc2.0_30s_prec_08.tif","wc2.0_30s_prec_09.tif")
for(i in 1:nrow(latlong)){
  for(j in 1:length(files)){
    msp.rast <- raster(files[j])
    msp.rast.j <- extract(msp.rast, latlong[i,2:1])
    if(j == 1){
      msp.rast.p <- msp.rast.j
    }
    else{
      msp.rast.p <- msp.rast.p + msp.rast.j
    }
  }
  if(i == 1){
    msp.rast.o <- msp.rast.p
  }
  else{
    msp.rast.o <- c(msp.rast.o, msp.rast.p)
  }
}
msp.latlong <- cbind(latlong, msp.rast.o)


##### AHM  annual heat-moisture index (MAT+10)/(MAP/1000))
### MAT = mean annual temperature (°C),
### MAP = mean annual precipitation (mm)
ahm <- cbind(latlong, ((mat.latlong[,3])+10)/((map.latlong[,3])/1000))

##### SHM = summer heat-moisture index ((MWMT)/(MSP/1000))
setwd("~/Downloads/")
shm.latlong <- cbind(latlong, (mwmt.latlong[,3])/(msp.latlong[,3]/1000))


##### CMD = Hargreaves climatic moisture deficit (mm)
### Mean Tmin summer months
setwd("/Volumes/Smithers/Rasters/")
files <- c("wc2.0_30s_tmin_06.tif", "wc2.0_30s_tmin_07.tif", "wc2.0_30s_tmin_08.tif")
i <- 1
j <- 1
for(i in 1:nrow(latlong)){
  for(j in 1:length(files)){
    tmin.rast <- raster(files[j])
    tmin.rast.j <- extract(tmin.rast, latlong[i,2:1])
    if(j == 1){
      tmin.rast.p <- tmin.rast.j
    }
    else{
      tmin.rast.p <- tmin.rast.p + tmin.rast.j
    }
  }
  tmin.rast.p <- tmin.rast.p/3
  if(i == 1){
    tmin.rast.o <- tmin.rast.p
  }
  else{
    tmin.rast.o <- c(tmin.rast.o, tmin.rast.p)
  }
}
tmin.latlong <- cbind(latlong, tmin.rast.o)
dim(tmin.latlong)

### Mean Tmax for summer months
setwd("/Volumes/Smithers/Rasters/")
files <- c("wc2.0_30s_tmax_06.tif", "wc2.0_30s_tmax_07.tif", "wc2.0_30s_tmax_08.tif")
i <- 1
j <- 1
for(i in 1:nrow(latlong)){
  for(j in 1:length(files)){
    tmax.rast <- raster(files[j])
    tmax.rast.j <- extract(tmax.rast, latlong[i,2:1])
    if(j == 1){
      tmax.rast.p <- tmax.rast.j
    }
    else{
      tmax.rast.p <- tmax.rast.p + tmax.rast.j
    }
  }
  tmax.rast.p <- tmax.rast.p/3
  if(i == 1){
    tmax.rast.o <- tmax.rast.p
  }
  else{
    tmax.rast.o <- c(tmax.rast.o, tmax.rast.p)
  }
}
tmax.latlong <- cbind(latlong, tmax.rast.o)
dim(tmin.latlong)

### Get Mean solar radiation in kJ/m^2/s^-1
setwd("/Volumes/Smithers/Rasters/")
files <- c("wc2.0_30s_srad_06.tif", "wc2.0_30s_srad_07.tif", "wc2.0_30s_srad_08.tif")
i <- 1
j <- 1
for(i in 1:nrow(latlong)){
  for(j in 1:length(files)){
    srad.rast <- raster(files[j])
    srad.rast.j <- extract(srad.rast, latlong[i,2:1])
    if(j == 1){
      srad.rast.p <- srad.rast.j
    }
    else{
      srad.rast.p <- srad.rast.p + srad.rast.j
    }
  }
  srad.rast.p <- srad.rast.p/3
  if(i == 1){
    srad.rast.o <- srad.rast.p
  }
  else{
    srad.rast.o <- c(srad.rast.o, srad.rast.p)
  }
}
srad.latlong <- cbind(latlong, srad.rast.o)


### Calculate Hargreaves climatic moisture deficit
cmd <- hargreaves(tmin.latlong[,3], tmax.latlong[,3], Ra = (srad.latlong[,3])*0.0001, Pre = map.latlong[,3], na.rm = F)
meanSummerHMD <- cbind(latlong, as.matrix(as.numeric(cmd)))
dim(tmax.latlong)
dim(tmin.latlong)
dim(meanSummerHMD)



##### Combine all data into out array
allclimatedata <- cbind(rownames(latlong), latlong[,1], latlong[,2], mat.latlong[,5], mwmt.latlong[,1], ahm[,3],
                  msp.latlong[,3], tmin.latlong[,3], tmax.latlong[,3], srad.latlong[,3], meanSummerHMD[,3], shm.latlong[,3])
allclimatedata[1:8,]
dim(allclimatedata)

##### Set column names of data
colnames(allclimatedata) <- c("Accession", "Latitude", "Longitude", "MeanAnnTemp", "MeanWarmestMonthTemnp", 
                              "AnnualHeatMoistIndx", "May2SeptPrec", "TempMin", "TempMax", "SolarRad", 
                              "HrgrvsSummerClimMoistDef", "SmrHeatMoistIndex")

##### Write Raw data to csv
setwd("~/Desktop/")
dim(allclimatedata)
write.csv(allclimatedata, file = "Boardman_HiLow_LocalClimateVars_90Acc_2022-02-11.csv", row.names = F, quote = F)


##### Create normalized data set in z-scores, StDevs from mean
allclimatedata.scaled <- scale(matrix.numeric(as.matrix(allclimatedata[,2:ncol(allclimatedata)])))

##### Write normalized z-score data to csv file
dim(allclimatedata.scaled)
write.csv(cbind(rownames(allclimatedata), allclimatedata.scaled),
  file = "Boardman_HiLow_ScaledLocalClimateVars_90Acc_2021-02-25.csv", row.names = F)




