
# This script plots spatially aggregated data over a period of time
library(rasterVis)
rm(list=ls())
############ Load necessary libraries
suppressPackageStartupMessages({
  library(sp)
  library(rgdal)
  #library(geosphere)
  library(ncdf4)
  library(lubridate)
  library(stringr)
  #library(tidyr)
  #library(ggplot2)
  #library(caTools)
  #library(RMAWGEN)
  #library(signal)
  #library(forecast)
  #library(futureheatwaves)
  #library(RmarineHeatWaves)
  #library(plyr)
  #library(dplyr)
  #library(vegan)
  #require(IRanges)
  #library(weathermetrics)
  #library(FactoMineR)
  #library(missMDA)
  #library(TTR)
  #library(pspline)
  #library(sfsmisc)
  library(raster)
  #library(rasterVis)
  #library(verification)
  #library(SpecsVerification)
  #library(s2dverification)
  #library(easyVerification)
  #library(PRROC)
  #library(SpatialVx)
  #library(extRemes)
  #library(gridExtra)
  #library("readxl")
})
set_Polypath(FALSE)

#Directories
basepath= 'C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\'

#Import shapefile
#shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm2.shp")
shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\gadm36_MRT_shp\\gadm36_MRT_2.shp")

#Color parameters (precipitation as an example)
breakpoints=c(-Inf,seq(0.5,5,0.5),Inf)
cols=colorRampPalette(c("white", "red","darkred", "orange", "black"))(length(breakpoints)-1)

# Import aggregated data (years in rows and provinces in columns)
basepath= 'C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\'

# Import aggregated data (years in rows and provinces in columns)
binary_precip= read.table(paste0(basepath, "mrt_binaire.csv"), sep=",", dec=".", header=T)
#names(binary_precip)= NULL
#binary_precip=da[2:46]
#Add the aggregated data into the shapefile
year=1983
for (i in 1:39){
x=cut(c(t(as.numeric(binary_precip[i,]))),breakpoints) ; levels(x)=cols
shp[[paste0("year_", year)]]=as.vector(x)
year=year+1
}



# Plot
png(paste0(basepath,"mrt_3_prov.png"), height=800, width=1200, type="cairo")
par(mfrow=c(5,8))
for (year in 1983:2021){
plot(shp, col=shp[[paste0("year_", year)]], main= year)
}
dev.off()

