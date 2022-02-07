
# This script plots spatially aggregated data over a period of time
library(rasterVis)

#Directories
basepath= "./"

#Import shapefile
shp=readOGR("/its/home/kg312/shapefiles/Mauritania/gadm36_MRT_1.shp")


#Color parameters (precipitation as an example)
breakpoints=c(-Inf,seq(500,1000,50),Inf)
cols=colorRampPalette(c("lightgreen", "yellow","orange","red", "darkred"))(length(breakpoints)-1)

# Import aggregated data (years in rows and provinces in columns)
binary_precip= read.table(paste0(basepath, "rain_exposure_adm3.csv"), sep=",", dec=".", header=T)

#Add the aggregated data into the shapefile
year=1981
for (i in 1:ncol(binary_precip)){
x=cut(c(t(binary_precip[i,])),breakpoints) ; levels(x)=cols
shp[[paste0("year_", year)]]=as.vector(x)
year=year+1
}


# Plot
png(paste0(basepath,"binary_precip.png"), height=800, width=1200, type="cairo")
par(mfrow=c(5,8))
for (year in 1981:2020){
plot(shp, col=shp[[paste0("year_", year)]], main= year)
}
dev.off()

