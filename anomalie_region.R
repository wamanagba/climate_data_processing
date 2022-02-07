rm(list=ls())


data <- read.csv("C:/Users/Yacou/Desktop/Yacouba_New/Stage_Climate_Centre/burk/4_10_region.csv")
vect=colnames(data)[-1]


df=data

a=quant=as.data.frame(sapply(df, function(x) quantile(x, 1/5)))
quant = quant[,1]
quant = quant[-1]
#vect=c('Bale','Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')
quant=as.data.frame(cbind(vect,quant))
#dt=as.data.frame(vec,dt$`sapply(df, function(x) quantile(x, 1/3))`)
quant <- data.frame(quant[,-1], row.names = quant[,1])
#as.numeric(quant['Bale',])


da= as.integer(data['Boucle.du.Mouhoun'] <= as.numeric(quant['Boucle.du.Mouhoun',]))

vec=colnames(data)[-c(1:2)]
for (cp in vec){
  d=as.integer(data[cp] <= as.numeric(quant[cp,]))
  da=cbind(da,d)
}
da=as.data.frame(da)
da=cbind(data['year'],da)
da_b=da
colnames(da)[2:14]=vect
brt=bur_binaire=da[2:14]
write.csv(brt, here::here("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre","reg_5_bur_binaire.csv"),row.names = FALSE )

#colnames(da)[2:46]=shp$NAME_2

#########################

rm(list=ls())

data <- read.csv('C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\mrt\\4_10_region.csv',encoding = "UTF-8")



shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\gadm36_MRT_shp\\gadm36_MRT_1.shp")
#shp=shapefile("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/TAMSAT/BFA_adm1.shp")

#LZ_names=paste0("LZ",sprintf("%02d", 1:length(shp$NAME_2)))

df=data

a=quant=as.data.frame(sapply(df, function(x) quantile(x, 1/5)))
quant = quant[,1]
quant = quant[-1]

vect=names(data)[2:14]
quant=as.data.frame(cbind(vect,quant))
#dt=as.data.frame(vec,dt$`sapply(df, function(x) quantile(x, 1/3))`)
quant <- data.frame(quant[,-1], row.names = quant[,1])
#as.numeric(quant['Bale',])


da= as.integer(data['Adrar'] <= as.numeric(quant["Adrar",]))


vec=vect[2:13]

for (cp in vec){
  d=as.integer(data[cp] <= as.numeric(quant[cp,]))
  da=cbind(da,d)
}

da=as.data.frame(da)
da=cbind(data['year'],da)

colnames(da)[2:14]=vect
da_mrt=da
da=da[2:14]
#write.csv(da, here::here("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre","mrt_region_5.csv"),row.names = FALSE )


#######################################################################################


library(sf)
library(ggplot2)
library(rgdal)
library(rgeos)

#Reading the shapefiles
sf <- st_read(dsn="C:\\Users\\User\\Desktop\\cty_council_dist.shp", layer="cty_council_dist_haw")
shape <- readOGR(dsn="C:\\Users\\User\\Desktop\\cty_council_dist.shp", layer="cty_council_dist_haw")
shp=shapefile("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/TAMSAT/BFA_adm1.shp")
shape=shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm2.shp")


#To view the attributes
head(shape@da)
summary(sf)

#Plotting the shapefile
plot(shape)
plot(sf)

#Covert the coordinate system
sf_gcs <- st_transform(sf, crs = "EPSG:4326")
shape_gcs <- spTransform(shape, CRS=CRS("+init=EPSG:4326"))

#Plotting the shapefile
plot(shape)
plot(sf_gcs)

#Plotting the districts only
plot(shape["NAME_2"], axes = TRUE, main = "Province")
























da=data['Bale']-dta['Bale',]

vec=c('Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')

for (cp in vec){
  d=data[cp]-dta[cp,]
  da=cbind(da,d)
  }
da=as.data.frame(da)
da=cbind(data['year'],da)

###################

da$colour <- ifelse(da$Bale < 0, "negative","positive")

x11();ggplot(da,aes(year,Bale,label=""))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(negative="firebrick1",positive="steelblue"))

for (cp in vec){
x11();ggplot(da,aes(year,Poni,label=""))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue"))
}
