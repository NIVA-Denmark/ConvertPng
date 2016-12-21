library("raster")
library("dplyr")


# folderin="C:/Data/GitHub/L&F/png/"
# folderout="C:/Data/GitHub/L&F/png/"
# file="E4. birds_eider_source_data_conv"
 load("activearea2.Rda")
     
     
convertrda<-function(folderin,folderout,file){
  
#load("activearea.Rda")
#load(paste0(folderin,"activearea.Rda"))
#Dataframe saved  "active"

load(paste0(folderin,file,".Rda"))
#Dataframe saved  "mapout"

map<-mapout
map1<-map[,,1]

rast1 = raster(map1)
crs(rast1)<-CRS("+init=epsg:3035")
extent(rast1) = c(3875412.1,4751464.1,3454009.3,3931992.3)
nx1<-ncol(rast1)
ny1<-nrow(rast1)

rast2<-raster(nrow=886,ncol=488)
crs(rast2)<-CRS("+init=epsg:3035")
extent(rast2) = c(3875412.1,4751464.1,3454009.3,3931992.3)

rast2<-resample(rast1,rast2,method='bilinear') #ngb'
#rast2 <- projectRaster(rast1, crs=CRS("+init=epsg:3035"), res=1000, method="bilinear")
map2<-as.matrix(rast2)
plot(rast2)
dat2<-as.data.frame(getValues(rast2))

# Replace NA's with 0
#dat2[,1]<-ifelse(is.na(dat2[,1]),0,dat2[,1])

varname<-gsub(" ", "_", file)
varname<-gsub("\\.", "", varname)
names(dat2)<-"Value"

dfjoin<-cbind(active,dat2)
dfjoin<-filter(dfjoin,OK==1,!is.na(Value))


dfout<-select(dfjoin,x,y,Value)
dfout$x<-round(dfout$x,0)
dfout$y<-round(dfout$y,0)
dfout$Value<-round(dfout$Value,3)
names(dfout)[names(dfout)=="Value"]<-varname

write.table(dfout,file=paste0(folderout,file,".csv"), row.names=FALSE,quote=FALSE,sep=',')

}

r <- raster(nrow=3, ncol=3)
r[] <- 1:ncell(r)
s <- raster(nrow=10, ncol=10)
s <- resample(r, s, method='bilinear')
#par(mfrow=c(1,2))
#plot(r)
#plot(s)

# dat1<-as.data.frame(getValues(rast1))
# dat1$x<-xFromCell(rast1,1:nrow(dat1))
# dat1$y<-yFromCell(rast1,1:nrow(dat1))
# 
# names(dat1)[1]<-"birds_eider"
# 
# dat1<-filter(dat1,!is.na(birds_eider))
# write.table(dat1,file=paste0(folderout,"birds_eider_201m.csv"), row.names=FALSE,quote=FALSE,sep=',')

