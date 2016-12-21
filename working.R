# Top 3931992.3
# Bottom 3454009.3
# Left 3875412.1
# Right 4751464.1




rm(list=ls())

# library("plyr")
library("dplyr")
library("png")
library("raster")
# library("tidyr")
# library("data.table")

folder="C:/Data/GitHub/L&F/png/"
file="active_area_600"
map=readPNG(paste0(folder,file,".png"), FALSE)
map1<-map[,,1]

rast1 = raster(map1)
crs(rast1)<-CRS("+init=epsg:3035")
extent(rast1) = c(3875412.1,4751464.1,3454009.3,3931992.3)
nx1<-ncol(rast1)
ny1<-nrow(rast1)


rast2<-raster(nrow=886,ncol=488)
crs(rast2)<-CRS("+init=epsg:3035")
extent(rast2) = c(3875412.1,4751464.1,3454009.3,3931992.3)

rast2<-resample(rast1,rast2,method='ngb')

#plot(rast1)
#plot(rast2)

#rast2 <- projectRaster(rast1, crs=CRS("+init=epsg:3035"), res=1000)
nx2<-ncol(rast2)
ny2<-nrow(rast2)
dat2<-getValues(rast2)

map2<-as.matrix(rast2)

plot(rast2)
df<-as.data.frame(rast2)
df$r<-1:nrow(df)
df$row<-ceiling((df$r-0.5)/nx2)
df$col<-df$r-(df$row-1)*nx2
df$x<-xFromCell(rast2,1:nrow(df))
df$y<-yFromCell(rast2,1:nrow(df))

df_active<-select(df,OK=layer,id=r,x,y,row,col)
df_active$OK<-ifelse(df_active$OK==1,1,NA)


active<-df_active
save(active,file="activearea2.Rda")

#map2<-matrix(data=dat2,nrow=ny2,ncol=nx2,byrow=TRUE)

mapout<-array(NA, c(ny2,nx2,3))
mapout[,,1:3]<-map2

writePNG(mapout,paste0(folder,file,"_conv.png"))
save(mapout,file=paste0(folder,file,"_conv.Rda"))

xyFromCell(rast2, c(1, ncol(rast2), ncell(rast2)-ncol(rast2)+1, ncell(rast2)))


#df2<-df[df$x==1,]
# 
# 
# r <- raster(nrow=3, ncol=3)
# r[] <- 1:ncell(r)
# s <- raster(nrow=10, ncol=10)
# s <- resample(r, s, method='bilinear')
# #par(mfrow=c(1,2))
# #plot(r)
# #plot(s)
# 
# 
# newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
# 
# projection(rast1) = CRS("+proj=longlat +datum=WGS84")
# test<-map1[1300:1400,1600:1700]
# 
# extent=c(3875412.1,4751464.1,3454009.3,3931992.3) #x0,x1,y0,y1 UTM
# 
# lx=extent[2]-extent[1]
# ly=extent[4]-extent[3]
# 
# nx=4357
# ny=2187
# dx=lx/nx
# dy=ly/ny
