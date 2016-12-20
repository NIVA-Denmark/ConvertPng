rm(list=ls())

library("plyr")
library("dplyr")
library("png")
library("tidyr")
library("data.table")

folder="C:/Data/GitHub/L&F/png/"

#file="mam_harbourPorpoise"
file="Bridges_dams"

#========== Define a row of cells in the image from where a colour scale will be read
use_colour_scale=FALSE

# 2 y-values and x-value defining the line through the legend scale
scalepoints<-c(174,369,1716)
scalevalues<-c(0,0.6) # The lowest and highest values of the scale

#========== Define x,y locations of individual cells and the value which the colours found there represent
use_colour_categories=TRUE
#colourpoints<-c(1716,552,NA) 
colourpoints<-c(3469,80,1,
                3215,406,0,
                2508,856,0,
                2492,170,0,
                2392,426,0)

# Positions x,y to match RGB and corresponding numeric value
# These will override scalevalues

#========== Define specific RGB values and their corresponding numeric values
use_RGB_list=FALSE
RGBvalues<-c(0,0,0,NA) # Groups of 4 values: R,G,B and the corresponding numeric value
# these will override scalevalues and colour points


#file="birds_kittiwake"
#scalepoints<-c(649,862,3700)

mPoints=matrix(colourpoints,nrow=3) 
mPoints=t(mPoints)
mRGB=matrix(RGBvalues,nrow=4)
mRGB=t(mRGB)

dfPoints<-as.data.frame(mPoints)
colnames(dfPoints)<-c("X","Y","Value")
dfRGB<-as.data.frame(mRGB)
colnames(dfRGB)<-c("R","G","B","Value")


# Map three RGB values to single integer value.
# Every RGB primary color can only have one of 256 values.



valuefromrgb<-function(nrgb,maprgb)
{
  maprgb$dist1<-maprgb[,1]-nrgb[1]
  maprgb$dist2<-maprgb[,2]-nrgb[2]
  maprgb$dist3<-maprgb[,3]-nrgb[3]
  maprgb$dist<-maprgb$dist1^2+maprgb$dist2^2+maprgb$dist3^2
  maprgb<-arrange(maprgb,dist)
  return(maprgb[1,4])
}

map=readPNG(paste0(folder,file,".png"), FALSE)


#Separate channels and arrange into lists

dtR<-data.table(map[,,1])
dtR$Y<-as.numeric(rownames(dtR))
dtR<-gather(dtR, "X", "R", -Y)
dtR$X<-as.numeric(substr(dtR$X,2,99999))
dtG<-data.table(map[,,2])
dtG$Y<-as.numeric(rownames(dtG))
dtG<-gather(dtG, "X", "G", -Y)
dtG$X<-as.numeric(substr(dtG$X,2,99999))
dtB<-data.table(map[,,3])
dtB$Y<-as.numeric(rownames(dtB))
dtB<-gather(dtB, "X", "B", -Y)
dtB$X<-as.numeric(substr(dtB$X,2,99999))


dtRGB<-left_join(dtR,dtG,by=c("X"="X","Y"="Y"))
dtRGB<-left_join(dtRGB,dtB,by=c("X"="X","Y"="Y"))
dtRGB<-as.data.table(dtRGB)

#If method using RGB values from a scale is active then find the colours and corresponding values
if(use_colour_scale==TRUE){
  # Strip containing colour scale
  scaleRGB=map[scalepoints[1]:scalepoints[2], scalepoints[3], ]
  dt_scaleRGB<-data.table(scaleRGB)
  colnames(dt_scaleRGB)<-c("SR","SG","SB")
  dt_scaleRGB$Value<-((nrow(scaleRGB):1)/nrow(scaleRGB))
  dt_scaleRGB$Value<- scalevalues[1]+((scalevalues[2]-scalevalues[1])*dt_scaleRGB$Value)
}

#If method using RGB values from specific points is active then find the colours and corresponding values
#i.e. "Categorical" colours
if(use_colour_categories==TRUE){
  # Find RGB values for the unique points
  dfPoints<-left_join(dfPoints,dtRGB,by=c("X"="X","Y"="Y"))
  dfPoints<-select(dfPoints,SR=R,SG=G,SB=B,Value)
  
  # Add the unique points to the list of RGBs from the scale
  if(use_colour_scale==TRUE){
    dt_scaleRGB2<-rbind(dt_scaleRGB,dfPoints)
  }else{
    dt_scaleRGB2<-dfPoints
  }
}



#Check that dt_scaleRGB is data.table before the heavy calculations
dt_scaleRGB2<-as.data.table(dt_scaleRGB2)

# Get all distinct combinations of R,G,B from the map
dtRGBdistinct<-as.data.table(distinct(dtRGB,R,G,B))

# Cartesian product of distinct RGB and scale RGB values
dtRGB2<-setkey(dtRGBdistinct[,c(k=1,.SD)],k)[dt_scaleRGB2[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]

# Find mean square distance between RGB (map) and RGB (scale) for all combinations
dtRGB2[, dist:=((R-SR)^2+(G-SG)^2+(B-SB)^2)]

# Find the minimum mean square distance for each RGB (map)
dtRGB3<-dtRGB2 %>%
  group_by(R,G,B) %>%
  summarise(min=min(dist))

# select the combination of RGB (map) and RGB (scale) with the smallest distance
dtRGB4<-left_join(dtRGB3,select(dtRGB2,R,G,B,dist,Value),by=c("R"="R","G"="G","B"="B","min"="dist"))

# For a few RGB (map) combinations there can more than one RGB (scale) at the same distance
# Take the average of the value for the matching RGB scale values.
dtRGB5<-dtRGB4 %>%
  group_by(R,G,B) %>%
  summarise(Value=mean(Value))

# Replace values for specific RGB combinations with values from user defined combinations
if(use_RGB_list){
  dtRGB5[dtRGB5$R==dfRGB$R & dtRGB5$G==dfRGB$G & dtRGB5$B==dfRGB$B,'Value']<-dfRGB[,'Value']
}

#Check that dtRGB5 is data.table
dtRGB5<-as.data.table(dtRGB5)

dtValue<-left_join(dtRGB,dtRGB5,by=c("R"="R","G"="G","B"="B"))
dtValue<-arrange(select(dtValue,X,Y,Value),X,Y)
dtValueWide<-dtValue %>% spread(X, Value)
dtValueWide$Y<-NULL

mapout<-map
mapvalues<-as.matrix(dtValueWide)

mapout[,,1]<-mapvalues
mapout[,,2]<-mapvalues
mapout[,,3]<-mapvalues

writePNG(mapout,paste0(folder,file,"_conv.png"))
save(mapout,file=paste0(folder,file,"_conv.Rda"))

