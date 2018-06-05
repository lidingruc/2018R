#########################################
#### Applied Spatial Statistics in R ####
#### Yuri M. Zhukov                  ####
#### 20 January 2010                 ####
#### modified by liding RUC          ####
#### 13 December 2017                ####
#########################################
#http://www.people.fas.harvard.edu/~zhukov/spatial.html

####目录###
# section1:
#1.1 point data 
#1.2 polygon data
#1.3 grid data
#1.4 data management

# section2:
#2.1 Distance Conversion
#2.2 Spatial Autocorrelation

# section3:
# Spatial Weights   

# section4:
# Point Pattern Analysis   

# section5:
# Geostatistics 

# section6:
#6.1 Spatial Autologistic Model  
#6.2 GWR
#6.3 Spatial Regression 

## Clear the workspace

rm(list=ls())


## Install packages

if (!require(maps)) install.packages("maps")
if (!require(maptools)) install.packages("maptools")
if (!require(sp)) install.packages("sp")
if (!require(spdep)) install.packages("spdep")
if (!require(gstat)) install.packages("gstat")
if (!require(splancs)) install.packages("splancs")
if (!require(spatstat)) install.packages("spatstat")
if (!require(lattice)) install.packages("lattice")
if (!require(pgirmess)) install.packages("pgirmess")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(classInt)) install.packages("classInt")
if (!require(spgwr)) install.packages("spgwr")



## Load spatial packages

library(maps)         ## Projections
library(maptools)     ## Data management
library(sp)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(splancs)      ## Kernel Density
library(spatstat)     ## Geostatistics
library(pgirmess)     ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(spgwr)        ## GWR


## Set working directory (if you already downloaded the files)

setwd("/Users/liding/E/Bdata/liding17/2018R/l14spa/intro")	## Mac
# setwd("C:\\") 					## Windows               
load("Datasets.RData")
ls()


##或者 Load directly from URL

url <- url("http://www.people.fas.harvard.edu/~zhukov/Datasets.RData")
load(url)
ls()


##或者 If loading from URL fails...

#download.file(url="http://www.people.fas.harvard.edu/~zhukov/Datasets.RData",dest="/Users/yurizhukov/Desktop/Datasets.RData")
load("Datasets.RData")
ls()

##save(laos,crime,cities,volcano,election,dat88,mat88,file="Datasets.RData")




####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 1              ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################


#########################################
#### Point Data: Baltimore Crime     ####
#########################################

## Explore data

head(crime)
dim(crime)

data <- crime


## Create matrix of coordinates

sp_point <- cbind(data$LONG, data$LAT)
colnames(sp_point) <- c("LONG","LAT")
head(sp_point)


## Projection: UTM Zone 17

proj <- CRS("+proj=utm +zone=17 +datum=WGS84")


## Create spatial object

data.sp <- SpatialPointsDataFrame(coords=sp_point,data,proj4string=proj)


## Bounding box of data points

bbox(data.sp)


## Plot crime locations

par(mar=c(2,2,0.2,0.2))
plot(data.sp,pch=16, cex=.5, axes=T)

dev.off()

library(ggplot2)
cdata <- fortify(data)
#########################################
#### Polygon Data: 2004 Election     ####
#########################################

## Explore data

summary(election)
names(election)
data <- election


## Lambert Conformal Conic Projection
#设定投影系统
#原来的投影有错误
#proj4string(data) <- CRS("+proj=lcc+lon_0=90w +lat_1=20n +lat_2=60n")
#spTransform
summary(data)[1:4]


## Plot basemap of counties

par(mar=c(0,0,0,0))
plot(data)

dev.off()
#http://127.0.0.1:12882/graphics/plot_zoom_png?width=1200&height=335

## Plot counties + Baltimore crime locations

par(mar=rep(0.5,4))
plot(election,xlim=bbox(data.sp)[1,],ylim=bbox(data.sp)[2,],col="beige")
plot(data.sp,pch=1, cex=.5,add=T, col="blue")

dev.off()

data2<-election[election@data$STATE_NAME=="Texas",]
#proj4string(data2)<- CRS("+proj=utm +zone=20 +datum=WGS84")
plot(data2)
######
## Plotting Attributes
######


## Look at some of the options

par(mar=c(0,10,0,0),cex=.6)
display.brewer.all(n=5)

dev.off()

## Create blue-state red-state palette

br.palette <- colorRampPalette(c("blue", "red"), space = "rgb")
br.palette(10)

par(mar=c(0,3,0,0),cex=.6)
plot(seq(1,5),cex=10,pch=11,col=br.palette(n=5))

dev.off()


## Let's plot the % of vote for Bush

data <- election
summary(data)
var <- data$Bush_pct


## Easy but unflexible option
#关于投影的说明：https://github.com/OSGeo/proj.4/wiki/GenParms
#http://proj4.org/projections/index.html
#proj4string(data) <- CRS("+proj=longlat +datum=WGS84")

proj4string(data) <- CRS("+proj=lcc")

spplot(data, zcol="Bush_pct", col.regions=br.palette(100), main="Percent of County Vote for Bush (2004)")

dev.off()


## Harder but more flexible option:

## Define number of colors in a palette

pal <- br.palette(n=5)


## Fixed intervals
classes_fx <- classIntervals(var, n=5, style="fixed", fixedBreaks=c(0, 10, 25, 50, 75, 100), rtimes = 1)
classes_sd <- classIntervals(var, n=5, style = "sd", rtimes = 1)
classes_fi <- classIntervals(var, n=5, style = "fisher", rtimes = 3)
classes_eq <- classIntervals(var, n=5, style = "equal", rtimes = 1)
classes_km <- classIntervals(var, n=5, style = "kmeans", rtimes = 1)
classes_qt <- classIntervals(var, n=5, style = "quantile", rtimes = 1)


## Compare classes

par(mar=c(2,2,2,1)+0.1, mfrow=c(2,3))
plot(classes_fx, pal=pal, main="Fixed Intervals", xlab="", ylab="")
plot(classes_sd, pal=pal, main="Standard Deviation", xlab="", ylab="")
plot(classes_fi, pal=pal, main="Fisher-Jenks", xlab="", ylab="")
plot(classes_km, pal=pal, main="K Means", xlab="", ylab="")
plot(classes_eq, pal=pal, main="Equal Interval", xlab="", ylab="")
plot(classes_qt, pal=pal, main="Quantile", xlab="", ylab="")

dev.off()



## Plot using fixed intervals

cols <- findColours(classes_fx, pal)

par(mfrow=c(1,1))

par(mar=rep(0,4))
plot(election,col=cols,border=NA)
legend(x="bottom",cex=.7,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Percent of County Vote for Bush (2004)",ncol=5)


## Plot binary Bush/Kerry (Red/Blue)

cols <- ifelse(data$Bush > data$Kerry,"red","blue")

par(mar=rep(0,4))
plot(election,col=cols,border=NA)

legend(x="bottom",cex=.7,fill=c("red","blue"),bty="n",legend=c("Bush","Kerry"),title="Winner of County Vote (2004)",ncol=2)

dev.off()



#########################################
#### Grid Data: Maunga Whau Volcano  ####
#########################################
# 等高线图

head(volcano)[,1:6]          ## Take a look at the data structure
dim(volcano)            ## 87 x 61 grid of elevation points

z <- volcano            ## Height Variable
x <- 10*(1:nrow(z))     ## 10 Meter Spacing (S-N)
y <- 10*(1:ncol(z))     ## 10 Meter Spacing (E-W)


## Contour Plot

par(mar=rep(0.5,4))               
contour(x, y, z, levels=seq(from=min(z), to=max(z), by=10),axes=F)

dev.off()


## Gradient
## 热力图
par(mar=rep(0.5,4))               
image(x, y, z, col=terrain.colors(100), axes=F)

dev.off()


## Gradient + Countour

par(mar=rep(0.5,4))               
image(x, y, z, col=terrain.colors(100), axes=F)
contour(x, y, z, levels=seq(from=min(z), to=max(z), by=10),axes=F, add=T)
contour(x, y, z, levels=seq(from=min(z), to=max(z), by=5),axes=F, add=T)

dev.off()


## 3-D Plot

par(mar=rep(0,4))               
persp(x,y,z,theta=120,phi=20,scale=F,axes=F)

dev.off()


## 3-D Elevation Plot w/ color

z <- 2 * volcano        ## Exaggerate the relief
x <- 10 * (1:nrow(z))   ## 10 meter spacing (S to N) 
y <- 10 * (1:ncol(z))   ## 10 meter spacing (E to W) 


## Create new grid

z0 <- min(z) - 20 
z <- rbind(z0, cbind(z0, z, z0), z0) 
x <- c(min(x) - 1e-10, x, max(x) + 1e-10) 
y <- c(min(y) - 1e-10, y, max(y) + 1e-10) 


## Create matrix of base colors

fcol <- matrix("green3", nr = nrow(z)-1, nc = ncol(z)-1) 
fcol[ , i2 <- c(1,ncol(fcol))] <- "gray" 
fcol[i1 <- c(1,nrow(fcol)) , ] <- "gray" 


## Take average of four neighboring values for palette

zi <- (volcano[ -1,-1] + volcano[ -1,-61] + volcano[-87,-1] + volcano[-87,-61])/4
pal <- terrain.colors(20)[cut(zi, quantile(zi, seq(0,1, len = 21)), include.lowest = TRUE)]
fcol[-i1,-i2] <- pal


## Plot it

par(mar=rep(0,4))
persp(x, y, z, theta=120, phi=15, col = fcol, scale = FALSE, shade = 0.4, border = NA) 

dev.off()

           
         
#########################################
#### Data Management                 ####
#########################################


## What if you have some date you'd like to merge with a map?

## 已经在github文件夹中，不需要下载
## 下载数据(if you haven't done so already)

url1 <- "http://www.people.fas.harvard.edu/~zhukov/world.shp"
url2 <- "http://www.people.fas.harvard.edu/~zhukov/world.shx"
url3 <- "http://www.people.fas.harvard.edu/~zhukov/world.dbf"
url4 <- "http://www.people.fas.harvard.edu/~zhukov/polity.csv"

dest1 <- "/Users/yurizhukov/Desktop/world.shp"
dest2 <- "/Users/yurizhukov/Desktop/world.shx"
dest3 <- "/Users/yurizhukov/Desktop/world.dbf"
dest4 <- "/Users/yurizhukov/Desktop/polity.csv"

download.file(url1,dest1)
download.file(url2,dest2)
download.file(url3,dest3)
download.file(url4,dest4)

## 已经在github文件夹中，不需要下载

setwd("/Users/liding/E/Bdata/liding17/2018R/l14spa/intro")


## Let's open a world map Shapefile

map <- readShapePoly("world",IDvar="MAP_CCODE",proj4string=CRS("+proj=eqc +lon_0=90w"))          ## Equidistant Cylindrical
summary(map)


## Plot the study region

par(mar=rep(0,4))
plot(map)

dev.off()


## Open POLITY IV dataset

polity <- read.csv("polity.csv")
names(polity)


## The two maps have common ID variable: CCODE
## Four step merging procedure:
## (1) Extract data.frame from map
## (2) Use merge() to join Polity data to map data.frame
## (3) Re-order new merged data.frame to conform with map
## (4) Use spCbind to join merged data.frame to map
 
m_ccode <- as.data.frame(map)       
merged <- merge(x=m_ccode, y=polity, by.x="CCODE", by.y="ccode", all.x=T, all.y=F)

merged <- merged[order(merged$MAP_CCODE),]
rownames(merged) <- map$MAP_CCODE

# 合并地图与新数据
map2 <- spCbind(map,merged)
names(map2)


## Remove duplicate rows

map2$CCODE.1 <- NULL
map2$SP_ID.1 <- NULL
map2$COUNTRY.1 <- NULL
map2$MAP_CCODE.1 <- NULL


## Recode Polity variable

map2$polity <- ifelse(map2$polity==-66,NA,map2$polity)
map2$polity <- ifelse(map2$polity==-77,NA,map2$polity)
map2$polity <- ifelse(map2$polity==-88,NA,map2$polity)


## Plot POLITY scores

dem.palette <- colorRampPalette(c("red", "green"), space = "rgb")
spplot(map2,"polity",col.regions=dem.palette(20), main="Polity IV Democracy Scores (2008)")    

dev.off()       

## Save new shapefile

# writePolyShape(map2, "Polity_Map")


####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 2              ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################




#####################################
#### Distance Conversion         ####
#####################################


## Function: Convert km to degrees
km2d <- function(km){
out <- (km/1.852)/60
return(out)
}
km2d(500) ## 500 km

## Function: Convert degrees to km
d2km <- function(d){
out <- d*60*1.852
return(out)
}
d2km(1) ## 1 degree




#########################################
#### Spatial Autocorrelation         ####
#########################################


data <- election
names(data)


## Create matrix of polygon centroids

map_crd <- coordinates(data)


## Contiguity Neighbors

W_cont_el <- poly2nb(data, queen=T)
W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)


## Plot the connections

par(mar=rep(0,4))
plot(W_cont_el_mat,coords=map_crd,pch=19, cex=0.1, col="gray")

dev.off()


## Global Autocorrelation Tests: Moran's I

moran.test(data$Bush_pct, listw=W_cont_el_mat, zero.policy=T)


## Global Autocorrelation Tests: Geary's C

geary.test(data$Bush_pct, listw=W_cont_el_mat, zero.policy=T)


## Global Autocorrelation Tests: Join Count

data$BushWin <- as.factor(ifelse(data$Bush > data$Kerry,1,0))
joincount.multi(data$BushWin, listw=W_cont_el_mat, zero.policy=T)


## Moran Scatterplot

par(mar=c(4,4,1.5,0.5))
moran.plot(data$Bush_pct, listw=W_cont_el_mat, zero.policy=T, xlim=c(0,100),ylim=c(0,100), pch=16, col="black",cex=.5, quiet=F, labels=as.character(data$NAME),xlab="Percent for Bush", ylab="Percent for Bush (Spatial Lag)", main="Moran Scatterplot")


## Local Autocorrelation: Local Moran's I (normality assumption)

lm1 <- localmoran(data$Bush_pct, listw=W_cont_el_mat, zero.policy=T)

data$lm1 <- abs(lm1[,4]) ## Extract z-scores

lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")
# proj4string(data) <- CRS("+proj=lcc")
spplot(data, zcol="lm1", col.regions=lm.palette(20), main="Local Moran's I (|z| scores)", pretty=T)


## Local Autocorrelation: Local Moran's I (saddlepoint approximation) [WARNING: this takes a while to run]

#lm2 <- localmoran.sad(lm(Bush_pct~1,data), nb=W_cont_el, style="W", zero.policy=T)
#head(lm2)


## Local Autocorrelation: Local Moran's I (exact methods)  [WARNING: this takes a while to run]

#lm3 <- localmoran.sad(lm(Bush_pct~1,data), nb=W_cont_el, style="W", zero.policy=T)
#head(lm3)



#####
## Simulated Autocorrelation
#####

weights <- W_cont_el_mat
n <- length(W_cont_el)
uncorr_x <- rnorm(n)
rho1 <- 0.9
rho2 <- -0.9
autocorr_x <- invIrW(weights, rho1) %*% uncorr_x
ncorr_x <- invIrW(weights, rho2) %*% uncorr_x
w.uncorr_x <- lag(weights, uncorr_x, zero.policy=T, NAOK=T)
w.autocorr_x <- lag(weights, autocorr_x, zero.policy=T, NAOK=T)
w.ncorr_x <- lag(weights, ncorr_x, zero.policy=T, NAOK=T)


## Plot observed vs. lagged values

par(mar=c(4,4,2.5,.5))
plot(uncorr_x, w.uncorr_x, xlab="Y ~ N(0, 1)", ylab="Spatial Lag of Y", main=expression(symbol("\162") == 0),col="grey",cex=.5,xlim=c(-4,4),ylim=c(-4,4))
abline(a=0,b=1,lty="dotted")
lines(lowess(uncorr_x, w.uncorr_x), lty=2, lwd=2, col="red")
legend(x="bottomright", lty=2, lwd=2, col="red", legend="LOESS Curve", bty="n")
                        ## Lowess curve
       
par(mar=c(4,4,2.5,.5))                 
plot(autocorr_x, w.autocorr_x, xlab="Y ~ N(0, 1)", ylab="Spatial Lag of Y", main=expression(symbol("\162") == 0.9),col="grey",cex=.5,xlim=c(-4,4),ylim=c(-4,4))
abline(a=0,b=1,lty="dotted")
lines(lowess(autocorr_x, w.autocorr_x), lty=2, lwd=2, col="red")

par(mar=c(4,4,2.5,.5))
plot(ncorr_x, w.ncorr_x, xlab="Y ~ N(0, 1)", ylab="Spatial Lag of Y", main=expression(symbol("\162") == -0.9), ,xlim=c(-4,4),ylim=c(-4,4),col="grey",cex=.5)
abline(a=0,b=1,lty="dotted")
lines(lowess(ncorr_x, w.ncorr_x), lty=2, lwd=2, col="red")

dev.off()



#####
## Correlogram
#####

## Centroids 

map_crd <- coordinates(map2)


## Country capitals

data <- as.data.frame(map2)
merged <- merge(data, cities[,c(1,4,5)], by.x="MAP_CCODE", by.y="MAP_CCODE", all.x=T, all.y=F)
merged <- merged[,c(1,40,41)]

merged <- merged[order(merged$MAP_CCODE),]
rownames(merged) <- map2$MAP_CCODE
map3 <- spCbind(map2,merged)

map_crd2 <- cbind(map3$LONG, map3$LAT)
colnames(map_crd2) <- c("LONG","LAT")
map_crd2 <- ifelse(is.na(map_crd2)==1,map_crd,map_crd2)
head(map_crd2)


## Create binary democracy variable

var <- ifelse(map2$polity>5,1,0)
var <- ifelse(is.na(var)==1,0,var)


## Run Correlogram 
corD1 <- correlog(map_crd2, var, method="Moran")

corD1 <- correlog(map_crd2, var, method="Moran", nbclass=30)
corD1


## Plot Moran's I coefficients and p-values (capitals)

par(mfrow=c(2,1), mar=c(2,4,.5,.5))

plot(x=d2km(corD1[,1]),y=corD1[,2],  type="b", main="",xaxp=c(0, 40000, 20), xaxs="r", ylab="Moran's I Coefficient")
abline(h=0, lty="solid")
plot(x=d2km(corD1[,1]),y=corD1[,3], type="b", main="", ylab="p-value",xaxp=c(0, 40000, 20), xaxs="r")
abline(h=0.05, lty="dashed", col="blue")
legend(x="topleft",bty="n", lty="dashed", col="blue", legend=expression(p<=.05),cex=.7)

dev.off()







####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 3              ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################


#################################
#### Spatial Weights         ####
#################################



data <- map3
IDs <- map3$MAP_CCODE
names(data)


#########
## Contiguity
#########


## Contiguity Neighbors (no snap distance)

W_cont <- poly2nb(data, queen=T)
W_cont_mat <- nb2listw(W_cont, style="W", zero.policy=TRUE)


## Contiguity Neighbors (snap distance = 500km)

W_cont_s <- poly2nb(data, queen=T, snap=km2d(500))
W_cont_s_mat <- nb2listw(W_cont_s, style="W", zero.policy=TRUE)


## Plot the connections

par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(data,border="grey")
plot(W_cont_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("Direct Contiguity")

plot(data,border="grey")
plot(W_cont_s_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("Contiguity + 500 km")

dev.off()



#########
## k = 1
#########

## Centroids
W_knn1 <- knn2nb(knearneigh(map_crd, k=1), row.names=IDs)
W_knn1_mat <- nb2listw(W_knn1)

## Capitals
W_knn1_2 <- knn2nb(knearneigh(map_crd2, k=1), row.names=IDs)
W_knn1_mat_2 <- nb2listw(W_knn1_2)


## Plot the connections

par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(data,border="grey")
plot(W_knn1_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("k=1 (Centroids)")

plot(data,border="grey")
plot(W_knn1_mat_2,coords=map_crd2,pch=19, cex=0.1, col="blue", add=T)
title("k=1 (Capitals)")

dev.off()



#########
## Interpoint distance weights
#########


## Centroids
dist <- unlist(nbdists(W_knn1, map_crd))
W_dist1 <- dnearneigh(map_crd, d1=0, d2=max(dist), row.names=IDs) 
W_dist1_mat <- nb2listw(W_dist1)


## Capitals
dist_2 <- unlist(nbdists(W_knn1_2, map_crd2))
W_dist1_2 <- dnearneigh(map_crd2, d1=0, d2=max(dist_2), row.names=IDs) 
W_dist1_mat_2 <- nb2listw(W_dist1_2)


## Plot the connections

par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(data,border="grey")
plot(W_dist1_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("Minimum Distance (Centroids)")

plot(data,border="grey")
plot(W_dist1_mat_2,coords=map_crd2,pch=19, cex=0.1, col="blue", add=T)
title("Minimum Distance (Capitals)")

dev.off()



#########
## k = 4
#########

## Centroids
W_knn4 <- knn2nb(knearneigh(map_crd, k=4), row.names=IDs)
W_knn4_mat <- nb2listw(W_knn4)


## Capitals
W_knn4_2 <- knn2nb(knearneigh(map_crd2, k=4), row.names=IDs)
W_knn4_mat_2 <- nb2listw(W_knn4_2)


## Plot the connections

par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(data,border="grey")
plot(W_knn4_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("k=4 (Centroids)")

plot(data,border="grey")
plot(W_knn4_mat_2,coords=map_crd2,pch=19, cex=0.1, col="blue", add=T)
title("k=4 (Capitals)")

dev.off()



#########
## Sphere of influence neighbors
#########

## Centroids
map_crd <- coordinates(data)
W_del <- tri2nb(map_crd)
require(RANN)
W_soi <- graph2nb(soi.graph(W_del, map_crd))
W_soi_mat <- nb2listw(W_soi)


## Capitals

W_del_2 <- tri2nb(map_crd2)
W_soi_2 <- graph2nb(soi.graph(W_del_2, map_crd2))
W_soi_mat_2 <- nb2listw(W_soi_2)


## Plot the connections

par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(data,border="grey")
plot(W_soi_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("Sphere of Influence (Centroids)")

plot(data,border="grey")
plot(W_soi_mat_2,coords=map_crd2,pch=19, cex=0.1, col="blue", add=T)
title("Sphere of Influence (Capitals)")

dev.off()



####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 4              ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################



#########################################
#### Point Pattern Analysis          ####
#########################################



data <- crime


## Take subsample of Baltimore dataset

data <- data[sample(500,replace=F),]
names(data)
head(data)
dim(data)


## Create matrix of coordinates 

sp_point <- matrix(NA, nrow=nrow(data),ncol=2)
sp_point[,1] <- data$LONG
sp_point[,2] <- data$LAT
colnames(sp_point) <- c("LONG","LAT")


## Random points

u.x <- runif(n=nrow(sp_point), min=bbox(sp_point)[1,1], max=bbox(sp_point)[1,2])
u.y <- runif(n=nrow(sp_point), min=bbox(sp_point)[2,1], max=bbox(sp_point)[2,2])


## Regular points

r.x <- seq(from=min(sp_point[,1]),to=max(sp_point[,1]),length=sqrt(nrow(sp_point)))
r.y <- seq(from=min(sp_point[,2]),to=max(sp_point[,2]),length=sqrt(nrow(sp_point)))
r.x <- jitter(rep(r.x,length(r.x)),.001)
r.y <- jitter(rep(r.y,each=length(r.y)),.001)


## Plot the points

par(mfrow=c(1,3),mar=c(4,4,1.5,0.5))
plot(x=sp_point[,1],y=sp_point[,2],main="Baltimore Data", xlab="LONG",ylab="LAT",cex=.5)
plot(x=u.x,y=u.y,main="Random Points", xlab="LONG",ylab="LAT",cex=.5)
plot(x=r.x,y=r.y,main="Regular Points", xlab="LONG",ylab="LAT",cex=.5)

dev.off()



######
## G Function
######
# km2d 函数是前面定义的

## G-Test: Baltimore crime

r <- seq(0,km2d(50),length.out=1000)

env <- envelope(ppp(x=sp_point[,1],y=sp_point[,2],window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Gest, r=r, nsim=99, nrank=2)


## G-Test: Uniformly distributed points

env.u <- envelope(ppp(x=u.x,y=u.y,window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Gest, r=r, nsim=99, nrank=2)


## G-Test: Regularly distributed points

env.r <- envelope(ppp(x=r.x,y=r.y,window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Gest, r=r, nsim=99, nrank=2)


GTest <- rbind(env, env.u, env.r)
RANDOM=rep(c("Baltimore Data","Random Pattern","Regular Pattern"), each=length(r))
GTest <- cbind(GTest, RANDOM)

## Plot observed vs. theoretical G values
library(lattice)
xyplot(obs~theo|RANDOM, data=GTest, ylim=c(0,1), xlim=c(0,1), ylab="Observed", xlab="Expected", type="l", main="G Function", panel=function(x, y, subscripts)
	{
		lpolygon(c(x, rev(x)), 
		   c(GTest$lo[subscripts], rev(GTest$hi[subscripts])),
		   border="gray", fill="gray"
		)

		llines(x, y, col="black", lwd=2)
	}
)



######
## F Function
######


## F-Test: Baltimore crime

r <- seq(0,km2d(50),length.out=1000)

env <- envelope(ppp(x=sp_point[,1],y=sp_point[,2],window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Fest, r=r, nsim=99, nrank=2)

## F-Test: Uniformly distributed noise

env.u <- envelope(ppp(x=u.x,y=u.y,window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Fest, r=r, nsim=99, nrank=2)


## F-Test: Regularly distributed noise

env.r <- envelope(ppp(x=r.x,y=r.y,window=owin(bbox(sp_point)[1,],bbox(sp_point)[2,])), fun=Fest, r=r, nsim=99, nrank=2)


FTest <- rbind(env, env.u, env.r)
FTest <- cbind(FTest, RANDOM=rep(c("Baltimore Data","Random Pattern","Regular Pattern"), each=length(r)))


## Plot observed vs. theoretical F values

xyplot(obs~theo|RANDOM, data=FTest, ylim=c(0,1), xlim=c(0,1), ylab="Observed", xlab="Expected", type="l", main="F Function", panel=function(x, y, subscripts)
	{
		lpolygon(c(x, rev(x)), 
		   c(FTest$lo[subscripts], rev(FTest$hi[subscripts])),
		   border="gray", fill="gray"
		)

		llines(x, y, col="black", lwd=2)
	}
)



dev.off()


######
## Kernel Density
######



## Create bounding box for grid 

poly <- as.points(c(min(sp_point[,1]),max(sp_point[,1]),max(sp_point[,1]),min(sp_point[,1])),c(max(sp_point[,2]),max(sp_point[,2]),min(sp_point[,2]),min(sp_point[,2])))
plot(poly, type="l")


## Alternatively, you could use the convex hull

## crds <- coordinates(sp_point)
## poly=crds[chull(crds),]


## Select bandwidth by minimizing MSE

mserw <- mse2d(sp_point, poly=poly, nsmse=100, range=.1)
bw <- mserw$h[which.min(mserw$mse)] ## Bandwidth=.01

par(mar=c(4,4,0.5,0.5))
plot(x=mserw$h, y=mserw$mse, xlab="Bandwidth", ylab="MSE", type="l")
i<-which.min(mserw$mse)
points(mserw$h[i], mserw$mse[i])

dev.off()


## Estimate Kernel Density

sp_points <- SpatialPoints(coords=sp_point, proj4string=CRS("+proj=utm +zone=17 +datum=WGS84"))

grd <- Sobj_SpatialGrid(sp_points,maxDim=100)$SG
grd <- GridTopology(summary(grd)$grid[,1],cellsize=summary(grd)$grid[,2],cells.dim=summary(grd)$grid[,3])

kernel1 <- spkernel2d(sp_point, poly=poly, h0=bw, grd=grd)
kernel2 <- spkernel2d(sp_point, poly=poly, h0=.05, grd=grd)
kernel3 <- spkernel2d(sp_point, poly=poly, h0=.1, grd=grd)
kernel4 <- spkernel2d(sp_point, poly=poly, h0=.15, grd=grd)

df <- data.frame(kernel1=kernel1,kernel2=kernel2,kernel3=kernel3,kernel4=kernel4)
SG <- SpatialGridDataFrame(grd, data=df)


## Plot Kernel Maps

ker.palette <- colorRampPalette(c("white", "orange","red","darkred","brown"), space = "rgb")

spplot(SG,col.regions=ker.palette(100),names.attr=c(paste("Bandwidth = ",bw, sep="", collapse=""),"Bandwidth = 0.05", "Bandwidth = 0.1","Bandwidth = 0.15"), main="Baltimore Crime Locations  (Kernel Density)")






####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 5              ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################



#########################################
#### Geostatistics                   ####
#########################################



## Data on U.S. air strikes in Laos

data <- laos


## Take subsample of Laos bombing dataset

data <- data[sample(500,replace=F),]
names(data)
head(data)
dim(data)


## Open simple world map

data(wrld_simpl)


## Create matrix of coordinates 

sp_point <- matrix(NA, nrow=nrow(data),ncol=2)
sp_point[,1] <- jitter(data$LONG,.001)
sp_point[,2] <- jitter(data$LAT, .001)
colnames(sp_point) <- c("LONG","LAT")

## Create spatial object

data.sp <- SpatialPointsDataFrame(coords=sp_point,data,proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))              ## Projection: UTM zone 48


## Explore 

par(mar=rep(0,4))
plot(data.sp,pch=1,cex=log(data.sp$LOAD_LBS)/5)

dev.off()


## Zoom in on study region

par(mar=c(2,2,0.5,0.5))
plot(wrld_simpl,xlim=bbox(data.sp)[1,]+c(-1,1),ylim=bbox(data.sp)[2,]+c(-2,2),col="lightgrey",axes=T) ## World Map
plot(data.sp,pch=16,cex=.5,col="red",add=T)

dev.off()


## Take a look at the payload variable

bubble(data.sp,"LOAD_LBS")

dev.off()


## Variogram cloud

plot(variogram(log(LOAD_LBS)~1, locations=coordinates(sp_point), data=data.sp, cloud=T),pch=16, cex=1)

dev.off()


## Sample variogram

plot(variogram(log(LOAD_LBS)~1, locations=coordinates(sp_point), data=data.sp, cloud=F),type="b",pch=16)

dev.off()


## Identify outlying pairs

sel <- plot(variogram(LOAD_LBS~1, locations=coordinates(sp_point), data=data.sp, alpha=c(0,45,90,135),cloud=T),pch=16,digitize=T, cex=1, col="blue")
plot(sel,data.sp)

dev.off()


## Find the outlying pairs in the dataset

sel

out.pair <- function(x,data,sel){
	a <- as.data.frame(data[sel$head,as.character(x)])
	b <- as.data.frame(data[sel$tail,as.character(x)])
	ID.a <- round(as.numeric(rownames(as.data.frame(data[sel$head,]))),0)
	ID.b <- round(as.numeric(rownames(as.data.frame(data[sel$tail,]))),0)
	out <- cbind(ID.a,a[,as.character(x)],ID.b,b[,as.character(x)])
	colnames(out) <- c("ID.a",paste(x,".a",sep=""),"ID.b",paste(x,".b",sep=""))
	out
	}

out.pair(x="LOAD_LBS",data=data.sp,sel=sel)


## Directional search

plot(variogram(log(LOAD_LBS)~1, locations=coordinates(sp_point), data=data.sp, alpha=c(0,45,90,135),cloud=T),pch=16)


## Modify cutoff

plot(variogram(log(LOAD_LBS)~1, locations=coordinates(sp_point), data=data.sp, cutoff=.5 ,cloud=F),type="b", pch=16)


## Compare to random distribution

v <- variogram(log(LOAD_LBS) ~ 1, locations=coordinates(sp_point), data.sp)
print(xyplot(gamma ~ dist, v, ylim=c(0,max(v$gamma)+2*sd(v$gamma)), pch = 3, type = 'b', lwd = 2,
	panel = function(x, y, ...) {
        for (i in 1:100) {
        	data.sp$random <- sample(data.sp$LOAD_LBS)
        	v <- variogram(log(random) ~ 1, locations=coordinates(data.sp), data.sp)
        	llines(v$dist, v$gamma, col = 'grey')
		}
		panel.xyplot(x, y, ...)
	},
	xlab = 'distance', ylab = 'semivariance'
))



## Fit variogram with exponential model

v.fit <- fit.variogram(v, vgm(psill=1, model="Exp", range=1))
plot(v, v.fit, pch = 16,cex=.5)

dev.off()

#########
## Ordinary Kriging
#########


## Create empty grid

grd <- Sobj_SpatialGrid(data.sp,maxDim=200)$SG

plot(grd,axes=T,col="grey")
points(data.sp)


## Generate predictions

kr <- krige(log(LOAD_LBS)~1, data.sp, grd, model=v.fit)
spplot(kr,col.regions=rev(terrain.colors(100)), names.attr=c("Predictions","Variance"), main="Ordinary Kriging,  Bomb Load (log)",pch=2,cex=2)



#########
## IDW Interpolation
#########


## Create empty grid

grd <- Sobj_SpatialGrid(data.sp,maxDim=200)$SG

plot(grd,axes=T,col="grey")
points(data.sp)


## Generate predictions

# k=.2
idw.out <- idw(log(LOAD_LBS)~1,data.sp,grd,idp=.2)
spplot(idw.out[1],col.regions=rev(heat.colors(100)), main="IDW Interpolation, Bomb Load (log)",sub="k = 1/5")

# k=1
idw.out <- idw(log(LOAD_LBS)~1,data.sp,grd,idp=1)
spplot(idw.out[1],col.regions=rev(heat.colors(100)), main="IDW Interpolation, Bomb Load (log)",sub="k = 1")

# k=5
idw.out <- idw(log(LOAD_LBS)~1,data.sp,grd,idp=5)
spplot(idw.out[1],col.regions=rev(heat.colors(100)), main="IDW Interpolation, Bomb Load (log)",sub="k = 5")




####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 6.a            ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################



#########################################
#### Spatial Regression              ####
#########################################


data <- election
names(data)


########
## Linear Model
########

mod.lm <- lm(Bush_pct ~ pcincome, data=data)
summary(mod.lm)


## Plot residuals

res <- mod.lm$residuals

res.palette <- colorRampPalette(c("red","orange","white", "lightgreen","green"), space = "rgb")
pal <- res.palette(5)

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=c(-50,-25,-5,5,25,50), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, main="Residuals from OLS Model", pretty=T, border="grey")
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from OLS Model",ncol=5)

dev.off()

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)



########
## SAR Model (WARNING: This takes a while to run)
########


mod.sar <- lagsarlm(Bush_pct ~ pcincome, data = data, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-12)
summary(mod.sar)

res <- mod.sar$residuals

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=c(-50,-25,-5,5,25,50), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from SAR Model",ncol=5)

dev.off()

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)


## Equilibrium effects

## Extract Weights 

names <- attr(W_cont_el,"region.id")
W <- matrix(0,nrow=length(names),ncol=length(names))
for(i in 1:length(names)){
W[i,as.vector(W_cont_el[[i]])] <- 1
}

I <- matrix(0,nrow=nrow(data),ncol=nrow(data))
diag(I) <- 1

X0 <- cbind(data$pcincome)
X1 <- cbind(data$pcincome)
X1[816] <- 2*X1[816]  ## Income in Bronx County doubles
Xd <- X1-X0


## Calculate Equilibrium Effects

## SAR
beta.sar <- mod.sar$coefficients
rho.sar <- mod.sar$rho

EE.sar <- solve(I - rho.sar*W)%*%Xd*beta.sar[2]
names(EE.sar) <- data$NAME
top.sar <- EE.sar[rev(order(EE.sar))][1:10]
top.sar  ## Top ten changes

## OLS
beta.lm <- mod.lm$coefficients
rho.lm <- 0

EE.lm <- solve(I - rho.lm*W)%*%Xd*beta.lm[2]
names(EE.lm) <- data$NAME
EE.lm[816]



########
## SEM Model (WARNING: This takes a while to run)
########


mod.sem <- errorsarlm(Bush_pct ~ pcincome, data = data, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sem)

res <- mod.sem$residuals

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=c(-50,-25,-5,5,25,50), rtimes = 1)
cols <- findColours(classes_fx,pal)

## par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from SEM Model",ncol=5)


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)



########
## SDM Model (WARNING: This takes a while to run)
########


mod.sdm <- lagsarlm(Bush_pct ~ pcincome, data = data, listw=W_cont_el_mat, zero.policy=T, type="mixed", tol.solve=1e-12)
summary(mod.sdm)

res <- mod.sdm$residuals

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=c(-50,-25,-5,5,25,50), rtimes = 1)
cols <- findColours(classes_fx,pal)

## par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from SDM Model",ncol=5)


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)




####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 6.b            ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################


########
## GWR (WARNING: This takes a while to run)
########



## WARNING: This takes a while to run
bwG <- gwr.sel(Bush_pct ~ pcincome, data = data, gweight=gwr.Gauss, verbose=F)
mod.gwr <- gwr(Bush_pct ~ pcincome, data = data, bandwidth=bwG, gweight=gwr.Gauss)


## Residuals

res <- mod.gwr$SDF$gwr.e

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=c(-50,-25,-5,5,25,50), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from GWR Model",ncol=5)

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)


## Coefficients

coef <- mod.gwr$SDF$pcincome

classes_fx <- classIntervals(coef, n=5, style="fixed", fixedBreaks=c(-.005,-.003,-.001,.001,.003,.005), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Local Coefficient Estimates (per capita income)",ncol=3)

dev.off()


####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 6.c            ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################



#########################################
#### Spatial Autologistic Model      ####
#########################################


## This section replicates the model introduced by Michael Ward 
## and Kristian Gleditsch, ``Location, Location, Location: An MCMC 
## Approach to Modeling the Spatial Context of War and Peace,'' 
## Political Analysis 10, n. 3 (2002).


## Data matrix

head(dat88)
class(dat88)


## Minimum distance matrix

head(mat88)
class(mat88)


## Create new DV: civil or international war

y <- I(dat88$cwar==1|dat88$iwar==1) 
dim(dat88)
y[y==T] <- 1
y <- as.numeric(y)
y <- as.matrix(y)
rownames(y) <- dat88$cowid
y

x <- as.matrix(dat88[,c("d","d.s")])
x <- apply(x,2,FUN=as.numeric)
rownames(x) <- dat88$cowid
x


# Create binary contiguity matrix with 475 km snap distance

W  <-  mat88
W[mat88<=475] <- 1
W[mat88>475 | is.na(mat88)] <- 0
W <- apply(W,MARGIN=2,as.numeric)
rownames(W) <- rownames(mat88)
colnames(W) <- rownames(mat88)
W


## Create spatially-lagged DV

W.y <- W %*% y
W.y


# Calculate initial Pseudolikelihood Estimates.

psi <-  glm(y ~ x+ W.y, family=binomial(link=logit))$coef
c.cur <- vector("numeric",length(y))


# Define Gibbs sampler

gibbs.generator <- function(y,x,W,psi,gibbs.burnin=100,gibbs.sample=1000,gibbs.interval=50){
   gather <- 1
   k <-1 
   ncov <- dim(x)[2] # X has covariates only
   c.cur <- vector("numeric",length(y))
   suff <- matrix(0,(gibbs.sample/gibbs.interval),(2+ncov))
   dimnames(suff) <- list(NULL, 
      c("c.cur",unlist(dimnames(x)[2]),"W"))
   for (i in 1:(gibbs.burnin+gibbs.sample)){
    for (j in 1: length(c.cur)){
       eta <- (exp(psi[1]+sum(psi[(length(psi)-1)]*x[j,])+psi[length(psi)]*(W%*%c.cur)[j])) 
        phat <- eta/(1+eta)
        if(runif(1,0,1) < phat){
	   c.cur[j] <- 1
        } else {c.cur[j] <- 0}
      } 
      if (i == gather+gibbs.burnin) {  
	suff[k,1] <- sum(c.cur)
        for (m in 1:ncov){	 
	    suff[k,(m+1)] <- sum(c.cur*x[,m])
        }
	suff[k,dim(suff)[2]] <- 0.5*sum(W%*%c.cur)
	gather <- gather + gibbs.interval 
	k <- k + 1 
      } 
      cat("Gibbs sample is :",i," of ",gibbs.sample,"\n") 
   } 
   return(list(suff = suff, psi= psi, c.cur = c.cur))
}


# Calculate the observed vector of s(y)

gibsout <- gibbs.generator(y=y,x=x,W=W,psi=psi,
  gibbs.burnin=100,gibbs.sample=1000,gibbs.interval=2)
gibsout



## Define sufficient statistics from observed data

tobs <- vector("numeric",length=(dim(x)[2]+2))
tobs[1] <- sum(y)
for (i in 1:dim(x)[2]){
 tobs[(i+1)] <- sum(x[,i]*y)
}
tobs[length(tobs)] <- 0.5*sum(W%*%y)


# Define Newton-Raphson algorithm

newton.raphson2<- function(xobs, xsim, psi, start.psi = psi, maxiter = 30, eps1 = 1e-15, eps2 = 1e-08,
	look = F, do.mc.se = T, lag.max = round(nrow(xsim)^0.5))
{
	iter <- 0
	nxt <- start.psi - psi
	av <- apply(xsim, 2, mean)
	xobs <- xobs - av
	xsim <- sweep(xsim, 2, av)
	ll <- 0
	repeat {
		iter <- iter + 1
		cur <- nxt
		prob <- exp(xsim %*% cur)
		prob <- prob/sum(prob)
		E <- apply(sweep(xsim, 1, prob, "*"), 2, sum)
		vtmp <- sweep(sweep(xsim, 2, E, "-"), 1, prob^0.5, "*")
		V <- t(vtmp) %*% vtmp
		nxt <- cur + (delt <- solve(V, xobs - E))
		ll.old <- ll
		repeat {
			ll <- sum(xobs * nxt) - log(mean(exp(xsim %*% nxt)))
			if(ll > ll.old - eps1)
				break
			else nxt <- cur + (delt <- delt/2)
		}
		if(look)
			print(cur + psi)
		if((abs(ll - ll.old) < eps1) || max(abs(delt/cur)) < eps2 || (
			iter >= maxiter))
			break
	}
	loglik <- ll.old
	cur <- nxt
	if(do.mc.se) {
		prob <- as.vector(exp(xsim %*% cur))
		prob <- prob/sum(prob)
		n <- length(prob)
		z <- sweep(xsim, 2, xobs, "-") * prob * n
		R <- acf(z, lag.max = lag.max, type = "covariance", plot = F)$
			acf
		part <- apply(R[-1,  ,  ], c(2, 3), sum)
		cov.zbar <- (R[1,  ,  ] + part + t(part))/n
		mc.se <- diag(solve(V, t(solve(V, cov.zbar))))^0.5
	}
	if(do.mc.se)
		list(theta = cur + psi, se = diag(solve(V))^0.5, mc.se = mc.se, 
			psi = psi, iter = iter, loglik = loglik, E = E + av, V
			 = V, cov.zbar = cov.zbar)
	else list(theta = cur + psi, se = diag(solve(V))^0.5, psi = psi, iter
			 = iter, loglik = loglik, E = E + av, V = V)
}


## Run Newton-Raphson

a <- newton.raphson2(xobs=tobs,xsim=gibsout$suff,psi=psi)


## Pull theta estimates 
theta88 <- as.matrix(a$theta)


## Model Comparison

psi.full <- glm(y ~ x+ W.y, family=binomial(link=logit))

compare <- cbind(
summary(psi.full)$coefficients[,1],
summary(psi.full)$coefficients[,2],
unlist(a[1]),
unlist(a[3])
)
colnames(compare) <- c("MPL.Coef","MPL.SE","MCMC.Coef","MCMC.SE")
compare



###
#applied spatial data analysis using R的附带文件
ASDAR_BOOK <- "http://www.asdar-book.org/book2ed"
chapters <- c("hello", "cm", "vis", "die", "cm2",
              "std", "sppa", "geos", "lat", "dismap")
# setwd() # move to download folder
for (i in chapters) {
  fn <- paste(i, "mod.R", sep="_")
  download.file(paste(ASDAR_BOOK, fn, sep = "/"), fn)
}
list.files()
