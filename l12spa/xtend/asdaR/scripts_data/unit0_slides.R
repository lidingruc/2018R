###################################################
library(maptools)
load("JohnSnow.RData")


###################################################
### chunk number 8: 
###################################################
o <- overlay(sohoSG, deaths)
deaths <- spCbind(deaths, as(o, "data.frame"))
deaths$b_nearer <- deaths$snowcost_broad < deaths$snowcost_not_broad
by(deaths$Num_Cases, deaths$b_nearer, sum)


###################################################
oopar <- par(mar=c(1,1,1,1)+0.1)
image(sohoSG, "snowcost_broad", breaks=seq(0,750,50),
 col=rev(heat.colors(15)))
symbols(coordinates(deaths), circles=4*sqrt(deaths$Num_Cases),
 inches=FALSE, add=TRUE, bg=c("pink","grey")[deaths$b_nearer+1])
source("legend_image.R") #from geoR
rect(528900, 180550, 529040, 180990, border=NA, col="white")
text(528970, 180950, "metres from\nBroad Street\npump", cex=0.6)
legend_image(c(528930, 528960), c(180600, 180900),
 sohoSG$snowcost_broad, vertical=TRUE, breaks=seq(0,750,50),
 col=rev(heat.colors(15)))
plot(nb_pump, add=TRUE, pch=8, cex=1.3)
plot(b_pump, add=TRUE, pch=3, cex=1.3, lwd=2)
rect(528900, 181330, 529140, 181380, border=NA, col="white")
legend(c(528910, 529100), c(181350, 181380),
 legend=c("Broad Street pump","other pumps"), pch=c(3,8), bty="n",
 cex=0.6, y.inter=0.7)
rect(528900, 181270, 529180, 181335, border=NA, col="white")
legend(c(528910, 529100), c(181275, 181325),
 legend=c("nearer Broad Street pump","nearer other pump"),
 fill=c("grey","pink"), bty="n", cex=0.6, y.inter=0.7)
par(oopar)


###################################################
library(rgdal)
mt1 <- readOGR("22712073", "METADATA")
DEM <- readGDAL("22712073/22712073.tif")
summary(DEM$band1)
is.na(DEM$band1) <- DEM$band1 <= 0
summary(DEM$band1)


###################################################
oopar <- par(mar=c(3,3,1,1)+0.1)
plot(mt1, axes=TRUE)
image(DEM, col=terrain.colors(40), add=TRUE)
par(oopar)



