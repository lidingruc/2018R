### R code from vignette source 'hello.Rnw'
### Encoding: UTF-8
# Data: data1964al.xy
###################################################
### code chunk number 8: hello.Rnw:394-397
###################################################
library(maptools)
library(maps)
library(rgdal)
grey_gamma <- 2.2


###################################################
### code chunk number 9: hello.Rnw:402-436
###################################################
volc.tab = read.table("data1964al.xy")
volc = SpatialPoints(volc.tab[c(2,1)])
llCRS <- CRS("+proj=longlat +ellps=WGS84")
proj4string(volc) <- llCRS
prj_new = CRS("+proj=moll +ellps=WGS84")
volc_proj = spTransform(volc, prj_new)
 wrld <- map("world", interior=FALSE, xlim=c(-179,179), ylim=c(-89,89),
 plot=FALSE)
 wrld_p <- pruneMap(wrld, xlim=c(-179,179))
 wrld_sp <- map2SpatialLines(wrld_p, proj4string=llCRS)
 wrld_proj <- spTransform(wrld_sp, prj_new)
 #save(c("wrld_proj", "wrld_sp"), file = "hsd_data/wrld.RData")
 #load("hsd_data/wrld.RData")
wrld_grd <- gridlines(wrld_sp, easts=c(-179,seq(-150,150,50),179.5), 
  norths=seq(-75,75,15), ndiscr=100)
wrld_grd_proj <- spTransform(wrld_grd, prj_new)
at_sp <- gridat(wrld_sp, easts=0, norths=seq(-75,75,15), offset=0.3)
at_proj <- spTransform(at_sp, prj_new)
plot(wrld_proj, col="grey50")
plot(wrld_grd_proj, add=TRUE, lty=3, col="grey50")
points(volc_proj, cex = .8, pch = 3, col = "blue")
text(coordinates(at_proj), pos=at_proj$pos, offset=at_proj$offset, 
  labels=parse(text=as.character(at_proj$labels)), cex=0.6)


###################################################
### code chunk number 10: hello.Rnw:573-610
###################################################
library(sp)
data(volcano)
grys <- grey.colors(8, 0.55, 0.95, grey_gamma)
layout(matrix(c(1,2,1,3,1,4),3,2,byrow=TRUE), c(3,1))
image(volcano, axes=FALSE, col=grys, asp=1, main="a")
contour(volcano, add=TRUE)
box()
image(volcano, axes=FALSE, col='white', asp=1, main="b")
# b:
library(maptools)
x2 = ContourLines2SLDF(contourLines(volcano))
plot(x2, add=TRUE)
# c:
box()
image(volcano, axes=FALSE, col='white', asp=1, main="c")
plot(x2[x2$level == 140,], add=TRUE)
# d:
box()
image(volcano, axes=FALSE, col=grys, asp=1, main="d")
x3l1 = coordinates(x2[x2$level == 160,])[[1]][[1]]
x3l2 = coordinates(x2[x2$level == 160,])[[1]][[2]]
x3 = SpatialPolygons(list(Polygons(list(Polygon(x3l1,hole=FALSE), 
    Polygon(x3l2,hole=TRUE)), ID=c("x"))))
plot(x3, col = '#FF8800', add = TRUE)
box()

