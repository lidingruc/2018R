### R code from vignette source 'cm2.Rnw'
### Encoding: UTF-8
# olinda1 L7_ETM8s.tif L7_ETMs.tif olinda_dem_utm25s.tif stream
###################################################
### code chunk number 8: cm2.Rnw:45-46
###################################################
require(lattice)


###################################################
### code chunk number 9: cm2.Rnw:133-148
###################################################
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
grys <- terrain.colors(11)
image(y, x, t(volcano)[ncol(volcano):1,], breaks=seq(90,200,10), col=grys, asp=1, axes=FALSE)
contour(y, x, t(volcano)[ncol(volcano):1,], levels=seq(90,200,10), asp=1, axes=FALSE)


###################################################
### code chunk number 10: cm2.Rnw:269-270
###################################################
library(rgeos)


###################################################
### code chunk number 13: cm2.Rnw:321-322
###################################################
getScale()


###################################################
### code chunk number 14: cm2.Rnw:359-363 (eval = FALSE)
###################################################
## library(testthat)
## set_do_poly_check(FALSE)
## test_package("rgeos")
## set_do_poly_check(TRUE)


###################################################
### code chunk number 15: cm2.Rnw:400-403
###################################################
library(rgdal)
set_ReplCRS_warn(FALSE)


###################################################
### code chunk number 16: cm2.Rnw:406-409
###################################################
olinda <- readOGR("olinda1.shp", "olinda1", integer64="allow.loss")
proj4string(olinda) <- CRS("+init=epsg:4674")
olinda_utm <- spTransform(olinda, CRS("+init=epsg:31985"))


###################################################
### code chunk number 17: cm2.Rnw:412-413
###################################################
set_ReplCRS_warn(TRUE)


###################################################
### code chunk number 18: cm2.Rnw:457-461
###################################################
Area <- gArea(olinda_utm, byid=TRUE)
olinda_utm$area <- sapply(slot(olinda_utm, "polygons"), slot, "area")
all.equal(unname(Area), olinda_utm$area)
olinda_utm$dens <- olinda_utm$V014/(olinda_utm$area/1000000)


###################################################
### code chunk number 19: cm2.Rnw:480-490
###################################################
library(RColorBrewer)
spplot(olinda_utm, "dens", at=c(0, 8000, 12000, 15000, 20000, 60000), col.regions=brewer.pal(6, "YlOrBr")[-1], col="grey30", lwd=0.5, colorkey=list(space="right", labels=list(cex=0.7), width=1))


###################################################
### code chunk number 20: cm2.Rnw:518-521
###################################################
bounds <- gUnaryUnion(olinda_utm)
gArea(olinda_utm)
sapply(slot(slot(bounds, "polygons")[[1]], "Polygons"), slot, "area")


###################################################
### code chunk number 21: cm2.Rnw:542-544
###################################################
pols_overlap <- gOverlaps(olinda_utm, byid=TRUE)
any(pols_overlap)


###################################################
### code chunk number 22: cm2.Rnw:566-573
###################################################
oScale <- getScale()
setScale(1e+4)
pols_overlap <- gOverlaps(olinda_utm, byid=TRUE)
any(pols_overlap)
bounds <- gUnaryUnion(olinda_utm)
setScale(oScale)
sapply(slot(slot(bounds, "polygons")[[1]], "Polygons"), slot, "area")


###################################################
### code chunk number 23: cm2.Rnw:592-593
###################################################
set_ReplCRS_warn(FALSE)


###################################################
### code chunk number 24: cm2.Rnw:596-604
###################################################
pan <- readGDAL("L7_ETM8s.tif")
proj4string(pan) <- CRS(proj4string(bounds))
TM <- readGDAL("L7_ETMs.tif")
proj4string(TM) <- CRS(proj4string(bounds))
names(TM) <- c("TM1", "TM2", "TM3", "TM4", "TM5", "TM7")
dem <- readGDAL("olinda_dem_utm25s.tif")
proj4string(dem) <- CRS(proj4string(bounds))
is.na(dem$band1) <- dem$band1 <= 0


###################################################
### code chunk number 25: cm2.Rnw:607-609
###################################################
set_ReplCRS_warn(TRUE)


###################################################
### code chunk number 26: cm2.Rnw:628-635 (eval = FALSE)
###################################################
## library(spgrass6)
## myGRASS <- "/home/rsb/topics/grass/g642/grass-6.4.2"
## loc <- initGRASS(myGRASS, tempdir(), SG=dem, override=TRUE)
## execGRASS("g.mapset", mapset="PERMANENT")
## execGRASS("g.proj", flag="c", proj4=proj4string(bounds))
## execGRASS("g.mapset", mapset=loc$MAPSET)
## execGRASS("g.region", flag="d")


###################################################
### code chunk number 27: cm2.Rnw:651-656 (eval = FALSE)
###################################################
## writeRAST6(dem, "dem", flags="o")
## execGRASS("g.region", rast="dem")
## respan <- gridparameters(pan)$cellsize
## execGRASS("r.resamp.rst", input="dem", ew_res=respan[1], ns_res=respan[2], elev="DEM_resamp")
## execGRASS("g.region", rast="DEM_resamp")


###################################################
### code chunk number 28: cm2.Rnw:674-677 (eval = FALSE)
###################################################
## execGRASS("r.watershed", elevation="DEM_resamp", stream="stream", threshold=1000L, convergence=5L, memory=300L)
## execGRASS("r.thin", input="stream", output="stream1", iterations=200L)
## execGRASS("r.to.vect", input="stream1", output="stream", feature="line")


###################################################
### code chunk number 29: cm2.Rnw:691-692
###################################################
set_ReplCRS_warn(FALSE)


###################################################
### code chunk number 30: cm2.Rnw:695-696 (eval = FALSE)
###################################################
## stream_utm <- readVECT6("stream")


###################################################
### code chunk number 31: cm2.Rnw:698-699
###################################################
stream_utm <- readOGR("stream.shp", "stream")


###################################################
### code chunk number 32: cm2.Rnw:701-702
###################################################
proj4string(stream_utm) <- CRS("+init=epsg:31985")


###################################################
### code chunk number 33: cm2.Rnw:704-705
###################################################
set_ReplCRS_warn(TRUE)


###################################################
### code chunk number 34: cm2.Rnw:707-709
###################################################
nrow(stream_utm)
summary(gLength(stream_utm, byid=TRUE))


###################################################
### code chunk number 35: cm2.Rnw:726-728
###################################################
t0 <- gTouches(stream_utm, byid=TRUE)
any(t0)


###################################################
### code chunk number 36: cm2.Rnw:744-745
###################################################
library(spdep)


###################################################
### code chunk number 37: cm2.Rnw:747-750
###################################################
lw <- mat2listw(t0)
nComp <- n.comp.nb(lw$neighbours)
nComp$nc


###################################################
### code chunk number 38: cm2.Rnw:768-772
###################################################
lns <- gLineMerge(stream_utm, id=as.character(nComp$comp.id))
length(row.names(lns))
summary(gLength(lns, byid=TRUE))
all.equal(SpatialLinesLengths(lns), unname(gLength(lns, byid=TRUE)))


###################################################
### code chunk number 39: cm2.Rnw:791-794
###################################################
GI <- gIntersection(lns, olinda_utm, byid=TRUE)
class(GI)
length(row.names(GI))


###################################################
### code chunk number 40: cm2.Rnw:817-827
###################################################
res <- numeric(nrow(olinda_utm))
head(row.names(GI))
range(as.integer(row.names(olinda_utm)))
rnGI <- as.integer(sapply(strsplit(row.names(GI), " "), "[", 2))+1
length(rnGI) == length(unique(rnGI))
lens <- gLength(GI, byid=TRUE)
tlens <- tapply(lens, rnGI, sum)
res[as.integer(names(tlens))] <- unname(tlens)
olinda_utm$stream_len <- res
summary(olinda_utm$stream_len)


###################################################
### code chunk number 41: cm2.Rnw:846-848
###################################################
tree <- gBinarySTRtreeQuery(lns, olinda_utm)
table(sapply(tree, length))


###################################################
### code chunk number 42: cm2.Rnw:872-880
###################################################
res1 <- numeric(length=length(tree))
for (i in seq(along=res1)) {
  if (!is.null(tree[[i]])) {
    gi <- gIntersection(lns[tree[[i]]], olinda_utm[i,])
    res1[i] <- ifelse(is.null(gi), 0, gLength(gi))
  }
}
all.equal(olinda_utm$stream_len, res1)


###################################################
### code chunk number 43: cm2.Rnw:899-901
###################################################
buf50m <- gBuffer(lns, width=50)
length(slot(buf50m, "polygons"))


###################################################
### code chunk number 44: cm2.Rnw:918-926
###################################################
GI1 <- gIntersection(buf50m, olinda_utm, byid=TRUE)
res <- numeric(length(slot(olinda_utm, "polygons")))
head(row.names(GI1))
rnGI <- as.integer(sapply(strsplit(row.names(GI1), " "), "[", 2))+1
length(rnGI) == length(unique(rnGI))
res[rnGI] <- gArea(GI1, byid=TRUE)
olinda_utm$buf_area <- res
olinda_utm$prop_50m <- olinda_utm$buf_area/olinda_utm$area


###################################################
### code chunk number 45: cm2.Rnw:942-943
###################################################
stream_inside <- gIntersection(lns, bounds)


###################################################
### code chunk number 46: cm2.Rnw:960-970
###################################################
library(RColorBrewer)
bl <- brewer.pal(5, "Blues")
spplot(olinda_utm, "prop_50m", col.regions=colorRampPalette(bl)(20), col="transparent", sp.layout=list("sp.lines", stream_inside), colorkey=list(space="right", labels=list(cex=0.7), width=1))


###################################################
### code chunk number 49: cm2.Rnw:1059-1063
###################################################
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y


###################################################
### code chunk number 50: cm2.Rnw:1066-1068
###################################################
sel = over(meuse, as(meuse.grid, "SpatialPixels"))
meuse = meuse[!is.na(sel),]


###################################################
### code chunk number 51: cm2.Rnw:1078-1079
###################################################
meuse = meuse[meuse.grid,]


###################################################
### code chunk number 52: cm2.Rnw:1118-1121
###################################################
gt <- GridTopology(c(178480, 329640), c(400,400), c(8,11))
coarseGrid <- SpatialGrid(gt, proj4string(meuse))
agg <- aggregate(meuse[c("zinc", "lead")], coarseGrid, max)


###################################################
### code chunk number 53: cm2.Rnw:1134-1144
###################################################
pts = list("sp.points", meuse, col='black')
require(RColorBrewer)
pal = function(n = 9) brewer.pal(n, "Reds")
spplot(agg, sp.layout = pts, col.regions = pal, cuts = 8)


###################################################
### code chunk number 54: cm2.Rnw:1176-1177
###################################################
TM$ndvi <- (TM$TM4 - TM$TM3)/(TM$TM4 + TM$TM3)


###################################################
### code chunk number 55: cm2.Rnw:1189-1196
###################################################
TM0 <- as(TM, "SpatialPixelsDataFrame")
TM1 <- TM0[bounds, ]
PC <- prcomp(as(TM1, "data.frame")[,1:6], center=TRUE, scale.=TRUE)
PCout <- predict(PC)
TM1$PC1 <- PCout[,1]
TM1$PC2 <- PCout[,2]
TM1$PC3 <- PCout[,3]


###################################################
### code chunk number 56: cm2.Rnw:1202-1210
###################################################
spplot(TM1, c("PC1", "PC2"), at=seq(-17, 17, length.out=21), col.regions=rev(colorRampPalette(brewer.pal(10, "PiYG"))(20)), sp.layout=list("sp.lines", as(olinda_utm, "SpatialLines"), lwd=0.5), colorkey=list(space="right", labels=list(cex=0.8), width=1))


###################################################
### code chunk number 57: cm2.Rnw:1226-1230
###################################################
o_mean <- over(olinda_utm, TM1[,c("PC1", "PC2", "PC3")])
str(o_mean)
row.names(o_mean) <- row.names(olinda_utm)
library(maptools)
olinda_utmA <- spCbind(olinda_utm, o_mean)



###################################################
### code chunk number 58: cm2.Rnw:1241-1248
###################################################
o_median <- over(olinda_utm, TM1[,c("PC1", "PC2", "PC3")], fn=median)
row.names(o_median) <- row.names(olinda_utmA)
names(o_median) <- paste(names(o_median), "med", sep="_")
olinda_utmB <- spCbind(olinda_utmA, o_median)
TM1$count <- 1
o_count <- over(olinda_utm, TM1[,"count"], fn=sum)
olinda_utmB$count <- o_count$count


###################################################
### code chunk number 59: cm2.Rnw:1259-1260
###################################################
summary(olinda_utmB[, grep("^PC|count", names(olinda_utmB))])


###################################################
### code chunk number 60: cm2.Rnw:1272-1277
###################################################
o_dem_median <- over(olinda_utm, dem, fn=median)
olinda_utmB$dem_median <- o_dem_median$band1
summary(olinda_utmB$dem_median)
o_ndvi_median <- over(olinda_utm, TM1["ndvi"], fn=median)
olinda_utmB$ndvi_median <- o_ndvi_median$ndvi


###################################################
### code chunk number 61: cm2.Rnw:1290-1291
###################################################
library(raster)


###################################################
### code chunk number 64: cm2.Rnw:1314-1316 (eval = FALSE)
###################################################
TMrs <- stack(TM1)
e1 <- extract(TMrs, as(olinda_utm, "SpatialPolygons"), small=FALSE)


###################################################
### code chunk number 65: cm2.Rnw:1328-1329 (eval = FALSE)
###################################################
e2 <- extract(raster(dem), as(olinda_utm, "SpatialPolygons"), small=FALSE)


###################################################
### code chunk number 66: cm2.Rnw:1331-1332
###################################################
table(sapply(e2, is.null))


###################################################
### code chunk number 68: cm2.Rnw:1344-1348
###################################################
all.equal(sapply(e1, nrow), olinda_utmB$count)
all.equal(sapply(e1, function(x) median(x[,"ndvi"])), olinda_utmB$ndvi_median)
med <- sapply(e2, function(x) ifelse(is.null(x), as.numeric(NA), median(x, na.rm=TRUE)))
all.equal(med, olinda_utmB$dem_median)


###################################################
### code chunk number 69: cm2.Rnw:1396-1407
###################################################
#o <- over(dem, bounds)
#dem$band1 <- dem$band1*o
set.seed(9876)
p_r <- spsample(bounds, 1000, type="random")
length(p_r)
dem <- dem[bounds,]
dem_sp <- as(dem, "SpatialPixelsDataFrame")
g_r <- spsample(dem_sp, 1000, type="random")
length(g_r)
g_rg <- spsample(dem_sp, 1000, type="regular")
length(g_rg)


###################################################
### code chunk number 70: cm2.Rnw:1420-1423
###################################################
p_r_dem <- over(p_r, dem)
g_r_dem <- over(g_r, dem)
g_rg_dem <- over(g_rg, dem)


###################################################
### code chunk number 71: cm2.Rnw:1430-1455
###################################################
plot(bounds)
plot(p_r, add=TRUE, cex=0.15)
title(main="polygon_random")
plot(g_r, cex=0.15)
title(main="grid_random")
plot(g_rg, cex=0.15)
title(main="grid_regular")
plot(ecdf(p_r_dem$band1), verticals=TRUE, do.p=FALSE, ann=FALSE,
 col.hor="green", col.vert="green")
title(main="ECDF")
plot(ecdf(g_r_dem$band1), verticals=TRUE, do.p=FALSE,
 col.hor="blue", col.vert="blue", add=TRUE)
plot(ecdf(g_rg_dem$band1), verticals=TRUE, do.p=FALSE,
 col.hor="red", col.vert="red", add=TRUE)
abline(h=c(0.25,0.5,0.75), lty=2, col="grey")
legend("bottomright", c("polygon random", "grid random", "grid regular"),
 col=c("green", "red", "blue"), lty=1, bty="n")


###################################################
### code chunk number 72: cm2.Rnw:1468-1477
###################################################
tab <- rbind(polygon_random=c(fivenum(p_r_dem$band1),
 nrow(p_r_dem)),
 grid_random=c(fivenum(g_r_dem$band1),
 nrow(g_r_dem)),
 grid_regular=c(fivenum(g_rg_dem$band1),
 nrow(g_rg_dem)))
colnames(tab) <- c("minimum", "lower-hinge", "median", "upper-hinge",
 "maximum", "n")
tab


###################################################
### code chunk number 73: cm2.Rnw:1499-1505
###################################################
o_sp <- as(olinda_utm, "SpatialPolygons")
whichPoly <- over(p_r, o_sp)
whichPoly1 <- gContains(o_sp, p_r, byid=TRUE)
whichPoly1a <- apply(unname(whichPoly1), 1, which)
table(sapply(whichPoly1a, length))
all.equal(whichPoly, whichPoly1a)


###################################################
### code chunk number 74: cm2.Rnw:1542-1547
###################################################
hels <- matrix(c(24.97, 60.17), nrow=1)
p4s <- CRS("+proj=longlat +datum=WGS84")
Hels <- SpatialPoints(hels, proj4string=p4s)
d041224 <- as.POSIXct("2004-12-24", tz="EET")
sunriset(Hels, d041224, direction="sunrise", POSIXct.out=TRUE)



