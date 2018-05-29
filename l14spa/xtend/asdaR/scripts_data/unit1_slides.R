###################################################
library(sp)
data(meuse)
coords <- SpatialPoints(meuse[, c("x", "y")])
summary(coords)


###################################################
meuse1 <- SpatialPointsDataFrame(coords, meuse)
names(meuse1)
summary(meuse1$zinc)
stem(meuse1$zinc, scale=1/2)


###################################################
data(meuse.riv)
str(meuse.riv)
river_polygon <- Polygons(list(Polygon(meuse.riv)), ID="meuse")
rivers <- SpatialPolygons(list(river_polygon))
summary(rivers)


###################################################
library(maptools)


###################################################
volcano_sl <- ContourLines2SLDF(contourLines(volcano))
row.names(slot(volcano_sl, "data"))
sapply(slot(volcano_sl, "lines"), function(x) slot(x, "ID"))
sapply(slot(volcano_sl, "lines"), function(x) length(slot(x, "Lines")))
volcano_sl$level


###################################################
data(meuse.grid)
coords <- SpatialPixels(SpatialPoints(meuse.grid[, c("x", "y")]))
meuseg1 <- SpatialPixelsDataFrame(coords, meuse.grid)
names(meuseg1)
slot(meuseg1, "grid")
object.size(meuseg1)
dim(slot(meuseg1, "data"))


###################################################
meuseg2 <- meuseg1
fullgrid(meuseg2) <- TRUE
slot(meuseg2, "grid")
class(slot(meuseg2, "grid"))
object.size(meuseg2)
dim(slot(meuseg2, "data"))


