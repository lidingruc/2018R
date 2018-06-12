# R script demonstrating creation of maps in which vector layers are
# superimposed atop a raster base layer.
#
# Two approaches:
#  1. Plot using spplot in the 'sp' package (depends on 'lattice')
#  2. Plot using plot as extended by the 'raster' package, and base
#     graphics as extened by 'sp'
#
# Authors: Jim Regetz & Rick Reeves
# Originally created: September 7, 2007 [reeves]
# Last modified: 13-Dec-2011 [regetz]
# National Center for Ecological Analysis and Synthesis (NCEAS),
# http://www.nceas.ucsb.edu/scicomp

# change to the directory containing map data
setwd("data")

# ---------- sp and lattice---------- #

library(sp)
library(rgdal)
library(maptools)

# read in the counties and their centroids
centroids <- readShapePoints("op-county-centroids")
counties <- readShapePoly("op-counties")

# read raster in as a SpatialGridDataFrame object
psImg <- readGDAL("op-landsat.img")

# specify overplot layers for use by spplot; in order for polygons to be
# plotted atop the raster, we need to convert them to SpatialLines
polys <- list("sp.lines", as(counties, "SpatialLines"), col="white")
points <- list("sp.points", centroids, col="lightblue", pch=1)
labels <- list("panel.text",
    coordinates(centroids)[,1], coordinates(centroids)[,2],
    labels=sub(" County", "", centroids$COUNTY),
    col="lightblue", font=2, pos=2)

# plot the raster with polygons, points, and labels
png("map-olympic-spplot.png", height=600, width=600)
spplot(psImg, "band1", col.regions=grey(0:256/256),
    sp.layout=list(points, labels, polys), cuts=256,
    colorkey=FALSE, scales=list(draw=TRUE),
    main="Olympic Peninsula, WA",
    legend=list(right=list(fun=mapLegendGrob(layout.north.arrow()))))
dev.off()


# ---------- sp and raster with base graphics---------- #

library(raster)

# read in the counties and their centroids
centroids <- readShapePoints("op-county-centroids")
counties <- readShapePoly("op-counties")

# read raster in as a Raster object
ps <- raster("op-landsat.img")

# plot the raster, then polygons, points, and labels
png("map-olympic-raster.png", height=600, width=600)
plot(ps, col=grey(0:256/256), legend=FALSE,
    main="Olympic Peninsula, WA")
par(bg="transparent")
plot(counties, border="white", col="transparent", add=TRUE)
points(centroids, col="lightblue")
text(centroids, labels=sub(" County", "", centroids$COUNTY),
    col="lightblue", font=2, pos=2)
dev.off()
