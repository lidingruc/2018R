###################################################
oopar <- par(mar=c(3,3,1,1)+0.1)
library(maptools)
library(maps)
ill <- map("county", regions="illinois", plot=FALSE, fill=TRUE)
IDs <- sub("^illinois,", "", ill$names)
ill_sp <- map2SpatialPolygons(ill, IDs, CRS("+proj=longlat"))
plot(ill_sp, axes=TRUE)
par(oopar)


###################################################
library(rgdal)


###################################################
ED50 <- CRS(paste("+init=epsg:4230", "+towgs84=-87,-96,-120,0,0,0,0"))
IJ.east <- as(char2dms("4d31\'00\"E"), "numeric")
IJ.north <- as(char2dms("52d28\'00\"N"), "numeric")
IJ.ED50 <- SpatialPoints(cbind(x=IJ.east, y=IJ.north), ED50)
res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat=TRUE)*1000


###################################################
EPSG <- make_EPSG()
EPSG[grep("Amersfoort", EPSG$note), 1:2]
RD_New <- CRS("+init=epsg:28992")
res <- CRSargs(RD_New)
cat(strwrap(res), sep="\n")
res <- showWKT(CRSargs(RD_New), morphToESRI = TRUE)
cat(strwrap(gsub(",", ", ", res)), sep="\n")


###################################################
library(maptools)
list.files()
getinfo.shape("scot_BNG.shp")
scot <- readShapePoly("scot_BNG.shp")


###################################################
scot1 <- readOGR(dsn=".", layer="scot_BNG")
cat(strwrap(proj4string(scot1)), sep="\n")


###################################################
getGDALDriverNames()$name
list.files()
SP27GTIF <- readGDAL("SP27GTIF.TIF")


###################################################
oopar <- par(mar=c(3,3,1,1)+0.1)
image(SP27GTIF, col=grey(1:99/100), axes=TRUE)
par(oopar)


###################################################
summary(SP27GTIF)



