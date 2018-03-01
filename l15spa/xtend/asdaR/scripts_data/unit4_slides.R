###################################################
library(rgdal)
BMcD <- readOGR(".", "BMcD")
BMcD$Fldf <- factor(BMcD$Fldf)
names(BMcD)
proj4string(BMcD) <- CRS("+init=epsg:28992")


###################################################
bubble(BMcD, "Zn")


###################################################
boxplot(Zn ~ Fldf, BMcD, width=table(BMcD$Fldf), col="grey")


###################################################
oopar <- par(mfrow=c(4,1))
plot(density(BMcD$Zn), main="", xlim=c(0,2000), lwd=2)
tull <- by(as(BMcD, "data.frame"), BMcD$Fldf, function(x) plot(density(x$Zn), main="", xlim=c(0,2000), lwd=2))
par(oopar)


###################################################
BMcD_grid <- as(readGDAL("BMcD_fldf.txt"), "SpatialPixelsDataFrame")
names(BMcD_grid) <- "Fldf"
BMcD_grid$Fldf <- as.factor(BMcD_grid$Fldf)
proj4string(BMcD_grid) <- CRS("+init=epsg:28992")


###################################################
pts = list("sp.points", BMcD, pch = 4, col = "white")
spplot(BMcD_grid, "Fldf", col.regions=1:3, sp.layout=list(pts))


###################################################
crds <- coordinates(BMcD)
poly <- crds[chull(crds),]
poly <- rbind(poly, poly[1,])
SPpoly <- SpatialPolygons(list(Polygons(list(Polygon(poly)), ID="poly")))
bbox(BMcD)
(apply(bbox(BMcD), 1, diff) %/% 50) + 1
grd <- GridTopology(c(178600, 330300), c(50,50), c(48, 41))
SG <- SpatialGrid(grd)
inside <- overlay(SG, SPpoly)
SGDF <- SpatialGridDataFrame(grd, data=data.frame(list(ins=inside)))
SPDF <- as(SGDF, "SpatialPixelsDataFrame")


###################################################
plot(BMcD, axes=TRUE)
plot(SPpoly, add=TRUE)
plot(SPDF, col="red", add=TRUE)


###################################################
bluepal <- colorRampPalette(c("azure1", "steelblue4"))
brks <- c(0,130,155,195,250,330,450,630,890,1270,1850)
cols <- bluepal(length(brks)-1)
sepal <- colorRampPalette(c("peachpuff1", "tomato3"))
brks.se <- c(0,240,250,260,270,280,290,300,350,400,1000)
cols.se <- sepal(length(brks.se)-1)
scols <- c("green", "red")


###################################################
library(ipred)
res <- errorest(Zn ~ 1, data = as(BMcD, "data.frame"), model=lm, est.para=control.errorest(k=nrow(BMcD), random=FALSE, predictions=TRUE))
round(res$error, 2)
fres <- lm(Zn ~ Fldf, data=BMcD)
anova(fres)
eres <- errorest(Zn ~ Fldf, data = as(BMcD, "data.frame"), model=lm, est.para=control.errorest(k=nrow(BMcD), random=FALSE, predictions=TRUE))
round(eres$error, 2)


###################################################
library(maptools)
BMcD_grid$lm_pred <- predict(fres, newdata=BMcD_grid)
image(BMcD_grid, "lm_pred", breaks=brks, col=cols)
title("Flood frequency model interpolation")
pe <- BMcD$Zn-eres$predictions
symbols(coordinates(BMcD), circles=sqrt(abs(pe)), fg="black", bg=scols[(pe < 0)+1], inches=FALSE, add=TRUE)
legend("topleft", fill=cols, legend=leglabs(brks), bty="n", cex=0.8)


###################################################
library(fields)
pe_tps <- numeric(nrow(BMcD))
cBMcD <- coordinates(BMcD)
for (i in seq(along=pe_tps)) {
  tpsi <- Tps(cBMcD[-i,], BMcD$Zn[-i])
  pri <- predict(tpsi, cBMcD[i,,drop=FALSE])
  pe_tps[i] <- BMcD$Zn[i]-pri
}
round(sqrt(mean(pe_tps^2)), 2)
tps <- Tps(coordinates(BMcD), BMcD$Zn)


###################################################
BMcD_grid$spl_pred <- predict(tps, coordinates(BMcD_grid))
image(BMcD_grid, "spl_pred", breaks=brks, col=cols)
title("Thin plate spline model")
symbols(coordinates(BMcD), circles=sqrt(abs(pe_tps)), fg="black", bg=scols[(pe_tps < 0)+1], inches=FALSE, add=TRUE)
legend("topleft", fill=cols, legend=leglabs(brks), bty="n", cex=0.8)


###################################################
library(gstat)
cvgm <- variogram(Zn~1, data=BMcD, width=100, cutoff=1000)
efitted <- fit.variogram(cvgm, vgm(psill=1, model="Exp", range=100, nugget=1))
efitted


###################################################
plot(cvgm, model=efitted, plot.numbers=TRUE, col="black")


###################################################
OK_fit <- gstat(id="OK_fit", formula = Zn ~ 1, data = BMcD, model=efitted)
pe <- gstat.cv(OK_fit, debug.level=0, random=FALSE)$residual
round(sqrt(mean(pe^2)), 2)
z <- predict(OK_fit, newdata=BMcD_grid, debug.level=0)
BMcD_grid$OK_pred <- z$OK_fit.pred
BMcD_grid$OK_se <- sqrt(z$OK_fit.var)


###################################################
image(BMcD_grid, "OK_pred", breaks=brks, col=cols)
title("Fitted exponential OK model")
symbols(coordinates(BMcD), circles=sqrt(abs(pe)), fg="black", bg=scols[(pe < 0)+1], inches=FALSE, add=TRUE)
legend("topleft", fill=cols, legend=leglabs(brks), bty="n", cex=0.8)


###################################################
image(BMcD_grid, "OK_se", breaks=brks.se, col=cols.se)
title("Fitted exponential OK standard errors")
symbols(coordinates(BMcD), circles=sqrt(abs(pe)), fg="black", bg=scols[(pe < 0)+1], inches=FALSE, add=TRUE)
legend("topleft", fill=cols.se, legend=leglabs(brks.se), bty="n", cex=0.8)


###################################################
cvgm <- variogram(Zn~Fldf, data=BMcD, width=100, cutoff=1000)
uefitted <- fit.variogram(cvgm, vgm(psill=1, model="Exp", range=100, nugget=1))
uefitted


###################################################
plot(cvgm, model=uefitted, plot.numbers=TRUE, col="black")


###################################################
UK_fit <- gstat(id="UK_fit", formula = Zn ~ Fldf, data = BMcD, model=uefitted)
pe_UK <- gstat.cv(UK_fit, debug.level=0, random=FALSE)$residual
round(sqrt(mean(pe_UK^2)), 2)
z <- predict(UK_fit, newdata=BMcD_grid, debug.level=0)
BMcD_grid$UK_pred <- z$UK_fit.pred
BMcD_grid$UK_se <- sqrt(z$UK_fit.var)


###################################################
image(BMcD_grid, "UK_pred", breaks=brks, col=cols)
title("Flood frequency UK model")
symbols(coordinates(BMcD), circles=sqrt(abs(pe_UK)), fg="black", bg=scols[(pe_UK < 0)+1], inches=FALSE, add=TRUE)
legend("topleft", fill=cols, legend=leglabs(brks), bty="n", cex=0.8)


###################################################
image(BMcD_grid, "UK_se", breaks=brks.se, col=cols.se)
title("Flood frequency UK interpolation standard errors")
symbols(coordinates(BMcD), circles=sqrt(abs(pe_UK)), fg="black", bg=scols[(pe_UK < 0)+1], inches=FALSE, add=TRUE)
legend("topleft", fill=cols.se, legend=leglabs(brks.se), bty="n", cex=0.8)


###################################################
pts = list("sp.points", BMcD, pch = 4, col = "black", cex=0.5)
spplot(BMcD_grid, c("lm_pred", "spl_pred", "OK_pred", "UK_pred"), at=brks, col.regions=cols, sp.layout=list(pts))


###################################################
writeGDAL(BMcD_grid["UK_pred"], "UK_pred.tif")


###################################################
library(maptools)
grd <- as.SpatialPolygons.SpatialPixels(BMcD_grid)
proj4string(grd) <- CRS(proj4string(BMcD))
grd.union <- unionSpatialPolygons(grd, rep("x", length(slot(grd, "polygons"))))
grd.union.ll <- spTransform(grd.union, CRS("+proj=longlat"))
llGRD <- GE_SpatialGrid(grd.union.ll, maxPixels = 100)
llGRD_in <- overlay(llGRD$SG, grd.union.ll)
llSPix <- as(SpatialGridDataFrame(grid=slot(llGRD$SG, "grid"), proj4string=CRS(proj4string(llGRD$SG)), data=data.frame(in0=llGRD_in)), "SpatialPixelsDataFrame")
SPix <- spTransform(llSPix, CRS("+init=epsg:28992"))
z <- predict(OK_fit, newdata=SPix, debug.level=0)
llSPix$pred <- z$OK_fit.pred
png(file="zinc_OK.png", width=llGRD$width, height=llGRD$height, bg="transparent")
par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
image(llSPix, "pred", col=bpy.colors(20))
dev.off()
kmlOverlay(llGRD, "zinc_OK.kml", "zinc_OK.png")


