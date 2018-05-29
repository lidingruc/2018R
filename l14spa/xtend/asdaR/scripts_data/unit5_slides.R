###################################################
library(rgdal)
drumlins <- readOGR(".", "drumlins")
drumlins_SP <- as(drumlins, "SpatialPoints")


###################################################
library(maptools)
library(spatstat)
drumlins_ppp <- as(drumlins_SP, "ppp")


###################################################
drumlins_ppp


###################################################
bb <- bounding.box(drumlins_ppp)
ch <- convexhull.xy(drumlins_ppp)
rr <- ripras(drumlins_ppp)
drumlins_rr <- ppp(drumlins_ppp$x, drumlins_ppp$y, window=rr)


###################################################
plot(drumlins_ppp)
plot(bb, add=TRUE, border="darkgreen", lwd=2, lty=1)
plot(ch, add=TRUE, border="darkred", lwd=2, lty=3)
plot(rr, add=TRUE, border="orange", lwd=2, lty=2)


###################################################
qc <- quadratcount(drumlins_ppp)


###################################################
cellsize <- mean(diff(attr(qc, "xbreaks")))
cellsize <- c(cellsize, mean(diff(attr(qc, "ybreaks"))))
grd <- GridTopology(c(attr(qc, "xbreaks")[1]+cellsize[1]/2, attr(qc, "ybreaks")[1]+cellsize[2]/2), cellsize, dim(qc))
t3 <- cbind(coordinates(grd), c(qc[,dim(qc)[2]:1]))
plot(as(SpatialGrid(grd), "Spatial"), axes=TRUE)
title(main="Quadrat counts")
plot(drumlins_SP, add=TRUE, cex=0.8)
text(t3[,1], t3[,2], t3[,3], cex=1.2, font=2, col="darkred")
abline(h=attr(qc, "ybreaks"))
abline(v=attr(qc, "xbreaks"))


###################################################
quadrat.test(drumlins_ppp)


###################################################
quadrat.test(drumlins_ppp, nx=6)
quadrat.test(drumlins_rr)


###################################################
xlim <- rr$xrange
ylim <- rr$yrange
m_asp <- (diff(ylim)/diff(xlim))
maxDim <- 100
mywidth <- myheight <- maxDim
if (m_asp < 1) {
    myheight1 <- mywidth * m_asp
    myheight <- ceiling(myheight1)
    cellsize <- c(diff(xlim)/mywidth, diff(ylim)/myheight1)
} else {
    mywidth1 <- myheight/m_asp
    mywidth <- ceiling(mywidth1)
    cellsize <- c(diff(xlim)/mywidth1, diff(ylim)/myheight)
}
cells.dim <- c(mywidth, myheight)
cellcentre.offset <- c(xlim[1] + (0.5 * cellsize[1]), ylim[1] + 
    (0.5 * cellsize[2]))
names(cellcentre.offset) <- c("x", "y")
grd <- GridTopology(cellcentre.offset, cellsize, cells.dim)
crds <- coordinates(grd)
crds <- list(x=crds[,1], y=crds[,2])


###################################################
k025 <- density(drumlins_rr, sigma=0.25, xy=crds)
SG <- as(k025, "SpatialGridDataFrame")
k050 <- density(drumlins_rr, sigma=0.5, xy=crds)
SG <- cbind(SG, as(k050, "SpatialGridDataFrame"))
k075 <- density(drumlins_rr, sigma=0.75, xy=crds)
SG <- cbind(SG, as(k075, "SpatialGridDataFrame"))
k100 <- density(drumlins_rr, sigma=1.0, xy=crds)
SG <- cbind(SG, as(k100, "SpatialGridDataFrame"))
names(SG) <- c("k025", "k050", "k075", "k100")


###################################################
spl <- list("sp.points", drumlins, pch=".", col=1)
spplot(SG, c("k025", "k050", "k075", "k100"), col.regions=terrain.colors(16), cuts=15, sp.layout=list(spl), par.strip.text=list(cex=0.7))


###################################################
library(rgl)
z <- as(SG["k025"], "matrix")
z[is.na(z)] <- 0
x <- 1:nrow(z)
y <- 1:ncol(z)
rgl.bg(color="white")
rgl.clear()
rgl.viewpoint(theta = 350, phi = 35)
rgl.surface(x, y, z)


###################################################
summary(as(SG, "data.frame")[,1:4])


###################################################
nns <- nndist(drumlins_rr)
summary(nns)


###################################################
plot(ecdf(nns))


###################################################
plot(ecdf(nns), xlim=c(0, 0.5))
plot(Gest(drumlins_ppp), add=TRUE, lwd=3)


###################################################
n <- drumlins_rr$n
nsim <- 29
ex <- expression(runifpoint(n, win=rr))
res <- envelope(drumlins_rr, Gest, nsim=nsim, simulate=ex, saveall=TRUE)


###################################################
plot(res, xlim=c(0,0.7))
for(i in 2:(nsim+1)) lines(attr(res, "savedata")[[1]], attr(res, "savedata")[[i]], col="grey")
plot(res, add=TRUE, xlim=c(0,0.7))


###################################################
source("clarkevans_rev.R")
clarkevans(drumlins_ppp)
clarkevans(drumlins_rr, correction="none")
clarkevans(drumlins_rr, correction="guard", clipregion=erode.owin(rr, r=1))


###################################################
ex <- expression(rSSI(0.18, n, win=rr))
res <- envelope(drumlins_rr, Gest, nsim=nsim, simulate=ex, saveall=TRUE)


###################################################
plot(res, xlim=c(0,0.7))
for(i in 2:(nsim+1)) lines(attr(res, "savedata")[[1]], attr(res, "savedata")[[i]], col="grey")
plot(res, add=TRUE, lwd=3, xlim=c(0,0.7))


###################################################
ex <- expression(runifpoint(n, win=rr))
res <- envelope(drumlins_rr, Kest, nsim=nsim, simulate=ex, saveall=TRUE)


###################################################
r <- res$r
Lhat <- function(k, r) { (sqrt(k/pi)) - r }
plot(r, Lhat(res$obs, r), type="n", ylab="L(r)", main="CSR simulation")
for(i in 2:(nsim+1)) lines(r, Lhat(attr(res, "savedata")[[i]], r), col="grey")
lines(r, Lhat(res$obs, r), lwd=2, col="brown")
lines(r, Lhat(res$lo, r), lwd=2, col="black", lty=2)
lines(r, Lhat(res$hi, r), lwd=2, col="black", lty=2)


###################################################
ex <- expression(rSSI(0.18, n, win=rr))
res <- envelope(drumlins_rr, Kest, nsim=nsim, simulate=ex, saveall=TRUE)


###################################################
r <- res$r
Lhat <- function(k, r) { (sqrt(k/pi)) - r }
plot(r, Lhat(res$obs, r), type="n", ylab="L(r)", main="SSI simulation")
for(i in 2:(nsim+1)) lines(r, Lhat(attr(res, "savedata")[[i]], r), col="grey")
lines(r, Lhat(res$obs, r), lwd=2, col="brown")
lines(r, Lhat(res$lo, r), lwd=2, col="black", lty=2)
lines(r, Lhat(res$hi, r), lwd=2, col="black", lty=2)

