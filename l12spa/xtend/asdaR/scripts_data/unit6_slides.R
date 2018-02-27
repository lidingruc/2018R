###################################################
library(rgdal)
NY8 <- readOGR(".", "NY8_utm18")
TCE <- readOGR(".", "TCE")
cities <- readOGR(".", "NY8cities")


###################################################
oopar <- par(mar=c(3,3,1,1)+0.1)
plot(NY8, border="grey60", axes=TRUE)
text(coordinates(cities), labels=as.character(cities$names), font=2, cex=0.8)
points(TCE, pch=1, cex=0.7, col="darkred")
points(TCE, pch=3, cex=0.7, col="darkred")
text(coordinates(TCE), labels=as.character(TCE$name), cex=0.7,
 font=1, pos=c(4,1,4,1,4,4,4,2,3,4,2), offset=0.3, col="darkred")
par(oopar)


###################################################
library(spdep)
NY_nb <- read.gal("NY_nb.gal", region.id=row.names(as(NY8, "data.frame")))


###################################################
oopar <- par(mar=c(1,1,1,1)+0.1)
plot(NY8, border="grey60")
plot(NY_nb, coordinates(NY8), pch=19, cex=0.6, add=TRUE)
par(oopar)


###################################################
Syracuse <- NY8[NY8$AREANAME == "Syracuse city",]
Sy0_nb <- subset(NY_nb, NY8$AREANAME == "Syracuse city")
isTRUE(all.equal(attr(Sy0_nb, "region.id"),
  row.names(as(Syracuse, "data.frame"))))
summary(Sy0_nb)


###################################################
Sy1_nb <- poly2nb(Syracuse)
isTRUE(all.equal(Sy0_nb, Sy1_nb, check.attributes=FALSE))


###################################################
oopar <- par(mar=c(3,3,1,1)+0.1)
plot(Syracuse, border="grey60")
plot(Sy0_nb, coordinates(Syracuse), add=TRUE, pch=19, cex=0.6)
par(oopar)


###################################################
Sy2_nb <- poly2nb(Syracuse, queen=FALSE)
isTRUE(all.equal(Sy0_nb, Sy2_nb, check.attributes=FALSE))


###################################################
oopar <- par(mar=c(3,3,1,1)+0.1)
plot(Syracuse, border="grey60")
plot(Sy0_nb, coordinates(Syracuse), add=TRUE, pch=19, cex=0.6)
plot(diffnb(Sy0_nb, Sy2_nb, verbose=FALSE), coordinates(Syracuse),
  add=TRUE, pch=19, cex=0.6, lwd=3)
par(oopar)


###################################################
coords <- coordinates(Syracuse)
IDs <- row.names(as(Syracuse, "data.frame"))
library(tripack)
Sy4_nb <- tri2nb(coords, row.names=IDs)
Sy5_nb <- graph2nb(soi.graph(Sy4_nb, coords), row.names=IDs)
Sy6_nb <- graph2nb(gabrielneigh(coords), row.names=IDs)
Sy7_nb <- graph2nb(relativeneigh(coords), row.names=IDs)
nb_l <- list(Triangulation=Sy4_nb, SOI=Sy5_nb, Gabriel=Sy6_nb,
  Relative=Sy7_nb)
sapply(nb_l, function(x) is.symmetric.nb(x, verbose=FALSE, force=TRUE))


###################################################
oopar <- par(mfrow=c(2,2), mar=c(1,1,1,1)+0.1)
plot(Syracuse, border="grey60")
plot(Sy4_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="Triangulation", cex=0.7)
plot(Syracuse, border="grey60")
plot(Sy5_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="SOI graph", cex=0.7)
plot(Syracuse, border="grey60")
plot(Sy6_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="Gabriel", cex=0.7)
plot(Syracuse, border="grey60")
plot(Sy7_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="Relative", cex=0.7)
par(oopar)


###################################################
Sy8_nb <- knn2nb(knearneigh(coords, k=1), row.names=IDs)
Sy9_nb <- knn2nb(knearneigh(coords, k=2), row.names=IDs)
Sy10_nb <- knn2nb(knearneigh(coords, k=4), row.names=IDs)
nb_l <- list(k1=Sy8_nb, k2=Sy9_nb, k4=Sy10_nb)
sapply(nb_l, function(x) is.symmetric.nb(x, verbose=FALSE, force=TRUE))
sapply(nb_l, function(x) n.comp.nb(x)$nc)


###################################################
oopar <- par(mfrow=c(1,3), mar=c(1,1,1,1)+0.1)
plot(Syracuse, border="grey60")
plot(Sy8_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="k=1", cex=0.7)
plot(Syracuse, border="grey60")
plot(Sy9_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="k=2", cex=0.7)
plot(Syracuse, border="grey60")
plot(Sy10_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="k=4", cex=0.7)
par(oopar)


###################################################
dsts <- unlist(nbdists(Sy8_nb, coords))
summary(dsts)
max_1nn <- max(dsts)
max_1nn
Sy11_nb <- dnearneigh(coords, d1=0, d2=0.75*max_1nn, row.names=IDs)
Sy12_nb <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=IDs)
Sy13_nb <- dnearneigh(coords, d1=0, d2=1.5*max_1nn, row.names=IDs)
nb_l <- list(d1=Sy11_nb, d2=Sy12_nb, d3=Sy13_nb)
sapply(nb_l, function(x) is.symmetric.nb(x, verbose=FALSE, force=TRUE))
sapply(nb_l, function(x) n.comp.nb(x)$nc)


###################################################
oopar <- par(mfrow=c(1,3), mar=c(1,1,1,1)+0.1)
plot(Syracuse, border="grey60")
plot(Sy11_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="< 1158m", cex=0.7)
plot(Syracuse, border="grey60")
plot(Sy12_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="< 1545m", cex=0.7)
plot(Syracuse, border="grey60")
plot(Sy13_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="< 2317m", cex=0.7)
par(oopar)


###################################################
res <- sapply(nb_l, function(x) table(card(x)))
mx <- max(card(Sy13_nb))
res1 <- matrix(0, ncol=(mx+1), nrow=3)
rownames(res1) <- names(res)
colnames(res1) <- as.character(0:mx)
res1[1, names(res$d1)] <- res$d1
res1[2, names(res$d2)] <- res$d2
res1[3, names(res$d3)] <- res$d3
barplot(res1, beside=TRUE, legend.text=TRUE, xlab="numbers of neighbours")


###################################################
Sy0_nb_lags <- nblag(Sy0_nb, maxlag=9)


###################################################
cell2nb(7, 7, type="rook", torus=TRUE)
cell2nb(7, 7, type="rook", torus=FALSE)


###################################################
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
gridded(meuse.grid) <- TRUE
dst <- max(slot(slot(meuse.grid, "grid"), "cellsize"))
mg_nb <- dnearneigh(coordinates(meuse.grid), 0, dst)
mg_nb
table(card(mg_nb))


###################################################
Sy0_lw_W <- nb2listw(Sy0_nb)
Sy0_lw_W


###################################################
Sy0_lw_B <- nb2listw(Sy0_nb, style="B")
Sy0_lw_B


###################################################
dsts <- nbdists(Sy0_nb, coordinates(Syracuse))
idw <- lapply(dsts, function(x) 1/(x/1000))
Sy0_lw_idwB <- nb2listw(Sy0_nb, glist=idw, style="B")
Sy0_lw_idwB


###################################################
library(RColorBrewer)
pal <- brewer.pal(9, "Reds")
oopar <- par(mfrow=c(1,3), mar=c(1,1,3,1)+0.1)
z <- t(listw2mat(Sy0_lw_W))
brks <- c(0,0.1,0.143,0.167,0.2,0.5,1)
nbr3 <- length(brks)-3
image(1:63, 1:63, z[,ncol(z):1], breaks=brks, col=pal[c(1,(9-nbr3):9)],
 main="W style", axes=FALSE)
box()
z <- t(listw2mat(Sy0_lw_B))
image(1:63, 1:63, z[,ncol(z):1], col=pal[c(1,9)], main="B style", axes=FALSE)
box()
z <- t(listw2mat(Sy0_lw_idwB))
brks <- c(0,0.35,0.73,0.93,1.2,2.6)
nbr3 <- length(brks)-3
image(1:63, 1:63, z[,ncol(z):1], breaks=brks, col=pal[c(1,(9-nbr3):9)],
 main="IDW B style", axes=FALSE)
box()
par(oopar)


###################################################
## Sy0_lw_D1 <- nb2listw(Sy11_nb, style="B")


###################################################
t1 <- try(Sy0_lw_D1 <- nb2listw(Sy11_nb, style="B"), silent=TRUE)
cat(strwrap(t1), sep="\n")


###################################################
Sy0_lw_D1 <- nb2listw(Sy11_nb, style="B", zero.policy=TRUE)
print(Sy0_lw_D1, zero.policy=TRUE)


###################################################
set.seed(987654)
n <- length(Sy0_nb)
uncorr_x <- rnorm(n)
cor(uncorr_x, lag(Sy0_lw_W, uncorr_x))
rho <- 0.5
IrW <- diag(n) - rho * listw2mat(Sy0_lw_W)
SARcov <- solve(t(IrW) %*% IrW)
SARcovL <- chol((SARcov + t(SARcov))/2)
autocorr_x <- t(SARcovL) %*% uncorr_x
cor(autocorr_x, lag(Sy0_lw_W, autocorr_x))


###################################################
Syracuse$uncorr_x <- uncorr_x
Syracuse$autocorr_x <- autocorr_x
spplot(Syracuse, c("uncorr_x", "autocorr_x"),
 col.regions=colorRampPalette(brewer.pal(6, "Reds"))(31), cuts=30)


###################################################
oopar <- par(mfrow=c(1,2), mar=c(4,4,3,2)+0.1)
plot(uncorr_x, lag(Sy0_lw_W, uncorr_x), xlab="random variable", cex.lab=0.8,
 ylab="spatial lag", main="Uncorrelated random variable", cex.main=0.8)
lines(lowess(uncorr_x, lag(Sy0_lw_W, uncorr_x)), lty=2, lwd=2)
plot(autocorr_x, lag(Sy0_lw_W, autocorr_x),
 xlab="autocorrelated random variable", ylab="spatial lag",
 main="Autocorrelated random variable", cex.main=0.8, cex.lab=0.8)
lines(lowess(autocorr_x, lag(Sy0_lw_W, autocorr_x)), lty=2, lwd=2)
par(oopar)


###################################################
moran_u <- moran.test(uncorr_x, listw=Sy0_lw_W)
moran_a <- moran.test(autocorr_x, listw=Sy0_lw_W)
moran_a1 <- moran.test(autocorr_x, listw=nb2listw(Sy9_nb, style="W"))
et <- coords[,1] - min(coords[,1])
trend_x <- uncorr_x + 0.00025 * et
moran_t <- moran.test(trend_x, listw=Sy0_lw_W)
moran_t1 <- lm.morantest(lm(trend_x ~ et), listw=Sy0_lw_W)


###################################################
lw <- nb2listw(NY_nb, style="B")
moran.test(NY8$Cases, listw=lw, randomisation=FALSE)
CR <- function(var, mle) rpois(length(var), lambda=mle)
MoranI.pboot <- function(var, i, listw, n, S0, ...) {
  return(moran(x=var, listw=listw, n=n, S0=S0)$I)
}
r <- sum(NY8$Cases)/sum(NY8$POP8)
rni <- r*NY8$POP8
set.seed(1)
boot2 <- boot(NY8$Cases, statistic=MoranI.pboot, R=999, sim="parametric",
  ran.gen=CR, mle=rni, listw=lw, n=length(NY8$Cases), S0=Szero(lw))
pnorm((boot2$t0 - mean(boot2$t))/sd(boot2$t), lower.tail=FALSE)


###################################################
Ip <- NY8$Cases/NY8$POP8
set.seed(987)
moran.mc(Ip, listw=lw, nsim=999)
EBImoran.mc(n=NY8$Cases, x=NY8$POP8, listw=lw, nsim=999)


###################################################
NY8$Zi <- log((1000*(NY8$Cases+1))/NY8$POP8)
moran.test(NY8$Zi, listw=lw, randomisation=FALSE)


###################################################
lm.morantest(lm(Zi ~ 1, data=NY8), listw=lw)


###################################################
lm_Zi <- lm(Zi ~ 1, data=NY8, weights=POP8)
lm.morantest(lm_Zi, listw=lw)


###################################################
res <- sp.correlogram(NY_nb, residuals(lm_Zi), order=8, method="I", style="B")
res


###################################################
plot(res)


###################################################
moran.plot(residuals(lm_Zi), lw, quiet=TRUE)


###################################################
res <- localmoran(x=NY8$Cases, listw=nb2listw(NY_nb, style="W"))
colnames(res)
NY8$Cases_locI_R_pv <- res[,5]
res0 <- localmoran(NY8$Zi, nb2listw(NY_nb, style="W"))
NY8$Zi_locI_R_pv <- res0[,5]
res1 <- localmoran.sad(model=lm_Zi, select=1:length(NY_nb), nb=NY_nb, style="W")
res1s <- summary(res1)
names(res1s)
NY8$Zi_locI_N_pv <- res1s[,3]
NY8$Zi_locI_S_pv <- res1s[,5]


###################################################
spplot(NY8, c("Cases_locI_R_pv", "Zi_locI_R_pv", "Zi_locI_N_pv", "Zi_locI_S_pv"), main="Overall p-values", at=seq(0,1,0.05), col.regions=rev(cm.colors(20)), par.strip.text=list(cex=0.7))


###################################################
spplot(NY8, c("Cases_locI_R_pv", "Zi_locI_R_pv", "Zi_locI_N_pv", "Zi_locI_S_pv"), xlim=c(403270, 433290), ylim=c(4652560, 4670170), at=c(0,0.01,0.05,1), col.regions=c("black", "grey", "white"), main="Binghampton p-values", par.strip.text=list(cex=0.7))


###################################################
ZiSAR <- errorsarlm(Zi ~ 1, NY8, lw)
res2 <- localmoran(residuals(ZiSAR), nb2listw(NY_nb, style="W"))
NY8$ZiSAR_locI_R_pv <- res2[,5]
res3 <- localmoran.sad(model=ZiSAR, select=1:length(NY_nb), nb=NY_nb, style="W")
res3s <- summary(res3)
NY8$ZiSAR_locI_N_pv <- res3s[,3]
NY8$ZiSAR_locI_S_pv <- res3s[,5]


###################################################
spplot(NY8, c("ZiSAR_locI_R_pv", "ZiSAR_locI_N_pv", "ZiSAR_locI_S_pv"), main="Overall SAR-corrected p-values", at=seq(0,1,0.05), col.regions=rev(cm.colors(20)), par.strip.text=list(cex=0.7))



