###################################################
library(rgdal)
scot_LL <- readOGR(".", "scot")
proj4string(scot_LL) <- CRS("+proj=longlat ellps=WGS84")
EPSG <- make_EPSG()
EPSG[grep("British National Grid", EPSG$note), 1:2]
scot_BNG0 <- spTransform(scot_LL, CRS("+init=epsg:27700"))


###################################################
library(maptools)
scot_dat <- read.table("scotland.dat", skip=1)
names(scot_dat) <- c("District", "Observed", "Expected", "PcAFF", "Latitude", "Longitude")
row.names(scot_dat) <- formatC(scot_dat$District, width=2, flag="0")
ID <- formatC(scot_BNG0$ID, width=2, flag="0")
scot_BNG1 <- spChFIDs(scot_BNG0, ID)
scot_BNG <- spCbind(scot_BNG1, scot_dat[match(ID, row.names(scot_dat)),])


###################################################
bluepal <- colorRampPalette(c("azure1", "steelblue4"))
brks <- c(0,5,10,25,50,100)
library(classInt)
O_CI <- classIntervals(scot_BNG$Observed, style="fixed", fixedBreaks=brks)
E_CI <- classIntervals(scot_BNG$Expected, style="fixed", fixedBreaks=brks)


###################################################
oopar <- par(mar=c(1,1,3,1)+0.1)
O_CIc <- findColours(O_CI, bluepal(2))
plot(scot_BNG, col=O_CIc)
title(main="Observed")
legend("topleft", fill=attr(O_CIc, "palette"), legend=names(attr(O_CIc, "table")), bty="n")
par(oopar)


###################################################
oopar <- par(mar=c(1,1,3,1)+0.1)
E_CIc <- findColours(E_CI, bluepal(2))
plot(scot_BNG, col=E_CIc)
title(main="Expected")
legend("topleft", fill=attr(E_CIc, "palette"), legend=names(attr(E_CIc, "table")), bty="n")
par(oopar)


###################################################
library(spdep)
ch <- choynowski(scot_BNG$Observed, scot_BNG$Expected)


###################################################
oopar <- par(mar=c(1,1,3,1)+0.1)
cols <- rep("white", length(ch$pmap))
cols[(ch$pmap < 0.05) & (ch$type)] <- "grey35"
cols[(ch$pmap < 0.05) & (!ch$type)] <- "grey75"
plot(scot_BNG, col=cols)
title(main="Choynowski probability map")
legend("topleft", fill=c("grey35", "white", "grey75"), legend=c("low", "N/S", "high"), bty="n")
par(oopar)


###################################################
pm <- probmap(scot_BNG$Observed, scot_BNG$Expected)
names(pm)
scot_BNG$SMR <- pm$relRisk


###################################################
oopar <- par(mar=c(1,1,3,1)+0.1)
brks_prob <- c(0,0.05,0.1,0.2,0.8,0.9,0.95,1)
fixed_prob <- classIntervals(pm$pmap, style="fixed", fixedBreaks=brks_prob)
library(RColorBrewer)
pal_prob <- rev(brewer.pal(5, "RdBu"))
cols <- findColours(fixed_prob, pal_prob)
plot(scot_BNG, col=cols)
title(main="Poisson probability map")
table <- attr(cols, "table")
legtext <- paste(names(table), " (", table, ")", sep="")
legend("topleft", fill=attr(cols, "palette"), legend=legtext, bty="n", cex=0.7, y.inter=0.8)
par(oopar)


###################################################
table(findInterval(pm$pmap, seq(0,1,1/10)))


###################################################
hist(pm$pmap, breaks=8, col="grey", main="Poisson probability map")


###################################################
eb1 <- EBest(scot_BNG$Observed, scot_BNG$Expected)
unlist(attr(eb1, "parameters"))
scot_BNG$EB_mm <- eb1$estmm*100
library(DCluster)
res <- empbaysmooth(scot_BNG$Observed, scot_BNG$Expected)
unlist(res[2:3])
scot_BNG$EB_ml <- res$smthrr*100


###################################################
CK_nb <- read.gal("CK.gal", region.id=scot_BNG$District)
CK_nb


###################################################
oopar <- par(mar=c(1,1,3,1)+0.1)
plot(scot_BNG, border="grey")
plot(CK_nb, coordinates(scot_BNG), col="blue3", add=TRUE)
par(oopar)


###################################################
eb2 <- EBlocal(scot_BNG$Observed, scot_BNG$Expected, CK_nb)
scot_BNG$EB_mm_local <- eb2$est*100


###################################################
spplot(scot_BNG, c("SMR", "EB_mm", "EB_ml", "EB_mm_local"), col.regions=bluepal(16))


###################################################
lw <- nb2listw(CK_nb)
set.seed(20070831)
moran.boot <- boot(as(scot_BNG, "data.frame"), statistic=moranI.boot, R=999, listw=lw, n=length(CK_nb), S0=Szero(lw))


###################################################
plot(moran.boot)


###################################################
moran.pgboot <- boot(as(scot_BNG, "data.frame"), statistic=moranI.pboot, sim="parametric", ran.gen=negbin.sim, R=999, listw=lw, n=length(CK_nb), S0=Szero(lw))


###################################################
plot(moran.pgboot)


###################################################
EBImoran.mc(scot_BNG$Observed, scot_BNG$Expected, lw, nsim=999)


###################################################
base.glm <- glm(Observed ~ 1 + offset(log(Expected)), data=scot_BNG, family=poisson())
base.glmQ <- glm(Observed ~ 1 + offset(log(Expected)), data=scot_BNG, family=quasipoisson())
library(MASS)
base.nb <- glm.nb(Observed ~ 1 + offset(log(Expected)), data=scot_BNG)
unlist(summary(base.nb)[20:21])


###################################################
test.nb.pois(base.nb, base.glm)
DeanB(base.glm)


###################################################
AFF.glm <- glm(Observed ~ PcAFF + offset(log(Expected)), data=scot_BNG, family=poisson())
AFF.glmQ <- glm(Observed ~ PcAFF + offset(log(Expected)), data=scot_BNG, family=quasipoisson())
AFF.nb <- glm.nb(Observed ~ PcAFF + offset(log(Expected)), data=scot_BNG)
unlist(summary(AFF.nb)[20:21])
anova(base.nb, AFF.nb)


###################################################
scot_BNG$base_glm_rst <- rstandard(base.glm)
scot_BNG$base_glmQ_rst <- rstandard(base.glmQ)
scot_BNG$base_nb_rst <- rstandard(base.nb)
scot_BNG$AFF_glm_rst <- rstandard(AFF.glm)
scot_BNG$AFF_glmQ_rst <- rstandard(AFF.glmQ)
scot_BNG$AFF_nb_rst <- rstandard(AFF.nb)


###################################################
spplot(scot_BNG, c("base_glm_rst", "base_glmQ_rst", "base_nb_rst", "AFF_glm_rst", "AFF_glmQ_rst", "AFF_nb_rst"), col.regions=colorRampPalette(brewer.pal(3, "RdBu"))(16))


###################################################
writePolyShape(scot_BNG, "scot_BNG")
crs <- showWKT(proj4string(scot_BNG), "scot_BNG.prj")
cat(strwrap(gsub(",", ", ", crs)), sep="\n")
sp2Mondrian(scot_BNG, "scot_BNG.txt")



