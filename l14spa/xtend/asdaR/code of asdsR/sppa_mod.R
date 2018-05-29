### R code from vignette source 'sppa.Rnw'
# data: spasthma spbdry spsrc sproads
###################################################
### code chunk number 6: sppa.Rnw:230-232
###################################################
library(spatstat)
data(japanesepines)


###################################################
### code chunk number 7: sppa.Rnw:234-235
###################################################
summary(japanesepines)


###################################################
### code chunk number 8: sppa.Rnw:258-259
###################################################
library(maptools)


###################################################
### code chunk number 9: sppa.Rnw:261-263
###################################################
spjpines <- as(japanesepines, "SpatialPoints")
summary(spjpines)


###################################################
### code chunk number 10: sppa.Rnw:275-277
###################################################
spjpines1 <- elide(spjpines, scale=TRUE, unitsq=TRUE)
summary(spjpines1)


###################################################
### code chunk number 11: sppa.Rnw:294-296
###################################################
pppjap <- as(spjpines1, "ppp")
summary(pppjap)


###################################################
### code chunk number 12: sppa.Rnw:300-311
###################################################
data(redwoodfull)
spred <- as(redwoodfull, "SpatialPoints")
data(cells)
spcells <- as(cells, "SpatialPoints")
dpp<-data.frame(rbind(coordinates(spjpines1), coordinates(spred), 
   coordinates(spcells)))
njap<-nrow(coordinates(spjpines1))
nred<-nrow(coordinates(spred))
ncells<-nrow(coordinates(spcells))
dpp<-cbind(dpp,c(rep("JAPANESE",njap), rep("REDWOOD", nred), rep("CELLS", ncells))) 
names(dpp)<-c("x", "y", "DATASET")


###################################################
### code chunk number 13: sppa.Rnw:319-327
###################################################
library(lattice)
print(xyplot(y~x|DATASET, data=dpp, pch=19, aspect=1))


###################################################
### code chunk number 15: sppa.Rnw:401-406
###################################################
library(rgdal)
spasthma <- readOGR("spasthma.shp", "spasthma")
spbdry <- readOGR("spbdry.shp", "spbdry")
spsrc <- readOGR("spsrc.shp", "spsrc")
sproads <- readOGR("sproads.shp", "sproads")


###################################################
### code chunk number 17: sppa.Rnw:420-436
###################################################
plot(spbdry, axes=TRUE, lwd=0.5)
plot(sproads, add=TRUE, lwd=2, col="darkslategrey")
c_c <- (spasthma$Asthma == "case") + 1
plot(spasthma[c_c == 1,], add=TRUE, pch=4, cex=0.6, col="mediumaquamarine")
plot(spasthma[c_c == 2,], add=TRUE, pch=17, cex=0.75, col="goldenrod2")
plot(spsrc, pch=22, add=TRUE, cex=1.2, bg="brown4")
legend("bottomright", legend=c("controls", "cases", "pollution sources"), pch=c(4, 17, 22), pt.cex=c(0.6, 0.75, 1.2), pt.bg=c(NA, NA, "brown4"), col=c("mediumaquamarine", "goldenrod2", "black"), bty="n") 


###################################################
### code chunk number 19: sppa.Rnw:547-557 (eval = FALSE)
###################################################
set.seed(120109)
# spatstat 1.45-0 enforce a tighter spacing of the r vector 2016-03-22
# r <- seq(0, sqrt(2)/6, by = 0.005)
r <- seq(0, sqrt(2)/6, by = 0.001)
envjap <- envelope(as(spjpines1, "ppp"), fun=Gest, r=r, nrank=2, nsim=99)
envred <- envelope(as(spred, "ppp"), fun=Gest, r=r, nrank=2, nsim=99)
envcells <- envelope(as(spcells, "ppp"), fun=Gest, r=r, nrank=2, nsim=99)
Gresults <- rbind(envjap, envred, envcells) 
Gresults <- cbind(Gresults, 
   y=rep(c("JAPANESE", "REDWOOD", "CELLS"), each=length(r)))


###################################################
### code chunk number 22: sppa.Rnw:595-620
###################################################
print(xyplot(obs~theo|y , data=Gresults, type="l", 
xlab = "theoretical", ylab = "observed", 
panel=function(x, y, subscripts) {
   lpolygon(c(x, rev(x)), 
   c(Gresults$lo[subscripts], rev(Gresults$hi[subscripts])),
   border="gray", col="gray"
)
llines(x, y, col="black", lwd=2)
}))


###################################################
### code chunk number 24: sppa.Rnw:661-670 (eval = FALSE)
###################################################
set.seed(30)
Fenvjap<-envelope(as(spjpines1, "ppp"), fun=Fest, r=r, nrank=2, nsim=99)
Fenvred<-envelope(as(spred, "ppp"), fun=Fest, r=r, nrank=2, nsim=99)
Fenvcells<-envelope(as(spcells, "ppp"), fun=Fest, r=r, nrank=2, nsim=99)
Fresults<-rbind(Fenvjap, Fenvred, Fenvcells)
Fresults<-cbind(Fresults, 
   y=rep(c("JAPANESE", "REDWOOD", "CELLS"), each=length(r)))


###################################################
### code chunk number 27: sppa.Rnw:700-716
###################################################
print(xyplot(obs~theo|y , data=Fresults, type="l", 
xlab = "theoretical", ylab = "observed", 
panel=function(x, y, subscripts) {
   lpolygon(c(x, rev(x)), 
   c(Fresults$lo[subscripts], rev(Fresults$hi[subscripts])),
   border="gray", col="gray"
   )
   llines(x, y, col="black", lwd=2)
}))


###################################################
### code chunk number 28: sppa.Rnw:925-932
###################################################
set.seed(7)
x<-runif(10)
nx<-length(x)
bw<-.1

k<-density(x, bw=bw, kernel="biweight")
k$y<-k$y*nx


###################################################
### code chunk number 29: sppa.Rnw:939-951
###################################################
plot(k, ylab="Intensity", main="")
points(x, rep(0, nx), pch=20)
for(i in 1:length(x))
  lines(density(x[i], bw=bw, kernel="biweight"), lty=2)

legend(x=14, y=0.6, legend=c("Intensity", "Kernel"), lty=c(1,2))


###################################################
### code chunk number 30: sppa.Rnw:996-997
###################################################
library(splancs)


###################################################
### code chunk number 31: sppa.Rnw:999-1008
###################################################
mserwq<-mse2d(as.points(coordinates(spred)),
 as.points(list(x=c(0,1,1,0), y=c(0,0,1,1))), 100, .15)
bwq<-mserwq$h[which.min(mserwq$mse)]
bwq

#Spatstat code
mserw<-bw.diggle(as(spred, "ppp"))
bw<-as.numeric(mserw)
bw


###################################################
### code chunk number 32: sppa.Rnw:1029-1045
###################################################
plot(mserwq$h, mserwq$mse, xlab="Bandwidth", ylab="MSE", type="l", ylim=c(-2,50), main="Quartic kernel")
i<-which.min(mserwq$mse)
points(mserwq$h[i], mserwq$mse[i])
plot(mserw, main="Gaussian kernel", xlab="Bandwidth", ylab="MSE")
points(attr(mserw, "h")[attr(mserw, "iopt")], bw)


###################################################
### code chunk number 33: sppa.Rnw:1074-1086
###################################################
library(splancs)
poly <- as.points(list(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0)))
sG <- Sobj_SpatialGrid(spred, maxDim=100)$SG
grd <- slot(sG, "grid")
summary(grd)
k0 <- spkernel2d(spred, poly, h0=bw, grd)
k1 <- spkernel2d(spred, poly, h0=.05, grd)
k2 <- spkernel2d(spred, poly, h0=.1, grd)
k3 <- spkernel2d(spred, poly, h0=.15, grd)
df <- data.frame(k0=k0, k1=k1, k2=k2, k3=k3) 
kernels <- SpatialGridDataFrame(grd, data=df)
summary(kernels)


###################################################
### code chunk number 34: sppa.Rnw:1118-1129
###################################################
cc <- coordinates(kernels)
xy<-list(x=cc[,1], y=cc[,2])
k4<-density(as(spred, "ppp"), .5*bw, dimyx=c(100, 100), xy=xy)
kernels$k4<-as(k4, "SpatialGridDataFrame")$v
k5<-density(as(spred, "ppp"), .5*.05, dimyx=c(100, 100), xy=xy)
kernels$k5<-as(k5, "SpatialGridDataFrame")$v
k6<-density(as(spred, "ppp"), .5*.1, dimyx=c(100, 100), xy=xy)
kernels$k6<-as(k6, "SpatialGridDataFrame")$v
k7<-density(as(spred, "ppp"), .5*.15, dimyx=c(100, 100), xy=xy)
kernels$k7<-as(k7, "SpatialGridDataFrame")$v
summary(kernels)


###################################################
### code chunk number 35: sppa.Rnw:1138-1159
###################################################
library(RColorBrewer)
gp <- brewer.pal(8, "Reds")
print(spplot(kernels, at=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), col.regions=colorRampPalette(gp)(15)[1:12], 
names.attr=c(paste("Q bw=",round(bw, digits=4), sep="", collapse=""),
"Q bw=0.05", "Q bw=0.1","Q bw=0.15", paste("G bw=", round(.5*bw, digits=4),
 sep="", collapse=""), "G bw=0.025", "G bw=0.05","G bw=0.075"), cex=0.7, colorkey=FALSE))


###################################################
### code chunk number 36: sppa.Rnw:1230-1249
###################################################
#Fit parametric model
loglambda<-function(x, alpha, beta)
{
    l<-alpha+sum(beta*c(x, x*x, prod(x)))
    return(l)
}

L<-function(alphabeta, x)
{
    l<-apply(x,1,loglambda, alpha=alphabeta[1], beta=alphabeta[-1])
    l<-sum(l)
    intL<-adaptIntegrate(lowerLimit=c(0,0), upperLimit=c(1,1), fDim=1,
        tol=1e-8, f=function(x, alpha=alphabeta[1], beta=alphabeta[-1])
        {
            exp(loglambda(x, alpha, beta))
    })
    l<-l-intL$integral
    return(l)#Optim minimises
}


###################################################
### code chunk number 37: sppa.Rnw:1266-1270
###################################################
library(cubature)
data(lansing)
x<-as.points(lansing[lansing$marks=="maple",])


###################################################
### code chunk number 38: sppa.Rnw:1272-1275 (eval = FALSE)
###################################################
#Maximise log-likelihood
optbeta<-optim(par=c(log(514),0,0,0,0,0), fn=L, control=list(maxit=1000, fnscale=-1), x=x)


###################################################
### code chunk number 40: sppa.Rnw:1309-1329
###################################################
grd <- GridTopology(cellcentre.offset=c(0.005,0.005), cellsize=c(0.01, 0.01),
  cells.dim=c(100, 100))
lambda<-exp(apply(coordinates(grd),1, function(X, alpha, beta)
    {
        loglambda(X, alpha, beta)
    }, alpha=optbeta$par[1], beta=optbeta$par[-1]
 ))

parint<-SpatialGridDataFrame(grd, data=data.frame(intensity=lambda))

lyt<-list("sp.points", SpatialPoints(x), pch=19, col="black", cex=0.7)
print(spplot(parint, at=seq(0,1400,length.out=8),
 col.regions=colorRampPalette(gp)(7), sp.layout=list(lyt)))


###################################################
### code chunk number 41: sppa.Rnw:1346-1348
###################################################
lmaple<-lansing[lansing$marks=="maple",]
ppm(Q=lmaple, trend=~x+y+I(x^2)+I(y^2)+I(x*y))


###################################################
### code chunk number 43: sppa.Rnw:1420-1430
###################################################
set.seed(30)
Kenvjap<-envelope(as(spjpines1, "ppp"), fun=Kest, r=r, nrank=2, nsim=99)
Kenvred<-envelope(as(spred, "ppp"), fun=Kest, r=r, nrank=2, nsim=99)
Kenvcells<-envelope(as(spcells, "ppp"), fun=Kest, r=r, nrank=2, nsim=99)
Kresults<-rbind(Kenvjap, Kenvred, Kenvcells)
Kresults<-cbind(Kresults, 
   y=rep(c("JAPANESE", "REDWOOD", "CELLS"), each=length(r)))


###################################################
### code chunk number 45: sppa.Rnw:1468-1485
###################################################
print(xyplot((obs-theo)~r|y , data=Kresults, type="l",
   ylim= c(-.06, .06), ylab=expression(hat(K) (r)  - pi * r^2),
   panel=function(x, y, subscripts) {
      Ktheo<- Kresults$theo[subscripts]
      lpolygon(c(r, rev(r)),
      c(Kresults$lo[subscripts]-Ktheo, rev(Kresults$hi[subscripts]-Ktheo)),
        border="gray", col="gray"
      )
      llines(r, Kresults$obs[subscripts]-Ktheo, lty=2, lwd=1.5, col="black")
}))


###################################################
### code chunk number 46: sppa.Rnw:1698-1706
###################################################
bwasthma<-.06

pppasthma<-as(spasthma, "ppp")
pppasthma$window<-as(spbdry, "owin")

marks(pppasthma)<-relevel(pppasthma$marks$Asthma, "control")



###################################################
### code chunk number 47: sppa.Rnw:1716-1721
###################################################
bwasthma<-.06


###################################################
### code chunk number 52: sppa.Rnw:1838-1845
###################################################
cases<-unmark(subset(pppasthma, marks(pppasthma) =="case"))
ncases<-npoints(cases)
controls<-unmark(subset(pppasthma, marks(pppasthma) =="control"))
ncontrols<-npoints(controls)

kcases<-density(cases, bwasthma)
kcontrols<-density(controls, bwasthma)


###################################################
### code chunk number 54: sppa.Rnw:1880-1888
###################################################

spkratio0<-as(kcases, "SpatialGridDataFrame")
names(spkratio0)<-"kcases"
spkratio0$kcontrols<-as(kcontrols, "SpatialGridDataFrame")$v
spkratio<-as(spkratio0, "SpatialPixelsDataFrame")

spkratio$kratio <- spkratio$kcases/spkratio$kcontrols
spkratio$logratio <- log(spkratio$kratio)-log(ncases/ncontrols)


###################################################
### code chunk number 56: sppa.Rnw:1977-1983
###################################################
niter <- 99
ratio <- rep(NA, niter)
pvaluemap <- rep(0, nrow(spkratio))
rlabelratio <- matrix(NA, nrow=niter, ncol=nrow(spkratio))


###################################################
### code chunk number 59: sppa.Rnw:2017-2030 (eval = FALSE)
###################################################
set.seed(1)
for(i in 1:niter)
{
pppasthma0<-rlabel(pppasthma)
casesrel <- unmark(subset(pppasthma0, marks(pppasthma0) =="case"))
controlsrel <- unmark(subset(pppasthma0, marks(pppasthma0) =="control"))

kcasesrel <- density(casesrel, bwasthma)
kcontrolsrel <- density(controlsrel, bwasthma)
kratiorel <- eval.im(kcasesrel/kcontrolsrel)
rlabelratio[i,] <- as(as(kratiorel, "SpatialGridDataFrame"), "SpatialPixelsDataFrame")$v
pvaluemap <- pvaluemap + (spkratio$kratio < rlabelratio[i,])
}


###################################################
### code chunk number 64: sppa.Rnw:2081-2087
###################################################
cellsize<-kcontrols$xstep*kcontrols$ystep
ratiorho <- cellsize*sum((spkratio$kratio-ncases/ncontrols)^2)
ratio <- cellsize*apply(rlabelratio, 1, 
 function(X, rho0 ){sum((X-rho0)^2)}, rho0=ncases/ncontrols
)
pvaluerho <- (sum(ratio > ratiorho)+1)/(niter+1)


###################################################
### code chunk number 65: sppa.Rnw:2125-2129
###################################################
spkratio$pvaluemap <- (pvaluemap+1)/(niter+1)
imgpvalue <- as.image.SpatialGridDataFrame(spkratio["pvaluemap"])
clpvalue <- contourLines(imgpvalue, levels=c(0,.05, .95, 1))
cl <- ContourLines2SLDF(clpvalue)


###################################################
### code chunk number 66: sppa.Rnw:2141-2170
###################################################
library(RColorBrewer)
cl05 <- cl[cl$level == "0.05",]
xzx <- slot(slot(cl05, "lines")[[1]], "Lines")
cl05a <- SpatialLines(list(Lines(xzx, ID="0.05")))
lyt05 <- list("sp.lines", cl05a, lwd=2, lty=2, col="grey95")
lyt95 <- list("sp.lines", cl[cl$level == "0.95",], lwd=2, lty=1)
lytb <- list("sp.polygons", spbdry)
lytp <- list("sp.points", spsrc, cex=0.9, pch=4, col="grey95", lwd=3)
brks <- quantile(spkratio$kratio[spkratio$kratio>0], seq(0,1,1/10), na.rm=TRUE)
brks[1] <- 0
lbrks <- formatC(brks, 3, 6, "g", " ")
cols <- colorRampPalette(brewer.pal(7, "Reds"))(length(brks)-1)
colorkey<-list(labels=lbrks,
  at=(0:10)/10, height=.5)

print(spplot(spkratio, "kratio",
   col.regions=cols,
   do.log=TRUE, 
   colorkey=colorkey,
   at=c(0, brks[-c(1,11)], max(spkratio$kratio, na.rm=TRUE)),
   sp.layout=list(lyt05, lyt95, lytb, lytp) 
))


###################################################
### code chunk number 67: sppa.Rnw:2233-2234
###################################################
rrbw<-bw.relrisk(pppasthma, hmax=.5)


###################################################
### code chunk number 69: sppa.Rnw:2271-2274
###################################################
bwasthmap <- 0.06 


###################################################
### code chunk number 71: sppa.Rnw:2293-2295
###################################################
rr<-relrisk(pppasthma, bwasthmap)
spkratio$prob<-as(as(rr, "SpatialGridDataFrame"), "SpatialPixelsDataFrame")$v


###################################################
### code chunk number 72: sppa.Rnw:2307-2317
###################################################
ats <- seq(0,max(spkratio$prob),length.out=11)
cols <- colorRampPalette(brewer.pal(8, "Reds"))(length(ats)-1)
print(spplot(spkratio, "prob", col.regions=cols, at=ats, sp.layout=list(lytb, lytp)))


###################################################
### code chunk number 73: sppa.Rnw:2375-2386
###################################################
spasthma$y <- as.integer(!as.integer(spasthma$Asthma)-1)
ccasthma <- coordinates(spasthma)
spasthma$x1 <- ccasthma[,1]
spasthma$x2 <- ccasthma[,2]
spasthma$dist1 <- sqrt(spasthma$d2source1)
spasthma$dist2 <- sqrt(spasthma$d2source2)
spasthma$dist3 <- sqrt(spasthma$d2source3)
spasthma$droads <- sqrt(spasthma$roaddist2)
spasthma$smoking <- as.factor(as.numeric(spasthma$Nsmokers>0))
spasthma$Genderf<- as.factor(spasthma$Gender)
spasthma$HayFeverf<- as.factor(spasthma$HayFever)


###################################################
### code chunk number 74: sppa.Rnw:2388-2390
###################################################
library(mgcv)
gasthma<-gam(y~1+dist1+dist2+dist3+droads+Genderf+Age+HayFeverf+smoking+s(x1,x2), data=spasthma[spasthma$Gender==1 | spasthma$Gender==2, ], family=binomial)


###################################################
### code chunk number 75: sppa.Rnw:2392-2393
###################################################
summary(gasthma)


###################################################
### code chunk number 76: sppa.Rnw:2395-2398
###################################################
sumGasth <- summary(gasthma)
cpv <- sumGasth$p.pv
spv <- sumGasth$s.table[4]


###################################################
### code chunk number 77: sppa.Rnw:2483-2486
###################################################
D2_mat <- as.matrix(spasthma$dist2)
RHO <- ncases/ncontrols
expsource2<-tribble(ccflag=spasthma$y, vars=D2_mat, rho=RHO, alphas=1, betas=1)


###################################################
### code chunk number 78: sppa.Rnw:2488-2489
###################################################
print(expsource2)


###################################################
### code chunk number 79: sppa.Rnw:2491-2494
###################################################
#Hay fever
Hay_mat <- as.matrix(spasthma$HayFever)
exphay <- tribble(ccflag=spasthma$y, rho=RHO, covars=Hay_mat, thetas=1)


###################################################
### code chunk number 80: sppa.Rnw:2496-2497
###################################################
print(exphay)


###################################################
### code chunk number 81: sppa.Rnw:2524-2526
###################################################
expsource2hay<-tribble(ccflag=spasthma$y, vars=D2_mat, rho=RHO,
 alphas=1, betas=1, covars=Hay_mat, thetas=1)


###################################################
### code chunk number 82: sppa.Rnw:2626-2634
###################################################
Kdif<-function(Xppp, r, cr="border")
{
	k1<-Kest(Xppp[marks(Xppp)=="case"], r=r, correction=cr)
	k2<-Kest(Xppp[marks(Xppp)=="control"], r=r, correction=cr)

	res<-data.frame(r=r, D=k1[[cr]]-k2[[cr]])
	return(fv(res, valu="D", fname="D"))
}


###################################################
### code chunk number 83: sppa.Rnw:2642-2648 (eval = FALSE)
###################################################
r<-seq(0, .15, by=.01)

envKdif<-envelope(pppasthma, Kdif, r=r, nsim=99, cr="iso",  nrank=2,
savefuns=TRUE,
   simulate=expression(rlabel(pppasthma)))


###################################################
### code chunk number 87: sppa.Rnw:2672-2674
###################################################
khcases<-Kest(cases, r=r, correction="isotropic")
khcontrols<-Kest(controls, r=r, correction="isotropic")


###################################################
### code chunk number 88: sppa.Rnw:2687-2690
###################################################
niter<-99
T<-rep(NA, niter)
set.seed(1234)


###################################################
### code chunk number 92: sppa.Rnw:2716-2728 (eval = FALSE)
###################################################
khcasesrel<-matrix(NA, nrow=length(r), ncol=niter)
khcontrolsrel<-matrix(NA, nrow=length(r), ncol=niter)

for(i in 1:niter)
{
	pppasthma0<-rlabel(pppasthma)
	casesrel <- unmark(subset(pppasthma0, marks(pppasthma0) =="case"))
	controlsrel <- unmark(subset(pppasthma0, marks(pppasthma0) =="control"))

	khcasesrel[,i]<-Kest(casesrel, r=r, correction="isotropic")$iso
	khcontrolsrel[,i]<-Kest(controlsrel, r=r, correction="isotropic")$iso
}


###################################################
### code chunk number 93: sppa.Rnw:2742-2751
###################################################
#Compute diagonal of var-cov matrix of dif. K-functions
khcovdiag<-apply(khcasesrel-khcontrolsrel, 1,var)
simfuns<-as.data.frame(attr(envKdif, "simfuns"))[,-1]
khcovdiag<-apply(simfuns, 1, var)
#Test statistics
T0<-sum( ((khcases$iso-khcontrols$iso)/sqrt(khcovdiag))[-1])
T<-apply(simfuns, 2, function(X){
	sum((X/sqrt(khcovdiag))[-1])
})


###################################################
### code chunk number 95: sppa.Rnw:2758-2759
###################################################
pvalue<-(sum(T>T0)+1)/(niter+1)
pvalue

###################################################
### code chunk number 97: sppa.Rnw:2778-2799
###################################################
plot(envKdif)
lines(r, -1.96*sqrt(khcovdiag), lty=2)
lines(r, +1.96*sqrt(khcovdiag), lty=2)


###################################################
### code chunk number 101: sppa.Rnw:2940-2950
###################################################
glmasthma<-glm(y~HayFeverf, data=spasthma, family="binomial")
prob<-fitted(glmasthma)
weights<-exp(glmasthma$linear.predictors)
lambda0<-  interp.im (kcontrols, coords(cases)[,1], coords(cases)[,2])
lambda1<- weights[marks(pppasthma) =="case"]*lambda0

ratiocc<-ncases/ncontrols
kihnocov<-Kinhom(cases, ratiocc*lambda0, r=r)
kih<-Kinhom(cases, lambda1, r=r)


###################################################
### code chunk number 103: sppa.Rnw:3002-3025
###################################################
## #Relabel with weights
## rlabelp<-function(Xppp, ncases, prob)
## {
## 	idxsel<-sample(1:npoints(Xppp), ncases, prob=prob)
## 	marks(Xppp)<-"control"
## 	marks(Xppp)[idxsel]<-"case"
## 	return(Xppp)
## }

## #Compute K_{I,\lambda})
## KIlambda<-function(Xppp, r, cr="iso", weights, sigma)
## {
##     idxrel<-marks(Xppp)=="case"
##     casesrel<-unmark(Xppp[idxrel])
##     controlsrel<-unmark(Xppp[!idxrel])
##     lambda0rel<-interp.im(density(controlsrel, sigma), coords(casesrel)[,1],
##         coords(casesrel)[,2])
##     lambda1rel<-weights[idxrel]*lambda0rel
##     KI<-Kinhom(casesrel, lambda1rel, r=r, correction=cr)
##     res<-data.frame(r=r, KI=KI[[cr]])
## 
##    return(fv(res, valu="KI", fname="K_[I,lambda]"))
## }


###################################################
### code chunk number 104: sppa.Rnw:3040-3049 (eval = FALSE)
###################################################
## set.seed(4567)
## envKInocov<-envelope(pppasthma,KIlambda, r=r, cr="iso", weights=weights, 
##    sigma=bwasthma, nsim=99, nrank=2, savefuns=TRUE, 
##    simulate=expression(rlabelp(pppasthma, ncases=ncases, prob=rep(ratiocc, npoints(pppasthma)))) )

## envKIcov<-envelope(pppasthma,KIlambda, r=r, cr="iso", weights=weights, 
##    sigma=bwasthma, nsim=99, nrank=2, savefuns=TRUE, 
##    simulate=expression(rlabelp(pppasthma, ncases=ncases, prob=prob)) )



###################################################
### code chunk number 110: sppa.Rnw:3118-3120
###################################################
## kinhomrelnocov<-as.data.frame(attr(envKInocov, "simfuns"))[,-1]
## kinhomrel<-as.data.frame(attr(envKIcov, "simfuns"))[,-1]


###################################################
### code chunk number 114: sppa.Rnw:3140-3146
###################################################
## #Using envelope()
## kinhsdnocov<-apply(kinhomrelnocov, 1, sd)
## D0nocov<-sum(((envKInocov$obs-envKInocov$mmean)/kinhsdnocov)[-1])
## Dnocov<-apply(kinhomrelnocov, 2, 
##    function(X){ sum(((X-envKInocov$mmean)/kinhsdnocov)[-1])})
## pvaluenocov<-(sum(Dnocov>D0nocov)+1)/(niter+1)


###################################################
### code chunk number 116: sppa.Rnw:3157-3162
###################################################
## kinhsd<-apply(kinhomrel, 1, sd)
## D0<-sum( ((envKIcov$obs-envKIcov$mmean)/kinhsd)[-1])
## D<-apply(kinhomrel, 2, 
##    function(X){ sum(((X-envKIcov$mmean)/kinhsd)[-1])})
## pvalue<-(sum(D>D0)+1)/(niter+1)


###################################################
### code chunk number 118: sppa.Rnw:3196-3239
###################################################
## plot(r, envKInocov$obs-envKInocov$mmean, type="l", 
##    ylim= c(-0.06,  0.06),
##    xlab="s", ylab= expression(hat(k)[I][","][hat(lambda)]-"E[s]"),
##    main ="No covariates" )
## lines(r, envKInocov$lo-envKInocov$mmean, lty=2)
## lines(r, envKInocov$hi-envKInocov$mmean, lty=2)
## plot(r, envKIcov$obs-envKIcov$mmean, type="l", 
##    ylim= c(-0.06,  0.06),
##    xlab="s", ylab= expression(hat(k)[I][","][hat(lambda)]-"E[s]"),
##    main ="Adjusting for Hay Fever" )
## lines(r, envKIcov$lo-envKIcov$mmean, lty=2)
## lines(r, envKIcov$hi-envKIcov$mmean, lty=2)


