###################################################
### chunk number 1: 
###################################################
rm(list=ls())
.owidth <- getOption("width")
options("width"=70)
.PngNo <- 0


###################################################
### chunk number 2: afig eval=FALSE
###################################################
## .PngNo <- .PngNo + 1; file <- paste("Fig-", .PngNo, ".pdf", sep="")
## pdf(file=file, width = 3.5, height = 4.75, pointsize = 10)
## opar <- par(no.readonly = TRUE)
## par(mar=c(4,4,2,1)+0.1)


###################################################
### chunk number 3: bfig eval=FALSE
###################################################
## .PngNo <- .PngNo + 1; file <- paste("Fig-", .PngNo, ".pdf", sep="")
## pdf(file=file, width = 5, height = 2.5, pointsize = 8)
## opar <- par(no.readonly = TRUE)
## par(mar=c(4,4,2,1)+0.1)


###################################################
### chunk number 4: cfig eval=FALSE
###################################################
## .PngNo <- .PngNo + 1; file <- paste("Fig-", .PngNo, ".pdf", sep="")
## pdf(file=file, width = 4.5, height = 3.75, pointsize = 10)
## opar <- par(no.readonly = TRUE)
## par(mar=c(4,4,2,1)+0.1)


###################################################
### chunk number 5: c1fig eval=FALSE
###################################################
## .PngNo <- .PngNo + 1; file <- paste("Fig-", .PngNo, ".pdf", sep="")
## pdf(file=file, width = 7, height = 5, pointsize = 16)
## opar <- par(no.readonly = TRUE)
## par(mar=c(4,4,2,1)+0.1)


###################################################
### chunk number 6: zfig eval=FALSE
###################################################
## par(opar)
## dev.null <- dev.off()
## cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### chunk number 7:  eval=FALSE
###################################################
## library(sampling)
## data(MU284)
## 
## MU284<-MU284[order(MU284$REG),]
## MU284$LABEL<-1:284
## 
## summary(MU284)


###################################################
### chunk number 8: 
###################################################
library(sampling)
data(MU284)

MU284<-MU284[order(MU284$REG),]
MU284$LABEL<-1:284


###################################################
### chunk number 9: 
###################################################
library(maptools)
swmap<-readShapePoly("Sweden_municipality")#, ID="KOD83_91")
#summary(swmap$"KOD83_91")

library(maptools)
swmap2<-unionSpatialPolygons(swmap, swmap$"KOD83_91")
swmap2<-SpatialPolygonsDataFrame(swmap2, data.frame(REG=as.factor(MU284$REG)), 
   match.ID =FALSE)

swmap3<-unionSpatialPolygons(swmap2, swmap2$REG)
swmap3<-SpatialPolygonsDataFrame(swmap3, data.frame(REG=as.factor(1:8)), match.ID =FALSE)


###################################################
### chunk number 10: 
###################################################
library(RColorBrewer)
cols<-brewer.pal(8, "Pastel1")

print(spplot(swmap3, "REG", col.regions=cols))


###################################################
### chunk number 11: 
###################################################
#Neighbours
library(spdep)

nb<-poly2nb(swmap3)
W<-nb2mat(nb, style="B")



###################################################
### chunk number 12: 
###################################################
#Select a few areas (Estimation of the national revenues)
N<-284 #Total number of municipalities
n<-32    #~1% Sample size
nreg<-length(unique(MU284$REG))

#Simple random sampling without replacement
set.seed(1)
smp<-srswor(n, N)
dsmp<-MU284[smp==1,]

table(dsmp$REG)


###################################################
### chunk number 13: 
###################################################
#Multi-stage random sampling
set.seed(1)
smpcl<-mstage(MU284, stage=list("cluster","cluster"), 
   varnames=list("REG", "LABEL"),
   size=list(8, rep(4, 8)), method="srswor")

dsmpcl<-MU284[smpcl[[2]]$LABEL,]

table(dsmpcl$REG)


###################################################
### chunk number 14: 
###################################################
#Multi-stage random sampling WITH MISSING AREAS
set.seed(1)
smpcl2<-mstage(MU284, stage=list("cluster","cluster"), 
   varnames=list("REG", "LABEL"),
   size=list(4, rep(8, 8)), method="srswor")

dsmpcl2<-MU284[smpcl2[[2]]$LABEL,]

table(dsmpcl2$REG)


###################################################
### chunk number 15: 
###################################################
#plot(MU284$LABEL, MU284$RMT85)
plot(dsmp$LABEL, dsmp$RMT85, pch=19, xlab="MUNICIPALITY", ylab="RMT85" )
points(dsmpcl$LABEL-.25, dsmpcl$RMT85, pch=19, col="red")
points(dsmpcl2$LABEL+.25, dsmpcl2$RMT85, pch=19, col="lightblue")
#abline(h=mean(MU284$RMT85))

lreg<-as.numeric(by(MU284$REG, MU284$REG,length))
for(i in 1:7)
	abline(v=sum(lreg[1:i]), lty=2)

legend(150, 800, c("SRSWOR", "CLSWOR", "CLSWOR2"), pch=rep(19, 3), 
   col=c("black", "red", "lightblue"))



###################################################
### chunk number 16: 
###################################################
library(survey)
RMT85<-sum(MU284$RMT85)
RMT85REG<-as.numeric(by(MU284$RMT85, MU284$REG, sum))

#	dest<-sum(dsmp$RMT85/(n/N))

#	destcl<-sum(dsmpcl$RMT85/smpcl[[2]]$Prob)

#	destcl2<-sum(dsmpcl2$RMT85/smpcl2[[2]]$Prob)


###################################################
### chunk number 17: 
###################################################
library(survey)

svy<-svydesign(~1, data=dsmp, fpc=rep(284, n))
dest<-svytotal(~RMT85, svy)


###################################################
### chunk number 18: 
###################################################
fpc<-lreg[dsmpcl$REG]

svycl<-svydesign(id=~1, strata=~REG, data=dsmpcl, fpc=fpc)
destcl<-svytotal(~RMT85, svycl)


###################################################
### chunk number 19: 
###################################################
fpc2<-lreg[dsmpcl2$REG]
svycl2<-svydesign(id=~1, strata=~REG, data=dsmpcl2, fpc=fpc2)
destcl2<-svytotal(~RMT85, svycl2)#Wrong estimator



###################################################
### chunk number 20: 
###################################################
#Estimation of domains
svyby(~RMT85, ~REG, svy, svytotal)


###################################################
### chunk number 21: 
###################################################
svyby(~RMT85, ~REG, svycl, svytotal)


###################################################
### chunk number 22: 
###################################################
svyby(~RMT85, ~REG, svycl2, svytotal)


###################################################
### chunk number 23: 
###################################################

pop.totals=c(`(Intercept)`=N, ME84=sum(MU284$ME84))

svygreg<-calibrate(svy, ~ME84, calfun="linear",
   population=pop.totals )
svytotal(~RMT85, svygreg)

svygregcl<-calibrate(svycl, ~ME84, calfun="linear",
   population=pop.totals )
svytotal(~RMT85, svygregcl)

svygregcl2<-calibrate(svycl2, ~ME84, calfun="linear",
   population=pop.totals )
svytotal(~RMT85, svygregcl2)



###################################################
### chunk number 24: 
###################################################
plot(MU284$ME84, MU284$RMT85)
plot(MU284$ME84, MU284$RMT85, xlim=c(0, 10000))

survlm<-lm(RMT85~ME84, dsmp)
survglm<-svyglm(RMT85~ME84, svy)

summary(survlm)
summary(survglm)


###################################################
### chunk number 25: 
###################################################
library(SAE)


destmean<-svyby(~RMT85, ~REG, svycl, svymean)

Y<-matrix(destmean[,2], ncol=1)
sigma2i<-matrix(destmean[,3], ncol=1)^2
X<-matrix(as.numeric(by(MU284$ME84, MU284$REG, mean)), ncol=1)

ebluparea<-EBLUP.area(Y, cbind(1, X),  sigma2i, 8)

#Error
print(sum((destmean[,2]-(RMT85REG/lreg))^2))
print(sum((ebluparea$EBLUP-(RMT85REG/lreg))^2))

print(ebluparea$randeff[,1])


###################################################
### chunk number 26: 
###################################################

plot(1:8, RMT85REG/lreg, xlab="REGION", ylab="AVG RMT85", ylim=c(60, 1800))
points(1:8, destmean[,2], pch=19, col="lightblue")
points(1:8, ebluparea$EBLUP, pch=19, col="pink")

legend(2, 1500, c("TRUE AVG", "DIRECT", "EBLUP"), 
   pch=c(1,19,19), col=c("black", "lightblue", "pink"))



###################################################
### chunk number 27:  eval=FALSE
###################################################
## W<-diag(0, 8)
## for(i in 2:7)
## {
## 	W[i-1, i]<-1
## 	W[i, i-1]<-1
## 	W[i+1, i]<-1
## 	W[i, i+1]<-1
## }
## 
## W[ W>0 | W%*%W>0]<-1
## diag(W)<-0


###################################################
### chunk number 28: 
###################################################
moran.test(Y, nb2listw(nb), alternative ="two.sided")


sebluparea<-SEBLUP.area(Y, matrix(cbind(1, X), ncol=2),  sigma2i, 8, 
   W, init=c(0, ebluparea$sigma2u) )
#sebluparea<-EBLUP.area(Y, matrix(cbind(1, X), ncol=2),  sigma2i, 8)

#Error
#print(sum((destmean[,2]-(RMT85REG/lreg))^2))
#print(sum((sebluparea$SEBLUP-(RMT85REG/lreg))^2))

print(paste("Rho:", sebluparea$rho,  "s.d.", sqrt(sebluparea$varsigmarho[2,2]),
   sep=" ", collapse=" ") )

print(sebluparea$randeff[,1])


###################################################
### chunk number 29: 
###################################################

plot(1:8, RMT85REG/lreg, xlab="REGION", ylab="AVG RMT85", ylim=c(60, 1800))
points(1:8, destmean[,2], pch=19, col="lightblue")
points(1:8, ebluparea$EBLUP, pch=19, col="pink")
points(1:8, sebluparea$SEBLUP, pch=19, col="black")

legend(2, 1500, c("TRUE AVG", "DIRECT", "EBLUP", "SEBLUP"), 
   pch=c(1,19,19,19), col=c("black", "lightblue", "pink", "black"))



###################################################
### chunk number 30: 
###################################################
swmap3$RMT85REGMEAN<-RMT85REG/lreg
swmap3$DESTMEAN<-destmean[,2]
swmap3$EBLUP<-ebluparea$EBLUP
swmap3$SEBLUP<-sebluparea$SEBLUP


###################################################
### chunk number 31: 
###################################################

print(spplot(swmap3, c("RMT85REGMEAN", "DESTMEAN", "EBLUP", "SEBLUP"),
    at=c(0,100, 150, 200, 250, 300, 500, 1000, 1500, 2000), 
    col.regions=brewer.pal(9, "Blues")) )



###################################################
### chunk number 32: 
###################################################
AEMSE<-function(est, actual) 
{
	formatC( sqrt( sum( (est-actual)^2))/length(actual), digits=5)
}


###################################################
### chunk number 33:  eval=FALSE
###################################################
## #Error of the estimation
## ermse<-rep(NA,3)
## 
## ermse[1]<-((dest-RMT85)^2)/RMT85
## ermse[2]<-((destcl-RMT85)^2)/RMT85
## ermse[3]<-((destcl2-RMT85)^2)/RMT85
## 
## print(ermse)


