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
### chunk number 7: 
###################################################
library(maptools)

nymap<-readShapePoly("NY8_utm18")


###################################################
### chunk number 8: 
###################################################
nymap$EXP<-nymap$POP8*sum(nymap$Cases)/sum(nymap$POP8)


###################################################
### chunk number 9: 
###################################################
library(spdep)

nynb<-poly2nb(nymap)


###################################################
### chunk number 10: 
###################################################
nyWBweights<-nb2WB(nynb)


###################################################
### chunk number 11: 
###################################################

d<-c(list(O=nymap$Cases, E=nymap$EXP), N=281, #nyWBweights,
   list(PCTAGE65P=nymap$PCTAGE65P, PCTOWNHOME=nymap$PCTOWNHOME,
   AVGIDIST=nymap$AVGIDIST))

inits1<-list(alpha=1, beta=c(0,0,0), u=rep(0, 281), v=rep(0, 281), precu=1,
   precv=1)

inits2<-list(alpha=10, beta=c(1,1,1), u=rep(1, 281), v=rep(1, 281), precu=.1,
   precv=.1)


###################################################
### chunk number 12: 
###################################################

library(R2WinBUGS)

mfile<-paste(getwd(), "/model.txt", sep="", collapse="")
tdir<-paste(getwd(), "/NYoutput", sep="", collapse="")
dir.create(tdir)


res<-bugs(data=c(d,nyWBweights), inits=list(inits1, inits2), 
   parameters.to.save=c("u", "v", "theta", "prob", "sigmau", "sigmav"),
   model.file = mfile,  working.directory = tdir, n.thin=3,
   n.chains = 2, n.iter = 6000, n.burnin = 3000)



###################################################
### chunk number 13: 
###################################################
nymap$prob<-res$mean$prob
nymap$theta<-res$mean$theta
nymap$u<-res$mean$u
nymap$v<-res$mean$v

logfile<-paste(getwd(), "/NYoutput/log.txt", sep="", collapse="")
reslog<-bugs.log(file=logfile)


###################################################
### chunk number 14: 
###################################################
library(RColorBrewer)
library(classInt)

thetaint<-classIntervals(nymap$theta, n=5, style="fixed", 
   fixedBreaks=c(0.02049,  0.1645,  0.7363 , 1.0630,  1.6390, 12.521))

cols<-brewer.pal(5, "Oranges")

cktheta<-list(labels=as.character(formatC(thetaint$brks, digits=3)),
  at=(0:5)/5, height=.5)

print(spplot(nymap, "theta", col.regions= cols,
   at=thetaint$brks, axes=TRUE, colorkey=cktheta,
   main="Smoothed Relative Risks"))


###################################################
### chunk number 15: 
###################################################
probint<-classIntervals(nymap$prob, n=4, style="fixed",
    fixedBreaks=c(0, .25, .5, .75, 1))
colsp<-brewer.pal(5, "Blues")

ckprob<-list(labels=as.character(formatC(probint$brks, digits=3)),
  at=(0:4)/4, height=.5)

print(spplot(nymap, "prob", col.regions= colsp,
   at=probint$brks, axes=TRUE, colorkey=ckprob,
   main="Probability map"))



###################################################
### chunk number 16: 
###################################################
sp2WB(map=nymap, file="NY_WB.txt")


###################################################
### chunk number 17: 
###################################################
bugs.data(d)
file.rename("data.txt", "dataNY.txt")
bugs.data(nyWBweights)
file.rename("data.txt", "data-spatialNY.txt")
bugs.data(inits1)
file.rename("data.txt", "inits1NY.txt")
bugs.data(inits2)
file.rename("data.txt", "inits2NY.txt")


