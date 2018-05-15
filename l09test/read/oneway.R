###################################################
### chunk number 1: preliminaries
###################################################
#line 4 "oneway.Rnw"
options(width=65)
library(lattice)
options(show.signif.stars = FALSE)


###################################################
### chunk number 2: fcrit
###################################################
#line 56 "oneway.Rnw"
qf(.95,4,45)


###################################################
### chunk number 3: pcalc1
###################################################
#line 95 "oneway.Rnw"
pf(7.5,4,30, lower=FALSE)
pf(1.765,3,30, lower=FALSE)


###################################################
### chunk number 4: SScompute
###################################################
#line 158 "oneway.Rnw"
set.seed(12346)
graph.aov<-data.frame(ethn=gl(3, 5), gre=c(rnorm(5, 500, 85), rnorm(5, 600, 85),
  rnorm(5, 700, 85)), ind=1:15)
par(mfrow=c(2,2))
plot(graph.aov$ind, graph.aov$gre, pch=(15+(graph.aov$ind>5)+(graph.aov$ind>10)), main="Index Plot")
plot(graph.aov$ind, graph.aov$gre, pch=(15+(graph.aov$ind>5)+(graph.aov$ind>10)), main="SS Total")
for (i in 1:15) lines(c(i,i), c(graph.aov$gre[i], mean(graph.aov$gre)))
abline(h=mean(graph.aov$gre))
plot(graph.aov$ind, graph.aov$gre, pch=(15+(graph.aov$ind>5)+(graph.aov$ind>10)), main="SS Error")
lines(c(1,5), c(485.01, 485.01))
lines(c(6,10), c(648.06, 648.06))
lines(c(11,15), c(716.92, 716.92))
for (i in 1:5) lines(c(i,i), c(graph.aov$gre[i], 485.01))
for (i in 6:10) lines(c(i,i), c(graph.aov$gre[i], 648.06))
for (i in 11:15) lines(c(i,i), c(graph.aov$gre[i], 716.92))


###################################################
### chunk number 5: dotplot1
###################################################
#line 194 "oneway.Rnw"
par(mfrow=c(1,1))
dot1<-data.frame(score1=c(rnorm(20, 20, 10), rnorm(20, 40, 10), rnorm(20, 60, 10)), group1=factor(rep(1:3, each=20)))
print(dotplot(group1~score1, dot1, type=c("p", "a")))


###################################################
### chunk number 6: dotplot2
###################################################
#line 202 "oneway.Rnw"
par(mfrow=c(1,1))
dot2<-data.frame(score2=c(rnorm(20, 20, 3), rnorm(20, 40, 3), rnorm(20, 60, 3)), group2=factor(rep(1:3, each=20)))
print(dotplot(group2~score2, dot2, type=c("p", "a")))


###################################################
### chunk number 7: aovdata
###################################################
#line 219 "oneway.Rnw"
ethdata<-data.frame(ethn=factor(rep(1:3, c(7,8,7))), score=c(8,7,6,7,9,11,13,11,13,14,18,17,14,12,15,14,13,15,15,20,21,22))
tapply(ethdata$score,ethdata$ethn,mean)
tapply(ethdata$score,ethdata$ethn,sd)


###################################################
### chunk number 8: homogtest
###################################################
#line 236 "oneway.Rnw"
fligner.test(score~ethn, ethdata)


###################################################
### chunk number 9: ethgraph
###################################################
#line 243 "oneway.Rnw"
boxplot(score~ethn, ethdata, ylab="Ethnicity", xlab="Score", horizontal=TRUE)


###################################################
### chunk number 10: ethgraph2
###################################################
#line 246 "oneway.Rnw"
print(dotplot(ethn~score, ethdata, type=c("p", "a")))


###################################################
### chunk number 11: ethgraph3
###################################################
#line 253 "oneway.Rnw"
library(gregmisc)
plotmeans(score~ethn, ethdata)


###################################################
### chunk number 12: aovrun
###################################################
#line 262 "oneway.Rnw"
m1<-aov(score~ethn, ethdata)
anova(m1)


###################################################
### chunk number 13: tukeyrun
###################################################
#line 288 "oneway.Rnw"
TukeyHSD(m1)


###################################################
### chunk number 14: tukeyplot
###################################################
#line 295 "oneway.Rnw"
plot(TukeyHSD(m1))


###################################################
### chunk number 15: hov1
###################################################
#line 303 "oneway.Rnw"
hovdata<-data.frame(score=c(1:10, 200), grp=factor(rep(1:2, c(5,6))))
fligner.test(score~grp, hovdata)
oneway.test(score~grp, hovdata)


###################################################
### chunk number 16: provedata
###################################################
#line 316 "oneway.Rnw"
newtrial<-data.frame(dv=c(1:9, 11), g1=rep(1:2, 5), g2=rep(1:2, each=5))


