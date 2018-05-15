###################################################
### chunk number 1: preliminaries
###################################################
#line 2 "MANOVA.Rnw"
options(width=60)
library(lattice)
options(show.signif.stars = FALSE)
set.seed(123454321)


###################################################
### chunk number 2: datarun
###################################################
#line 156 "MANOVA.Rnw"
manova.data<-data.frame(group=as.factor(rep(1:3, c(4,3,5))), y1=c(2,3,5,2,4,5,6,7,8,10,9,7), y2=c(3,4,4,5,8,6,7,6,7,8,5,6))
with(manova.data, tapply(y1, group, mean))
with(manova.data, tapply(y2, group, mean))


###################################################
### chunk number 3: datagraph
###################################################
#line 165 "MANOVA.Rnw"
par(mfrow=c(2,1))
boxplot(y1~group, manova.data, main="y1 Boxplot", horizontal=T)
boxplot(y2~group, manova.data, main="y2 Boxplot", horizontal=T)


###################################################
### chunk number 4: manovarun
###################################################
#line 174 "MANOVA.Rnw"
(m1<-manova(cbind(y1, y2)~group, manova.data))
summary(m1, test="Wilks")


