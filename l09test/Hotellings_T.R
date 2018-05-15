###################################################
### chunk number 1: preliminaries
###################################################
#line 4 "Hotellings_T.Rnw"
options(width=60)
library(lattice)
library(MASS)
options(show.signif.stars = FALSE)
set.seed(123454321)


###################################################
### chunk number 2: datageneration
###################################################
#line 87 "Hotellings_T.Rnw"
library(ICSNP)
math.teach<-data.frame(teacher=factor(rep(1:2, c(3,6))), satis=c(1,3,2,4,6,6,5,5,4), know=c(3,7,2,6,8,8,10,10,6))
with(math.teach, tapply(know, teacher, mean))
with(math.teach, tapply(satis, teacher, mean))


###################################################
### chunk number 3: datagraphs
###################################################
#line 97 "Hotellings_T.Rnw"
par(mfrow=c(2,1))
boxplot(know~teacher, math.teach, 
main="Teacher Knowledge", horizontal=T)
boxplot(satis~teacher, math.teach, 
main="Teacher Satisfaction", horizontal=T)


###################################################
### chunk number 4: HotT2
###################################################
#line 108 "Hotellings_T.Rnw"
(m1<-with(math.teach, HotellingsT2(cbind(satis, know)~teacher)))


