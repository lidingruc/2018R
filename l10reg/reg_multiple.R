###################################################
### chunk number 1: preliminaries
###################################################
#line 2 "multiple2.Rnw"
options(width=60)
library(lattice)
options(show.signif.stars = FALSE)
set.seed(123454321)


###################################################
### chunk number 2: multi1
###################################################
#line 87 "multiple2.Rnw"
data1<-data.frame(x=1:10, y=c(1:5, 8:12), z=c(1:7, 11, 13, 19))
(m1<-lm(y~x+z, data1))
data2<-data.frame(x=scale(1:10), y=scale(c(1:5, 8:12)), z=scale(c(1:7, 11, 13, 19)))
(m2<-lm(y~x+z, data2))


###################################################
### chunk number 3: multi2
###################################################
#line 111 "multiple2.Rnw"
m1
cor(m1$fitted, data1$x)
cor(m1$fitted, data1$z)


