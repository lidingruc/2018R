###################################################
### chunk number 1: preliminaries
###################################################
#line 2 "intro2.Rnw"
options(width=60)
library(lattice)
options(show.signif.stars = FALSE)
set.seed(123454321)


###################################################
### chunk number 2: readdata
###################################################
#line 37 "intro2.Rnw"
library(MASS)
set.seed(12346)
cov.mat<-matrix(c(225, 200, 30, 200, 225, 15, 30, 15, 225), 3, 3, dimnames=list(c("reading", "spelling", "math"), c("reading", "spelling", "math")))
studknow<-data.frame(mvrnorm(40, c(80, 78, 64), cov.mat))
head(studknow)


###################################################
### chunk number 3: readgraph
###################################################
#line 48 "intro2.Rnw"
print(splom(~ studknow, aspect = 1, type = c("g", "p")))


###################################################
### chunk number 4: studr
###################################################
#line 58 "intro2.Rnw"
cor(studknow)


###################################################
### chunk number 5: studknowablines
###################################################
#line 69 "intro2.Rnw"
par(mfrow=c(1,3))
plot(studknow$spelling, studknow$reading, main="r = 0.929")
abline(lm(studknow$reading~studknow$spelling))  
plot(studknow$math, studknow$spelling, main="r = -0.179")
abline(lm(studknow$spelling~studknow$math))
plot(studknow$reading, studknow$math, main="r = -0.200")
abline(lm(studknow$math~studknow$reading))


###################################################
### chunk number 6: rexample
###################################################
#line 82 "intro2.Rnw"
ex1<-c(1,2,3,4)
ex2<-c(1,2,3,5)
ex3<-c(1,4,5,2)


###################################################
### chunk number 7: rexample2
###################################################
#line 87 "intro2.Rnw"
par(mfrow=c(1,3))
plot(ex1, ex2, main="r = 0.983", pch=16)
abline(lm(ex2~ex1))
segments(ex1, fitted(lm(ex2~ex1)), ex1, ex2, col="red") 
plot(ex1, ex3, main="r = 0.283", pch=16)
abline(lm(ex3~ex1))
segments(ex1, fitted(lm(ex3~ex1)), ex1, ex3, col="red") 
plot(ex2, ex3, main="r = 0.107", pch=16)
abline(lm(ex3~ex2))
segments(ex2, fitted(lm(ex3~ex2)), ex2, ex3, col="red") 


###################################################
### chunk number 8: reg1
###################################################
#line 147 "intro2.Rnw"
new.data<-data.frame(dv=1:10, iv=c(1,3,2,5,4,6,6,8,9,11))
summary(m1<-lm(dv~iv, new.data))


###################################################
### chunk number 9: reg1cor
###################################################
#line 186 "intro2.Rnw"
cor(new.data)^2


###################################################
### chunk number 10: reg2cor
###################################################
#line 190 "intro2.Rnw"
cor(fitted(m1), new.data$dv)^2


###################################################
### chunk number 11: reg2
###################################################
#line 199 "intro2.Rnw"
plot(new.data$iv, new.data$dv, pch=16)
abline(lm(new.data$dv~new.data$iv))
segments(new.data$iv, fitted(m1), new.data$iv, new.data$dv, col="red") 


###################################################
### chunk number 12: reg21
###################################################
#line 208 "intro2.Rnw"
mean(new.data)


###################################################
### chunk number 13: reg22
###################################################
#line 211 "intro2.Rnw"
plot(new.data$iv, new.data$dv, pch=16)
abline(lm(new.data$dv~new.data$iv))
abline(v=5.5, col="red")
abline(h=5.5, col="red")


###################################################
### chunk number 14: reg3
###################################################
#line 221 "intro2.Rnw"
cbind(new.data, fit=m1$fit, resid=m1$resid)


###################################################
### chunk number 15: reg31
###################################################
#line 228 "intro2.Rnw"
par(mfrow=c(1,1))


###################################################
### chunk number 16: reg4
###################################################
#line 231 "intro2.Rnw"
par(mfrow=c(2,2))
plot(m1)


###################################################
### chunk number 17: reg5
###################################################
#line 242 "intro2.Rnw"
influence(m1)$coefficients


