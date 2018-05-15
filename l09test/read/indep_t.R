###################################################
### chunk number 1: preliminaries
###################################################
#line 4 "indep_t.Rnw"
options(width=60)
library(lattice)
options(show.signif.stars = FALSE)
set.seed(123454321)


###################################################
### chunk number 2: firstind
###################################################
#line 68 "indep_t.Rnw"
cholest<-data.frame(chol=c(245,170,180,190,200,210,220,230,240,250,260,185,205,160,170,180,190,200,210,165),
  gender=rep(c("female", "male"), c(12,8)))
str(cholest)
boxplot(chol~gender, cholest, ylab="Cholesterol Score")


###################################################
### chunk number 3: normalt
###################################################
#line 78 "indep_t.Rnw"
par(mfrow=c(1,2))
qqnorm(cholest$chol[cholest$gender=="male"], main="QQNorm for the Males")
qqnorm(cholest$chol[cholest$gender=="female"], main="QQNorm for the Females")


###################################################
### chunk number 4: homv
###################################################
#line 92 "indep_t.Rnw"
var.test(chol~gender, cholest)


###################################################
### chunk number 5: firsttest
###################################################
#line 99 "indep_t.Rnw"
t.test(chol~gender, cholest)


###################################################
### chunk number 6: estrial
###################################################
#line 158 "indep_t.Rnw"
with(cholest, tapply(chol, gender, mean))
with(cholest, tapply(chol, gender, sd))
with(cholest, tapply(chol, gender, length))


