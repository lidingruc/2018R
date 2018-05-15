###################################################
### chunk number 1: preliminaries
###################################################
#line 4 "chisq.Rnw"
options(width=60)
library(lattice)
library(MASS)
options(show.signif.stars = FALSE)
set.seed(123454321)


###################################################
### chunk number 2: chicrit
###################################################
#line 114 "chisq.Rnw"
qchisq(0.95,1)


###################################################
### chunk number 3: datastart
###################################################
#line 125 "chisq.Rnw"
haireye<-data.frame(eyes=rep(c("blue", "brown"), c(52,62)),
  hair=rep(c("fair", "dark", "fair", "dark"), c(38,14,11,51)))
table(haireye)


###################################################
### chunk number 4: chisqrun
###################################################
#line 134 "chisq.Rnw"
chisq.test(table(haireye), correct=F)
chisq.test(table(haireye), correct=F)$expected
table(haireye)


###################################################
### chunk number 5: modelresid
###################################################
#line 160 "chisq.Rnw"
chisq.test(table(haireye), correct=F)$resid


###################################################
### chunk number 6: newhaireye
###################################################
#line 171 "chisq.Rnw"
set.seed(12346)
haireye$gender<-sample(0:1, 114, replace=T)
table(haireye)


###################################################
### chunk number 7: newhaireye2
###################################################
#line 180 "chisq.Rnw"
(mnew<-chisq.test(table(haireye), correct=F))
mnew$resid


###################################################
### chunk number 8: chiobs
###################################################
#line 199 "chisq.Rnw"
observed<-matrix(c(32,24,265,199,391,287), nrow=3, byrow=T)
chisq.test(observed, correct=F)
cbind(observed,chisq.test(observed)$resid)


