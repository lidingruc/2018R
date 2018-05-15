###################################################
### chunk number 1: preliminaries
###################################################
#line 2 "suppress2.Rnw"
options(width=60)
library(lattice)
options(show.signif.stars = FALSE)
set.seed(123454321)


###################################################
### chunk number 2: supp1
###################################################
#line 23 "suppress2.Rnw"
library(MASS)
correlation<-matrix(c(1, 0.5, 0, 0.5, 1.0, 0.5, 0, 0.5, 1.0), ncol=3, nrow=3, 
  dimnames=list(c("dv", "iv1", "iv2"), c("dv", "iv1", "iv2")))
correlation
set.seed(12346)
suppressor.set<-data.frame(mvrnorm(n=1000,rep(10, 3), correlation))


###################################################
### chunk number 3: supp2
###################################################
#line 35 "suppress2.Rnw"
m1<-lm(dv~iv1+iv2, suppressor.set)
summary(m1)


###################################################
### chunk number 4: supp3
###################################################
#line 43 "suppress2.Rnw"
library(yhat)
regr(m1)$Beta_Weights
regr(m1)$Structure_Coefficients
regr(m1)$Commonality_Data$CC


###################################################
### chunk number 5: supp4
###################################################
#line 82 "suppress2.Rnw"
m0<-lm(dv~iv1, suppressor.set)
anova(m0,m1)


