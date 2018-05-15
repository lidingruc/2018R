###################################################
### chunk number 1: preliminaries
###################################################
#line 2 "Intro2.Rnw"
options(width=60)
library(nlme)
library(lme4)
library(lattice)
options(show.signif.stars = FALSE)


###################################################
### chunk number 2: mla1
###################################################
#line 134 "Intro2.Rnw"
sciach<-read.table("http://faculty.smu.edu/kyler/courses/7309/sciach.txt", header=T)
str(sciach)
sciach$GROUP<-factor(sciach$GROUP)


###################################################
### chunk number 3: mla2
###################################################
#line 146 "Intro2.Rnw"
mean(sciach$SCIENCE)
with(sciach, tapply(SCIENCE, GROUP, mean))
anova(aov(SCIENCE~GROUP, sciach))


###################################################
### chunk number 4: mla3
###################################################
#line 161 "Intro2.Rnw"
m0<-lme(SCIENCE~1, random=~1|GROUP, sciach)
summary(m0)


###################################################
### chunk number 5: mla4
###################################################
#line 169 "Intro2.Rnw"
cbind(means=with(sciach, tapply(SCIENCE, GROUP, mean)), coef(m0))


###################################################
### chunk number 6: mla5
###################################################
#line 176 "Intro2.Rnw"
par(mfrow=c(1,2))
boxplot(resid(m0)~GROUP, sciach, horizontal=T, main="Homogeneity of Variance")
qqnorm(resid(m0), main="QQplot for Null Model")


###################################################
### chunk number 7: mla6
###################################################
#line 303 "Intro2.Rnw"
m.lm<-lm(SCIENCE~URBAN, sciach)
summary(m.lm)


###################################################
### chunk number 8: mla7
###################################################
#line 311 "Intro2.Rnw"
plot(SCIENCE~URBAN, sciach)
abline(lm(SCIENCE~URBAN, sciach))


###################################################
### chunk number 9: mla8
###################################################
#line 319 "Intro2.Rnw"
m1<-lme(SCIENCE~URBAN, random=~1|GROUP, sciach)
summary(m1)


###################################################
### chunk number 10: mla9
###################################################
#line 327 "Intro2.Rnw"
anova(m0,m1)


