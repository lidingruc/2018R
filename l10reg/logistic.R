###################################################
### chunk number 1: preliminaries
###################################################
#line 2 "logistic2.Rnw"
options(width=60)
library(lattice)
options(show.signif.stars = FALSE)
set.seed(123454321)


###################################################
### chunk number 2: logistic1
###################################################
#line 53 "logistic2.Rnw"
mamm<-read.table("cohenex.txt", header=T)
attach(mamm)
head(mamm)


###################################################
### chunk number 3: logistic2
###################################################
#line 62 "logistic2.Rnw"
layout(matrix(1:2, ncol=2))
cdplot(factor(comply)~physrec)
cdplot(factor(comply)~knowledge)


###################################################
### chunk number 4: logistic3
###################################################
#line 71 "logistic2.Rnw"
layout(matrix(1:2, ncol=2))
cdplot(factor(comply)~benefits)
cdplot(factor(comply)~barriers)


###################################################
### chunk number 5: logistic4
###################################################
#line 80 "logistic2.Rnw"
m1<-glm(comply~physrec, family=binomial(link="logit"))
summary(m1)


###################################################
### chunk number 6: logistic5
###################################################
#line 89 "logistic2.Rnw"
exp(-1.8383)


###################################################
### chunk number 7: logistic6
###################################################
#line 93 "logistic2.Rnw"
exp(-1.8383)*exp(2.2882)


###################################################
### chunk number 8: logistic7
###################################################
#line 97 "logistic2.Rnw"
exp(-1.8383)/(1+exp(-1.8383))


###################################################
### chunk number 9: logistic8
###################################################
#line 101 "logistic2.Rnw"
(exp(-1.8383)*exp(2.2882))/(1+exp(-1.8383)*exp(2.2882))
-1.8383+2.2882
exp(0.45)/(1+exp(0.45))


###################################################
### chunk number 10: logistic9
###################################################
#line 113 "logistic2.Rnw"
correct.m1<-ifelse(m1$fitted<.5, 0, 1)
table(comply, correct.m1)
cbind(physrec, comply, logit=m1$linear, prob=m1$fitted)[1:6,]


###################################################
### chunk number 11: logistic10
###################################################
#line 122 "logistic2.Rnw"
m2<-glm(comply~knowledge, family=binomial(link="logit"))
summary(m2)


###################################################
### chunk number 12: logistic11
###################################################
#line 131 "logistic2.Rnw"
exp(-0.745)


###################################################
### chunk number 13: logistic111
###################################################
#line 135 "logistic2.Rnw"
0.3109+(-0.745*0.50)
exp(-0.0616)/(1+exp(-0.0616))


###################################################
### chunk number 14: logistic12
###################################################
#line 140 "logistic2.Rnw"
correct.m2<-ifelse(m2$fitted<.5, 0, 1)
table(comply, correct.m2)


###################################################
### chunk number 15: logistic13
###################################################
#line 148 "logistic2.Rnw"
m3<-glm(comply~physrec+knowledge, family=binomial(link="logit"))
summary(m3)


###################################################
### chunk number 16: logistic14
###################################################
#line 157 "logistic2.Rnw"
exp(2.278)


###################################################
### chunk number 17: logistic15
###################################################
#line 161 "logistic2.Rnw"
exp(-0.429)


###################################################
### chunk number 18: logistic16
###################################################
#line 165 "logistic2.Rnw"
-1.5679+2.2779+(-0.4286*0.70)
exp(0.40998)/(1+exp(0.40998))
table(comply, ifelse(m3$fitted<.5,0,1))


###################################################
### chunk number 19: logistic17
###################################################
#line 174 "logistic2.Rnw"
m4<-glm(comply~benefits+barriers, family=binomial(link="logit"))
summary(m4)


###################################################
### chunk number 20: logistic18
###################################################
#line 183 "logistic2.Rnw"
table(comply, ifelse(m4$fitted<.5,0,1))


###################################################
### chunk number 21: logistic19
###################################################
#line 187 "logistic2.Rnw"
-2.3664+(0.7061*3)+(-0.6036*4)
exp(-2.6625)/(1+exp(-2.6625))


###################################################
### chunk number 22: logistic192
###################################################
#line 192 "logistic2.Rnw"
-2.3664+(0.7061*5)+(-0.6036*1)
exp(0.5605)/(1+exp(0.5605))


###################################################
### chunk number 23: logistic20
###################################################
#line 200 "logistic2.Rnw"
plot(benefits~barriers)
symbols(barriers, benefits, circles=predict(m4, type="response"), add=T)


###################################################
### chunk number 24: logistic21
###################################################
#line 205 "logistic2.Rnw"
detach(mamm)


