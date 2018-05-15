###################################################
### chunk number 1: fa1
###################################################
#line 6 "fascript.Rnw"
library(MBESS)
data(HS.data)
library(psych)


###################################################
### chunk number 2: fa2
###################################################
#line 13 "fascript.Rnw"
pc<-principal(HS.data[,7:11])
pc


###################################################
### chunk number 3: fa3
###################################################
#line 19 "fascript.Rnw"
pc$values


###################################################
### chunk number 4: fa4
###################################################
#line 24 "fascript.Rnw"
plot(pc$values)


###################################################
### chunk number 5: fa41
###################################################
#line 29 "fascript.Rnw"
fa.parallel(HS.data[,7:11])


###################################################
### chunk number 6: fa5
###################################################
#line 34 "fascript.Rnw"
pc$loadings


###################################################
### chunk number 7: fa6
###################################################
#line 39 "fascript.Rnw"
pc$communality


###################################################
### chunk number 8: fa7
###################################################
#line 44 "fascript.Rnw"
pc2<-principal(HS.data[,7:11], scores=T)
head(pc2$scores)


###################################################
### chunk number 9: fa8
###################################################
#line 50 "fascript.Rnw"
m1<-lm(pc2$scores~HS.data$numeric+HS.data$arithmet)
summary(m1)


###################################################
### chunk number 10: fa9
###################################################
#line 58 "fascript.Rnw"
new.data<-subset(HS.data, select=c(deduct, numeric, problemr, arithmet, paragrap, sentence, wordc, wordm))
cor(new.data)


###################################################
### chunk number 11: fa10
###################################################
#line 64 "fascript.Rnw"
pc8<-principal(new.data, nfactors=8)
pc8


###################################################
### chunk number 12: fa11
###################################################
#line 69 "fascript.Rnw"
plot(pc8$values, main="Scree Plot of Eigenvalues", xlab="Components", ylab="Eigenvalues")


###################################################
### chunk number 13: fa112
###################################################
#line 73 "fascript.Rnw"
fa.parallel(new.data)


###################################################
### chunk number 14: fa12
###################################################
#line 78 "fascript.Rnw"
pc8$loadings


###################################################
### chunk number 15: fa13
###################################################
#line 83 "fascript.Rnw"
pc8$communality


###################################################
### chunk number 16: fa14
###################################################
#line 88 "fascript.Rnw"
pc8.2<-principal(new.data, nfactors=8, scores=T)
head(pc8.2$scores)


###################################################
### chunk number 17: fa15
###################################################
#line 95 "fascript.Rnw"
pc2<-principal(new.data, nfactors=2, rotate="varimax")
pc2


###################################################
### chunk number 18: fa16
###################################################
#line 101 "fascript.Rnw"
pc2$loadings


###################################################
### chunk number 19: fa17
###################################################
#line 106 "fascript.Rnw"
pc2$communality


###################################################
### chunk number 20: fa18
###################################################
#line 112 "fascript.Rnw"
pc2.2<-principal(new.data, nfactors=2, scores=T)
head(pc2.2$scores)


###################################################
### chunk number 21: fa19
###################################################
#line 120 "fascript.Rnw"
efa<-factor.pa(HS.data[,7:11])
efa


###################################################
### chunk number 22: fa20
###################################################
#line 126 "fascript.Rnw"
efa$values


###################################################
### chunk number 23: fa21
###################################################
#line 130 "fascript.Rnw"
plot(efa$values, main="Scree Plot")


###################################################
### chunk number 24: fa22
###################################################
#line 134 "fascript.Rnw"
fa.parallel(HS.data[,7:11])


###################################################
### chunk number 25: fa23
###################################################
#line 139 "fascript.Rnw"
efa$loadings


###################################################
### chunk number 26: fa24
###################################################
#line 144 "fascript.Rnw"
efa$communality


###################################################
### chunk number 27: fa25
###################################################
#line 149 "fascript.Rnw"
efa2<-factor.pa(HS.data[,7:11], scores=T)
head(efa2$scores)


###################################################
### chunk number 28: fa26
###################################################
#line 155 "fascript.Rnw"
efa1<-lm(efa2$scores~HS.data$numeric+HS.data$arithmet)
summary(efa1)


###################################################
### chunk number 29: fa27
###################################################
#line 163 "fascript.Rnw"
library(GPArotation)
efa8<-factor.pa(new.data, nfactors=8, rotate="oblimin")
efa8


###################################################
### chunk number 30: fa28
###################################################
#line 170 "fascript.Rnw"
efa8$values


###################################################
### chunk number 31: fa29
###################################################
#line 175 "fascript.Rnw"
plot(efa8$values, main="Scree Plot")


###################################################
### chunk number 32: fa291
###################################################
#line 179 "fascript.Rnw"
fa.parallel(new.data)


###################################################
### chunk number 33: fa30
###################################################
#line 184 "fascript.Rnw"
efa8$loadings


###################################################
### chunk number 34: fa31
###################################################
#line 189 "fascript.Rnw"
efa8$communality


###################################################
### chunk number 35: fa32
###################################################
#line 194 "fascript.Rnw"
efa8.2<-factor.pa(new.data, nfactors=8, scores=T, rotate="oblimin")
head(efa8.2$scores)


###################################################
### chunk number 36: fa33
###################################################
#line 200 "fascript.Rnw"
efa2<-factor.pa(new.data, nfactors=2, rotate="oblimin")
efa2


###################################################
### chunk number 37: fa34
###################################################
#line 206 "fascript.Rnw"
efa2$values


###################################################
### chunk number 38: fa35
###################################################
#line 211 "fascript.Rnw"
efa2$loadings


###################################################
### chunk number 39: fa36
###################################################
#line 216 "fascript.Rnw"
efa2$communality


###################################################
### chunk number 40: fa37
###################################################
#line 221 "fascript.Rnw"
efa2.2<-factor.pa(new.data, nfactors=2, scores=T, rotate="oblimin")
head(efa2.2$scores)


###################################################
### chunk number 41: fa38
###################################################
#line 227 "fascript.Rnw"
efa2.promax<-factor.pa(new.data, nfactors=2, rotate="promax")
efa2.promax


