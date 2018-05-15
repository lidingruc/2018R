###################################################
### chunk number 1: preliminaries
###################################################
#line 4 "twoway.Rnw"
options(width=65)
library(lattice)
options(show.signif.stars = FALSE)


###################################################
### chunk number 2: twoway2
###################################################
#line 124 "twoway.Rnw"
twoway<-read.table("http://faculty.smu.edu/kyler/courses/7311/twoway1.txt", header=T)


###################################################
### chunk number 3: twoway3
###################################################
#line 127 "twoway.Rnw"
head(twoway)
str(twoway)


###################################################
### chunk number 4: twoway4
###################################################
#line 135 "twoway.Rnw"
twoway$gender<-factor(twoway$gender)
twoway$program<-factor(twoway$program)
str(twoway)
table(twoway$gender, twoway$program)


###################################################
### chunk number 5: twoway5
###################################################
#line 146 "twoway.Rnw"
barplot(tapply(twoway$gre, list(twoway$program, twoway$gender), mean), beside=T, col=rainbow(3))


###################################################
### chunk number 6: twoway6
###################################################
#line 154 "twoway.Rnw"
interaction.plot(twoway$gender, twoway$program, twoway$gre)


###################################################
### chunk number 7: twoway7
###################################################
#line 161 "twoway.Rnw"
m1<-aov(gre~gender+program, twoway)
summary(m1)
m2<-aov(gre~gender*program, twoway)
summary(m2)


###################################################
### chunk number 8: twoway8
###################################################
#line 171 "twoway.Rnw"
bartlett.test(gre~gender*program, twoway)
bartlett.test(gre~program*gender, twoway)
fligner.test(gre~gender*program, twoway)


###################################################
### chunk number 9: twoway9
###################################################
#line 180 "twoway.Rnw"
model.tables(m2, "means")


###################################################
### chunk number 10: twoway9
###################################################
#line 187 "twoway.Rnw"
TukeyHSD(m2)$'gender:program'


