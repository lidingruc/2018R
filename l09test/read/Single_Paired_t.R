###################################################
### chunk number 1: preliminaries
###################################################
#line 4 "Single_Paired_t.Rnw"
options(width=60)
library(lattice)
library(gregmisc)
options(show.signif.stars = FALSE)
set.seed(123454321)


###################################################
### chunk number 2: IQintro
###################################################
#line 20 "Single_Paired_t.Rnw"
iqdata<-c(100, 110, 95, 98, 120, 118, 143, 94, 82, 114)
mean(iqdata)
sd(iqdata)


###################################################
### chunk number 3: iqdataci
###################################################
#line 31 "Single_Paired_t.Rnw"
plotmeans(iqdata~rep("IQ Data", 10))
abline(h=100)


###################################################
### chunk number 4: iqdataci2
###################################################
#line 43 "Single_Paired_t.Rnw"
iq2<-rnorm(10000, 100, 15)
iqdata2<-vector()
set.seed(12346)
for(i in 1:25){new.samp<-sample(iq2, 10, replace=T)
 iqdata2<-c(iqdata2,new.samp)}
plotmeans(iqdata2~as.factor(rep(1:25, each=10)))
abline(h=100)


###################################################
### chunk number 5: IQ2
###################################################
#line 196 "Single_Paired_t.Rnw"
iqdata<-c(100, 110, 95, 98, 120, 118, 143, 94, 82, 114)
t.test(iqdata, mu=100)


###################################################
### chunk number 6: IQ2
###################################################
#line 208 "Single_Paired_t.Rnw"
iqdata2<-c(100, 110, 145, 115, 120, 118, 143, 94, 122, 114)
t.test(iqdata2, mu=100)


###################################################
### chunk number 7: iqdataci3
###################################################
#line 219 "Single_Paired_t.Rnw"
plotmeans(c(iqdata, iqdata2)~rep(c("iqdata", "iqdata2"), each=10))
abline(h=100)


###################################################
### chunk number 8: paired1
###################################################
#line 258 "Single_Paired_t.Rnw"
pre<-c(60,70,65,68,72,49,59,82)
post<-c(71,79,82,78,85,60,63,97)
c(mean(pre), sd(pre), mean(post), sd(post))
plotmeans(c(pre, post)~rep(c("pre", "post"), each=8))


###################################################
### chunk number 9: pairedassum
###################################################
#line 277 "Single_Paired_t.Rnw"
print(p1<-xyplot(post~pre), split=c(1,1,2,1), more=TRUE)
print(tmd(p1), split=c(2,1,2,1))


###################################################
### chunk number 10: pairedassum2
###################################################
#line 285 "Single_Paired_t.Rnw"
pre1<-seq(10, 100, by=10)
post1<-seq(82, 100, length=10)
print(p2<-xyplot(post1~pre1), split=c(1,1,2,1), more=TRUE)
print(tmd(p2), split=c(2,1,2,1))


###################################################
### chunk number 11: pairedassum3
###################################################
#line 295 "Single_Paired_t.Rnw"
pre2<-1:10
post2<-c(11:19, 50)
print(p3<-xyplot(post2~pre2), split=c(1,1,2,1), more=TRUE)
print(tmd(p3), split=c(2,1,2,1))


###################################################
### chunk number 12: paired2
###################################################
#line 305 "Single_Paired_t.Rnw"
cbind(pre, post, diff=pre-post)
plotmeans(pre-post~rep("difference", 8))


###################################################
### chunk number 13: paired3
###################################################
#line 313 "Single_Paired_t.Rnw"
t.test(pre, post, paired=T)
t.test(pre-post, mu=0)


