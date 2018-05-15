library(quantreg)
library(MASS)
library(psych)

########## EXAMINING RR METHODS USING REAL DATA ###############
crime<-read.table("http://www.ats.ucla.edu/stat/R/dae/crime.csv", sep=",", header = TRUE) 
attach(crime)
### taking out DC ####
crime2<-crime[ sid !=51, ]
head(crime2)

#### Running OLS regression ####
m1 <- lm(crime ~ poverty + single, data=crime2)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0)) 
plot(m1, las = 1) 
par(opar) 
### immediately see that 25 is an outlier, but also something happening with 9 ####
influence(m1)$coefficients


crime2[9,]

crime2[25,]

### making new variable with all of the cooks.distances (MASS needed) ###
d1<-cooks.distance(m1)
### making new variable with all of the standard residuals###
r<-stdres(m1)
### putting information into easy to see matrix ####
a<-cbind(crime2, d1, r)
### The lowest value that Cook's D can assume is zero, and the higher the Cook's D is, the more influential the point. The conventional cut-off point is 4/n, where n is the number of observations in the data set. So, we are taking the states which qualify under this cut-off point ####
a[d1>4/50,]
### From this we see that 9, 25, and 49 have some irregularity ####

### now we will also look at the absolute value of the residuals - then take out the top 10 states with the largest residuals (note 49 now not included) ####
rabs<-abs(r)
a<-cbind(crime2, d1, r, rabs)
asorted<-a[order(-rabs), ]
asorted[1:10,]

#### Let's run the robust regression using m-estimator method and compare to OLS #### 
m1.huber<-rlm(crime ~poverty + single, data=crime2)
summary(m1.huber)

### calculating t-statistic####
qt(p=.05, df=47) 
### now isolating huber weights and looking at top 10 - keep in mind that OLS uses weights of 1, so the closer the weights are to 1, the more the results will be similar to OLS ####
a<-cbind(crime2, d1, r, m1.huber$w)
asorted<-a[order(m1.huber$w),]
asorted[1:10,]

### Comparing the results of OLS regression model and RLM (m-estimator) model using huber statistic###
m1<-lm(crime ~poverty + single, data=crime2)
summary(m1)

m1.huber<-rlm(crime ~poverty + single, data=crime2)
summary(m1.huber)

### Can also try with Least-Trim Squares (note: cant do summary)#####
m1.LTS<-ltsreg(crime~poverty+single,data=crime2)
m1.LTS

###### Regression Quantile Model ###########
m1.LAD<-rq(crime~poverty+single,data=crime2)
summary(m1.LAD)

######## Bounded influence using bisqrd #############
m1.bi<-rlm(crime~poverty+single,data=crime2,method='MM')
summary(m1.bi)




##################### LOOOKING AT METHODS GRAPHICALLY ###########
#### Reading in outlier data ######
fivpercent<-read.csv("http://faculty.smu.edu/kyler/courses/7314/student/fivepercent.csv",header=T)
tenpercent<-read.csv("http://faculty.smu.edu/kyler/courses/7314/student/tenpercent.csv",header=T)
fifpercent<-read.csv("http://faculty.smu.edu/kyler/courses/7314/student/fifpercent.csv",header=T)
twepercent<-read.csv("http://faculty.smu.edu/kyler/courses/7314/student/twentypercent.csv",header=T)
#### Creating OLS regression lines for comparison #####
m.fivpercent<-lm(taks~mathgrade,data=fivpercent)
m.tenpercent<-lm(taks~mathgrade,data=tenpercent)
m.fifpercent<-lm(taks~mathgrade,data=fifpercent)
m.twepercent<-lm(taks~mathgrade,data=twepercent)
#### Looking at the differences graphically ######
par(mfrow=c(2,2))
plot(fivpercent$mathgrade,fivpercent$taks,xlab="Math Grade",ylab="TAKS Score");abline(m.fivpercent,col="blue")
plot(tenpercent$mathgrade,tenpercent$taks,xlab="Math Grade",ylab="TAKS Score");abline(m.tenpercent,col="red")
plot(fifpercent$mathgrade,fifpercent$taks,xlab="Math Grade",ylab="TAKS Score");abline(m.fifpercent,col="brown")
plot(twepercent$mathgrade,twepercent$taks,xlab="Math Grade",ylab="TAKS Score");abline(m.twepercent,col="green")
##### Plotting them all on one ########
par(mfrow=c(1,1))
plot(fivpercent$mathgrade,fivpercent$taks,xlab="Math Grade",ylab="TAKS Score");abline(m.fivpercent,col="blue");abline(m.tenpercent,col="red");abline(m.fifpercent,col="brown");abline(m.twepercent,col="green")


################## Huber's M-estimator RLM model ###############
hm.fivpercent<-rlm(taks~mathgrade,data=fivpercent)
hm.tenpercent<-rlm(taks~mathgrade,data=tenpercent)
hm.fifpercent<-rlm(taks~mathgrade,data=fifpercent)
hm.twepercent<-rlm(taks~mathgrade,data=twepercent)
#### Looking at the differences graphically ######
par(mfrow=c(2,2))
plot(fivpercent$mathgrade,fivpercent$taks,main="Five Percent",xlab="Math Grade",ylab="TAKS Score");abline(hm.fivpercent,col="blue");abline(m.fivpercent)
plot(tenpercent$mathgrade,tenpercent$taks,main="Ten Percent",xlab="Math Grade",ylab="TAKS Score");abline(hm.tenpercent,col="red");abline(m.tenpercent)
plot(fifpercent$mathgrade,fifpercent$taks,main="Fifteen Percent",xlab="Math Grade",ylab="TAKS Score");abline(hm.fifpercent,col="brown");abline(m.fifpercent)
plot(twepercent$mathgrade,twepercent$taks,main="Twenty Percent",xlab="Math Grade",ylab="TAKS Score");abline(hm.twepercent,col="green");abline(m.twepercent)
##### Plotting them all on one ########
par(mfrow=c(1,1))
plot(fivpercent$mathgrade,fivpercent$taks,main="M-estimator",xlab="Math Grade",ylab="TAKS Score");abline(hm.fivpercent,col="blue");abline(hm.tenpercent,col="red");abline(hm.fifpercent,col="brown");abline(hm.twepercent,col="green")

################ Regression Quantile model ####################
rm.fivpercent<-rq(taks~mathgrade,data=fivpercent)
rm.tenpercent<-rq(taks~mathgrade,data=tenpercent)
rm.fifpercent<-rq(taks~mathgrade,data=fifpercent)
rm.twepercent<-rq(taks~mathgrade,data=twepercent)
#### Looking at the differences graphically ######
par(mfrow=c(2,2))
plot(fivpercent$mathgrade,fivpercent$taks,main="Five Percent",xlab="Math Grade",ylab="TAKS Score");abline(rm.fivpercent,col="blue");abline(m.fivpercent)
plot(tenpercent$mathgrade,tenpercent$taks,main="Ten Percent",xlab="Math Grade",ylab="TAKS Score");abline(rm.tenpercent,col="red");abline(m.tenpercent)
plot(fifpercent$mathgrade,fifpercent$taks,main="Fifteen Percent",xlab="Math Grade",ylab="TAKS Score");abline(rm.fifpercent,col="brown");abline(m.fifpercent)
plot(twepercent$mathgrade,twepercent$taks,main="Twenty Percent",xlab="Math Grade",ylab="TAKS Score");abline(rm.twepercent,col="green");abline(m.twepercent)
##### Plotting them all on one ########
par(mfrow=c(1,1))
plot(fivpercent$mathgrade,fivpercent$taks,xlab="Math Grade",ylab="TAKS Score");abline(rm.fivpercent,col="blue");abline(rm.tenpercent,col="red");abline(rm.fifpercent,col="brown");abline(rm.twepercent,col="green")

################ Least Trim Squares (LTS) model ################
ltsm.fivpercent<-ltsreg(taks~mathgrade,data=fivpercent)
ltsm.tenpercent<-ltsreg(taks~mathgrade,data=tenpercent)
ltsm.fifpercent<-ltsreg(taks~mathgrade,data=fifpercent)
ltsm.twepercent<-ltsreg(taks~mathgrade,data=twepercent)
#### Looking at the differences graphically ######
par(mfrow=c(2,2))
plot(fivpercent$mathgrade,fivpercent$taks,main="Five Percent",xlab="Math Grade",ylab="TAKS Score");abline(ltsm.fivpercent,col="blue");abline(m.fivpercent)
plot(tenpercent$mathgrade,tenpercent$taks,main="Ten Percent",xlab="Math Grade",ylab="TAKS Score");abline(ltsm.tenpercent,col="red");abline(m.tenpercent)
plot(fifpercent$mathgrade,fifpercent$taks,main="Fifteen Percent",xlab="Math Grade",ylab="TAKS Score");abline(ltsm.fifpercent,col="brown");abline(m.fifpercent)
plot(twepercent$mathgrade,twepercent$taks,main="Twenty Percent",xlab="Math Grade",ylab="TAKS Score");abline(ltsm.twepercent,col="green");abline(m.twepercent)
##### Plotting them all on one ########
par(mfrow=c(1,1))
plot(fivpercent$mathgrade,fivpercent$taks,main="LTS",xlab="Math Grade",ylab="TAKS Score");abline(ltsm.fivpercent,col="blue");abline(ltsm.tenpercent,col="red");abline(ltsm.fifpercent,col="brown");abline(ltsm.twepercent,col="green")

############# Bounded Influence using Bsqrd estimates ############
bim.fivpercent<-rlm(taks~mathgrade,data=fivpercent,method='MM')
bim.tenpercent<-rlm(taks~mathgrade,data=tenpercent,method='MM')
bim.fifpercent<-rlm(taks~mathgrade,data=fifpercent,method='MM')
bim.twepercent<-rlm(taks~mathgrade,data=twepercent,method='MM')
#### Looking at the differences graphically ######
par(mfrow=c(2,2))
plot(fivpercent$mathgrade,fivpercent$taks,main="Five Percent",xlab="Math Grade",ylab="TAKS Score");abline(bim.fivpercent,col="blue");abline(m.fivpercent)
plot(tenpercent$mathgrade,tenpercent$taks,main="Ten Percent",xlab="Math Grade",ylab="TAKS Score");abline(bim.tenpercent,col="red");abline(m.tenpercent)
plot(fifpercent$mathgrade,fifpercent$taks,main="Fifteen Percent",xlab="Math Grade",ylab="TAKS Score");abline(bim.fifpercent,col="brown");abline(m.fifpercent)
plot(twepercent$mathgrade,twepercent$taks,main="Twenty Percent",xlab="Math Grade",ylab="TAKS Score");abline(bim.twepercent,col="green");abline(m.twepercent)
##### Plotting them all on one ########
par(mfrow=c(1,1))
plot(fivpercent$mathgrade,fivpercent$taks,main="M-estimator",xlab="Math Grade",ylab="TAKS Score");abline(bim.fivpercent,col="blue");abline(bim.tenpercent,col="red");abline(bim.fifpercent,col="brown");abline(bim.twepercent,col="green")


############## Can also do winzoring which is like trimming but instead regresses outliers toward quantile means ############
data(twepercent)

y <- winsor(twepercent$taks)
describe(y)
######## comparing to original data #########
describe(twepercent$taks)

#### also winzoring mathgrade variable ####### 
z<-winsor(twepercent$mathgrade)
describe(z)
describe(twepercent$mathgrade)
### putting into one dataframe - if needed #####
yz<-data.frame(y,z)
### comparing OLS regression with winsor variables with OLS regression #####
m.win<-lm(y~z);summary(m.win)
summary(m.twepercent)