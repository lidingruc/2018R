#### First Read in the Data
ingram<-read.table("http://faculty.smu.edu/kyler/courses/7309/ingram.txt", header=T)
library(sem)

#### Run a linear model with Attitude and SubNorm predicting Intent
summary(m1<-lm(Intent~Attitude+SubNorm, ingram))

#### Run this same linear model in SEM
ingram.simple<-specify.model()
   Attitude  ->  Intent,   gam13, NA
   SubNorm   ->  Intent,   gam23, NA
   Attitude <->  Attitude, psi11, 1
   SubNorm  <->  SubNorm,  psi22, 1
   Intent   <->  Intent,   psi33, NA
   
sem.simple<-sem(ingram.simple, cov(ingram), 60)
summary(sem.simple)

sd(ingram)^2
sd(m1$res)^2

#### Run a linear model with an interaction effect
summary(lm(Intent~Attitude*SubNorm, ingram))

#### Run this same linear model with interaction in SEM
ingram2<-data.frame(ingram)
ingram2$AttSub<-ingram2$Attitude*ingram2$SubNorm
ingram.interact<-specify.model()
   Attitude  ->  Intent,   gam13, NA
   SubNorm   ->  Intent,   gam23, NA
   Attitude <->  Attitude, psi11, 1
   SubNorm  <->  SubNorm,  psi22, 1
   Intent   <->  Intent,   psi33, NA
   AttSub    ->  Intent,   gam43, NA
   AttSub   <->  AttSub,   psi44, 1
   
sem.interact <-sem(ingram.interact, cov(ingram2), 60)
summary(sem.interact)

#### Run a simple path model in SEM
ingram.mediate<-specify.model()
   Attitude  ->  Intent,   gam13, NA
   SubNorm   ->  Intent,   gam23, NA
   Attitude  ->  SubNorm,  gam12, NA
   Attitude <->  Attitude, psi11, 1
   SubNorm  <->  SubNorm,  psi22, NA
   Intent   <->  Intent,   psi33, NA
   
sem.mediate<-sem(ingram.mediate, cov(ingram), 60)
summary(sem.mediate)

#### Complicated Path Model
ingram.model<-specify.model()
   Attitude  ->  Intent,   gam11, NA
   SubNorm   ->  Intent,   gam12, NA
   PBC       ->  Intent,   gam13, NA
   PBC       ->  Behavior, gam21, NA
   Intent    ->  Behavior, gam22, NA
   Attitude <->  Attitude, psi11, 1
   SubNorm  <->  SubNorm,  psi22, 1
   PBC      <->  PBC,      psi33, 1
   Intent   <->  Intent,   psi44, NA
   Behavior <->  Behavior, psi55, NA
   Attitude <->  SubNorm,  cor12, NA
   SubNorm  <->  PBC,      cor23, NA
   Attitude <->  PBC,      cor13, NA
   
sem.ingram<-sem(ingram.model, cor(ingram), 60)
summary(sem.ingram)

