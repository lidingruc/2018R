#### First Read in the Data
ingram<-read.table("http://faculty.smu.edu/kyler/courses/7309/ingram.txt", header=T)
library(sem)

#### Run a linear model with Attitude and SubNorm predicting Intent
summary(m1<-lm(Intent~Attitude+SubNorm, ingram))

#### Run this same linear model in SEM
ingram.simple<-specifyModel()
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
ingram.interact<-specifyModel()
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
ingram.mediate<-specifyModel()
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


#Loehlin problem 2.5 
obs.var2.5 = c('Ach1',  'Ach2',  'Amb1',  'Amb2',  'Amb3')
R.prob2.5 = matrix(c(
  1.00 ,  .60  , .30,  .20,   .20,                  
  .60,  1.00,   .20,   .30,   .10, 
  .30,   .20,  1.00,   .70,   .60 ,   
  .20,   .30,   .70,  1.00,   .50, 
  .20,   .10,   .60,  .50,  1.00), ncol=5,byrow=TRUE) 

#correlated factors structure (ambition <-> Achievement) 
model2.5=matrix(c(
  'Ambit ->  Amb1',      'a', NA,
  'Ambit -> Amb2' ,      'b', NA,
  'Ambit -> Amb3' ,      'c', NA,
  'Achieve -> Ach1',     'd', NA,
  'Achieve -> Ach2',     'e', NA,
  'Ambit <-> Achieve',   'f', NA,
  'Amb1 <-> Amb1' ,      'u', NA,
  'Amb2 <-> Amb2' ,      'v', NA,
  'Amb3 <-> Amb3' ,      'w', NA,
  'Ach1 <-> Ach1' ,      'x', NA,
  'Ach2 <-> Ach2' ,      'y', NA,
  'Achieve <-> Achieve',  NA, 1,
  'Ambit <-> Ambit',      NA, 1),
  ncol=3, byrow=TRUE)

sem2.5= sem(model2.5, R.prob2.5,60, obs.var2.5)
summary(sem2.5,digits=3)

#causal structure with errors in achievement
#ambition -> achievement
 model2.51=matrix(c(
    	'Ambit ->  Amb1',      'a', NA,
     	'Ambit -> Amb2' ,      'b', NA,
     	'Ambit -> Amb3' ,      'c', NA,
     	'Achieve -> Ach1',     'd', NA,
     	'Achieve -> Ach2',     'e', NA,
     	'Ambit -> Achieve',   'f', NA,
     	'Amb1 <-> Amb1' ,      'u', NA,
     	'Amb2 <-> Amb2' ,      'v', NA,
     	'Amb3 <-> Amb3' ,      'w', NA,
     	'Ach1 <-> Ach1' ,      'x', NA,
     	'Ach2 <-> Ach2' ,      'y', NA,
     	'Achieve <-> Achieve',  NA, 1,
     	'Ambit <-> Ambit',      NA, 1),
     	ncol=3, byrow=TRUE)

sem2.51= sem(model2.51,R.prob2.5,100, obs.var2.5)
summary(sem2.51,digits=3)

#causal structure with errors in achievement
#ambition -> achievement

model2.52=matrix(c(
  'Ambit ->  Amb1',      'a', NA,
  'Ambit -> Amb2' ,      'b', NA,
  'Ambit -> Amb3' ,      'c', NA,
  'Achieve -> Ach1',     'd', NA,
  'Achieve -> Ach2',     'e', NA,
  'Ambit -> Achieve',   'f', NA,
  'Amb1 <-> Amb1' ,      'u', NA,
  'Amb2 <-> Amb2' ,      'v', NA,
  'Amb3 <-> Amb3' ,      'w', NA,
  'Ach1 <-> Ach1' ,      'x', NA,
  'Ach2 <-> Ach2' ,      'y', NA,
  'Achieve <-> Achieve', 'z', NA,
  'Ambit <-> Ambit',      NA, 1),
  ncol=3, byrow=TRUE)

sem2.52= sem(model2.52,R.prob2.5,100, obs.var2.5)
summary(sem2.52,digits=3)

#causal structure with errors in achievement
#ambition -> achievement
#fix achievement to Loehlin answer

model2.53=matrix(c(
  'Ambit ->  Amb1',      'a', NA,
  'Ambit -> Amb2' ,      'b', NA,
  'Ambit -> Amb3' ,      'c', NA,
  'Achieve -> Ach1',     'd', NA,
  'Achieve -> Ach2',     'e', NA,
  'Ambit -> Achieve',   'f', NA,
  'Amb1 <-> Amb1' ,      'u', NA,
  'Amb2 <-> Amb2' ,      'v', NA,
  'Amb3 <-> Amb3' ,      'w', NA,
  'Ach1 <-> Ach1' ,      'x', NA,
  'Ach2 <-> Ach2' ,      'y', NA,
  'Achieve <-> Achieve', NA, .873,
  'Ambit <-> Ambit',      NA, 1),
  ncol=3, byrow=TRUE)

sem2.53= sem(model2.53,R.prob2.5,100, obs.var2.5)
summary(sem2.53,digits=3)

#Loehlin problem from table 2-12
#Note that version a is a classic example of congeneric measurement.
#Alternatively, this could be thought of as underidentified higher order model

obs.var2.12 = c('a',  'b',  'c',  'd')
R.prob2.12 = matrix(c(
  1.00 ,  .30,    .20,   .10,                                               
  .30,  1.00,   .20,   .20,                                               
  .20,   .20,  1.00,   .30,                                            
  .10,   .20,   .30,  1.00), 
  ncol=4,byrow=TRUE)

model2.12a=matrix(c(
  'g ->  a',      'a1', NA,
  'g -> b' ,      'b1', NA,
  'g -> c' ,      'c1', NA,
  'g -> d',     'd1', NA,
  'a <-> a',      'e1', NA,
  'b <-> b',      'e2', NA,
  'c <-> c',      'e3', NA,
  'd <-> d',      'e4', NA,
  'g <-> g',       NA, 1),
  ncol=3, byrow=TRUE)
sem2.12a= sem(model2.12a,R.prob2.12,120, obs.var2.12)
summary(sem2.12a,digits=3)

#a 1 degree of freedom model
model2.12b=matrix(c(
  'g ->  a',      'a1', NA,
  'g -> b' ,      'b1', NA,
  'f -> c' ,      'c1', NA,
  'f -> d',     'd1', NA,
  'a <-> a',      'e1', NA,
  'b <-> b',      'e2', NA,
  'c <-> c',      'e3', NA,
  'd <-> d',      'e4', NA,
  'g <-> g',       NA, 1,
  'f <-> f',       NA, 1,
  'g <-> f',        'fg', NA),
  ncol=3, byrow=TRUE)
sem2.12b= sem(model2.12b,R.prob2.12,120, obs.var2.12)
summary(sem2.12b,digits=3)

#the following higher level model has 0 degrees of freedom
model2.12c=matrix(c(
  'g ->  a',      'a1', NA,
  'g -> b' ,      'b1', NA,
  'f -> c' ,      'c1', NA,
  'f -> d',       'd1', NA,
  'a <-> a',      'e1', NA,
  'b <-> b',      'e2', NA,
  'c <-> c',      'e3', NA,
  'd <-> d',      'e4', NA,
  'g <-> g',       NA, 1,
  'f <-> f',       NA, 1,
  'h -> g',        'hg', NA,
  'h -> f',        NA,1,
  'h <-> h',        NA,1),
  ncol=3, byrow=TRUE)
sem2.12c= sem(model2.12c,R.prob2.12,120, obs.var2.12)
summary(sem2.12c,digits=3)



#Loehlin problem 2.9

obs.var2.09 = c('w',  'x',  'y',  'z')
R.prob2.09 = matrix(c(
  1.00 ,  .40,    .50,   .30,                                               
  .40,  1.00,   .55,   .35,                                               
  .50,   .55,  1.00,   .40,                                            
  .30,   .35,   .40,  1.00), 
  ncol=4,byrow=TRUE)

model2.09=matrix(c(
  'g ->  w',      'a1', NA,
  'g -> x' ,      'b1', NA,
  'g -> y' ,      'c1', NA,
  'g -> z',     'd1', NA,
  'w <-> w',      'e1', NA,
  'x <-> x',      'e2', NA,
  'y <-> y',      'e3', NA,
  'z <-> z',      'e4', NA,
  'g <-> g',       NA, 1),
  ncol=3, byrow=TRUE)
sem2.09= sem(model2.09,R.prob2.09,500, obs.var2.09)
summary(sem2.09,digits=3)


obs.var2.09b = c('w',  'x',  'y',  'z')

R.prob2.09b = matrix(c(
  1.00 ,  .40,    .50,   .30,                                               
  .40,  1.00,   .55,   .35,                                               
  .50,   .55,  1.00,   .40,                                            
  .30,   .35,   .40,  1.00), 
  ncol=4,byrow=TRUE)

model2.09b=matrix(c(
  'g ->  w',      NA,1,
  'g -> x' ,      'b1', NA,
  'g -> y' ,      'c1', NA,
  'g -> z',     'd1', NA,
  'w <-> w',      'e1', NA,
  'x <-> x',      'e2', NA,
  'y <-> y',      'e3', NA,
  'z <-> z',      'e4', NA,
  'g <-> g',       'e',NA),
  ncol=3, byrow=TRUE)

sem2.09b= sem(model2.09b,R.prob2.09b,500, obs.var2.09b)
summary(sem2.09b,digits=3)


# A FIRST LOOK AT LAVAAN
## By Grace Charles, presented at Davis R Users' Group on May 15, 2015
## adapted from Jim Grace's SEM workshop and Lavaan tutorials
# https://gist.github.com/noamross/9d5ae9680fe8357ccd94
# Set your working directory
setwd("~/Desktop/DAVIS/sem workshop")


###Load Libraries
library(semPlot)
library(lavaan)
library(qgraph)

#Built in dataset

data(PoliticalDemocracy)
PD<-PoliticalDemocracy
head(PD)

## here is an example model from one of Lavaan's build-in datasets
## the measurement model equations are "latent" and represented by =~
## regressions are indicated by ~
## residual correlations (in this case because they represent different years of the same measurement) are represented by ~~

model <- '
# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 
y2~~ y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'

#fit your SEM
fit <- sem(model, data = PD)

#summarize results
summary(fit, standardized = TRUE, rsq = T)

##plot results using semPaths function in qgraph
semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")

##check to see if you missed anything. High mi values suggest that there is a path that you missed.

modindices(fit)

## looks good

##can also look at variance tables  
vartable(fit)


## sometimes you get warnings about the scale of your variables
#Warning message:
# In getDataFull(data = data, group = group, group.label = group.label, :
#                 lavaan WARNING: some observed variances are (at least) a factor 100 times larger than others; please rescale

# in this case, all you have to do to make this error go away is rescale variables

#model comparison

#you can compare alternative pathway models using AIC, BIC, etc:

#create second alternative model
names(PD)

model2 <- '
# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ dem60
#took out ind60 from regression
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
fit2 <- sem(model2, data = PD)
summary(fit2)

AIC(fit, fit2)

## what about nonlinear data?

# Set your working directory
("~/Desktop/sem workshop")
#commands in bold

# Load data and name file ?k.dat?
k.dat<-read.csv("/Users/liding/E/Bdata/rtemp/lm_data/Keeley_rawdata_select4.csv")

# Examine contents of keeley data file
names(k.dat)
head(k.dat)


# Write lavaan code for this single equation model
mod <- '
rich ~ cover
cover ~ firesev
'

k.dat$cov2<-k.dat$cover^2

mod2<- '
rich ~ cover + cov2
cover ~ firesev
cover ~~ cov2
cov2 ~~ firesev
'



# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod, data=k.dat)
mod2.fit<- sem(mod2, data=k.dat,fixed.x=FALSE)

#need to rescale data.
vartable(mod1.fit)
k.dat$rich<-k.dat$rich/100

# Output a summary of the computed results - summary of mod2 suggests that both cover and cover squared can impact 
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr
summary(mod2.fit, rsq=T)


semPaths(mod1.fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")
semPaths(mod2.fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")






