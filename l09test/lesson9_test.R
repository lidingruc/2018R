##############
##  本次课的主要内容：
#一、简单描述性统计、列联表、相关。
#二、介绍T检验和方差分析
#三、介绍回归分析及其原理
#四、Rstudio提供的统计原理互动资料https://www.rstudio.com/products/shiny/shiny-user-showcase/
#五、QSS中关于probability和Uncertainty的内容：https://jrnold.github.io/qss-tidy/probability.html
# https://jrnold.github.io/qss-tidy/uncertainty.html
# -------------------------------------------
# --加载必要的包
# -------------------------------------------

if (!require(tidyverse)) install.packages('tidyverse')	
if (!require(data.table))install.packages('data.table')
if (!require(dplyr))install.packages('dplyr')
if (!require(sjPlot))install.packages('sjPlot')
if (!require(sjmisc))install.packages('sjmisc')
if (!require(haven)) install.packages('haven')


# -------------------------------------------
# --读入数据并进行了预处理
# -------------------------------------------
library(haven)
library(tidyverse)
setwd("/Users/liding/E/Bdata/liding17/2018R/data/")
cgss2013 <- read_spss("cgss2013.sav") 
# cgss2013 <- read_stata("/Users/liding/DATA/CGSS/2003-2013/2013/cgss2013_14.dta")

#定义一个函数方便查看数据集中的变量和变量标签
des <- function (dfile) {
  lbl = sapply(dfile, attr, 'label')
  if (is.list(lbl)) {
    lbl[sapply(lbl, is.null)] = ''
    lbl[sapply(lbl, length) > 1] = ''
    lbl = unlist(lbl)
  }
  Encoding(lbl) = 'UTF-8'
  dfile_var = data.frame(var =names(dfile), lbl = lbl) 
  View(dfile_var)
}  
# 通过View数据框的方式查看有哪些变量
des(cgss2013)


#替换用户定义缺失值为系统缺失值
cgss2013[cgss2013==-1] <- NA
cgss2013[cgss2013==-2] <- NA
cgss2013[cgss2013==-3] <- NA
cgss2013[cgss2013==9999997] <- NA
cgss2013[cgss2013==9999998] <- NA
cgss2013[cgss2013==9999999] <- NA

# 删除没有用到的取值标签
cgss2013 <- sjlabelled::drop_labels(cgss2013) 

#将还有取值标签的变量转为因子，无取值标签的变量仍然为labelled类型
cgss2013 <- sjmisc::to_label(cgss2013) 

#将剩余的labelled变量，都转化为普通数值变量
w <- which(sapply(cgss2013, class) == 'labelled')
cgss2013[w]<- lapply(cgss2013[w], function(x) as.numeric(as.character(x)))

#原来的函数是转为数值class(cgss2013[w]) <- c("numeric")

dim(cgss2013)

sjPlot::set_theme(theme.font ='STXihei' )
cgss2013 %>%
  select(a10) %>% sjPlot::sjplot(fun="frq")


# -------------------------------------------
# --一、描述统计 Descriptive Statistics
# -------------------------------------------
summary(cgss2013) 
summary(cgss2013$a8a)

# A. Continuous variable : income
mean(cgss2013$a8a, na.rm = TRUE)
median(cgss2013$a8a, na.rm = TRUE)
sd(cgss2013$a8a, na.rm = TRUE)
quantile(cgss2013$a8a, na.rm = TRUE)


# B.  Categorical variable : gender  edu
table(cgss2013$a2)
prop.table(table(cgss2013$a2))
prop.table(table(cgss2013$a7a))

as.data.frame(prop.table(table(cgss2013$a7a)))

# 交叉表
table(cgss2013$a7a,cgss2013$a2)  
ftable(cgss2013$a7a,cgss2013$a2)
with(cgss2013,table(a7a,a2))
xtabs(~a7a+a2,cgss2013)


# row and column percentage
prop.table(table(cgss2013$a7a,cgss2013$a2), 1)
prop.table(table(cgss2013$a7a,cgss2013$a2), 2)

# 边缘分布
margin.table(table(cgss2013$a7a,cgss2013$a2),1)
addmargins(table(cgss2013$a7a,cgss2013$a2))

# 下面这种算法是错误的
prop.table(addmargins(table(cgss2013$a7a,cgss2013$a2), 1))
addmargins(prop.table(table(cgss2013$a7a,cgss2013$a2), 2))

# svytable()  带权数的列联表

#C、使用SJ系列包输出spss样式的输出结果
library(sjPlot)
library(sjmisc)

#简单统计表 默认带上基本统计
frq(cgss2013$a7a)
frq(cgss2013$a7a,out ="viewer")
sjt.frq(cgss2013$a7a) # 旧命令

#带自定义统计量
sjt.frq(cgss2013$a7a,encoding = 'utf-8',
        show.skew = TRUE, 
        show.kurtosis = TRUE);

###########
# 列联表
sjt.xtab(cgss2013$a7a,cgss2013$a2, 
         show.obs = TRUE, show.cell.prc = FALSE, show.col.prc = TRUE,title="分性别的教育分布情况")

flat_table(cgss2013,a7a,a2,margin="row")

# 图
sjp.xtab(cgss2013$a2,cgss2013$a10, type ="bar", margin ="row",
         bar.pos = "stack")

#查看数据集中因子变量的标签与频数信息
view_df(cgss2013[,550:577], show.frq = TRUE, show.prc = TRUE)


# D. Correlation 
cor(cgss2013$a8a,cgss2013$a8b,use="complete.obs")

par(family="STKaiti")
plot(cgss2013$a8a,cgss2013$a8b)


# -------------------------------------------
# -- 二. 假设检验 Hypothesis Testing --
# ------------------------------------------- 
############
# A、Chi-square Test, small difference
# 教育水平和性别
chisq.test(cgss2013$a7a,cgss2013$a2)
summary(table(cgss2013$a2, cgss2013$a7a))

# expected cell 
ch01 <- chisq.test(cgss2013$a7a,cgss2013$a2)
ch01$residuals
chisq.test(cgss2013$a7a,cgss2013$a2)$expected

# 标准化偏差，评估单元格影响
chisq.test(cgss2013$a7a,cgss2013$a2)$resid

##########
mytable<-xtabs(~cgss2013$a7a+cgss2013$a2)
chisq.test(mytable)

##########
#似然比检验
#### Likelihood-Ratio Test Statistic for IxJ tables
LRstats=function(data){
  G2=2*sum(data*log(data/chisq.test(data)$expected))
  G2pvalue=1-pchisq(G2,df=(dim(data)[1]-1)*(dim(data)[2]-1))
  ans=c(G2,G2pvalue)
  ans
}
LRstats(mytable)

# 二手列联表分析
observed <- matrix(c(32, 24, 265, 199, 391, 287),nrow = 3, byrow = T)
observed

chisq.test(observed, correct = F)
cbind(observed, chisq.test(observed)$expected)
cbind(observed, chisq.test(observed)$resid)

############
# B、t-test

# One sample t test
t.test(cgss2013$a8a, mu=25000)
t.test(cgss2013$a8a, mu=25000,alternative ='less')

# Independent 2 group t test where y is numeric and x is a binary factor
t.test(a8a ~ a2, data = cgss2013)
var.test(a8a ~ a2, data = cgss2013)
# Paired t test

t.test(cgss2013$a8a, cgss2013$a8b, paired = TRUE) 

tResults <- t.test(cgss2013$a8a, cgss2013$a8b, paired = TRUE) 
summary(tResults)
tResults$statistic
tResults['statistic']

# 原理示例
# 假定方差相等，假定正太分布，检验均值相等假设
cholest <- data.frame(chol = c(245, 170, 180,190, 200, 210, 220, 230, 240, 250, 260, 185,205, 160, 170, 180, 190, 200, 210, 165), gender = rep(c("female","male"), c(12, 8)))
str(cholest)
cholest 
boxplot(chol ~ gender, cholest, ylab = "Cholesterol Score")

# 正态分布？
par(mfrow = c(1, 2))
qqnorm(cholest$chol[cholest$gender == "male"],
       main = "QQNorm for the Males")

qqnorm(cholest$chol[cholest$gender == "female"],
       main = "QQNorm for the Females")

# 方差相等
var.test(chol ~ gender, cholest)

# 均值相等 
t.test(chol ~ gender, cholest)

############
#C、anova
par(family="STKaiti")
par(mfrow = c(1, 1))
boxplot(a8a~a7a,data=cgss2013)

edu_inc <- cgss2013 %>% group_by(a7a) %>%
  summarize(meaninc = mean(a8a, na.rm = TRUE)) 

ggplot(edu_inc,aes(x=a7a,y=meaninc)) +
  geom_bar(stat = "identity")

aov_inc <- aov(a8a ~ a7a , data = cgss2013)
summary(aov_inc)
aov_inc 
plot(aov_inc)

TukeyHSD(aov_inc)

##交互效应的表示方式
fit <- aov(a8a ~ a7a + a2 + a7a:a2, data = cgss2013)
fit
fit <- aov(a8a ~ a7a*a2, data = cgss2013)
fit

# 协方差分析
aov(a8a ~ a3a + a2, data = cgss2013)
aov(a8a ~ a3a * a2, data = cgss2013)

#可以使用lm模型来做，提取其中的方差分析部分即可
#见下面的例子

# F检验的临界值，4自由度，50个案例
qf(0.95, 4, 45)

############
#D、回归分析 Linear Regression Model

?lm
?predict.lm

lm_inc <- lm(a8a ~ I(2013-a3a) + a2, data = cgss2013)
summary(lm_inc)
anova(lm_inc)
par(mfrow = c(2, 2))
plot(lm_inc)
confint(lm_inc)

## 回归分析的原理展示
## 构建一个虚拟数据
set.seed(1)
x <- seq(1,5,length.out=100)
noise <- rnorm(n =100, mean =0, sd = 1)
beta0 <- 1
beta1 <- 2
y <- beta0+ beta1*x +noise
par(mfrow = c(1, 1))
plot(y ~ x)

model <- lm(y ~x)  # 生成了一个model 对象
plot(y~x)
abline(model)

###回归模型的输出结果
summary(model)
model.matrix(model)

##model对象中包含的其他内容
names(model)

## 判定系数的含义
ybar <- mean(y)
yPred <- model$fitted.values
Rsquared <- sum((yPred-ybar)^2)/sum((y-ybar)^2)
sqrt(sum(model$residuals^2)/98)

## 预测值作图
yConf <- predict(model,interval= 'confidence')
yPred <- predict(model,interval= 'prediction')
plot(y~x,col='grey',pch=16)
yConf <- as.data.frame(yConf)
yPred <- as.data.frame(yPred)
lines(yConf$lwr~x,col='black',lty=3)
lines(yConf$upr~x,col='black',lty=3)
lines(yPred$lwr~x,col='black',lty=2)
lines(yPred$upr~x,col='black',lty=2)
lines(yPred$fit~x,col='black',lty=1)

## 纳入虚拟变量 要转为因子类型
set.seed(1)
x <- factor(rep(c(0,1),each=30))
y <- c(rnorm(30,0,1),rnorm(30,1,1))
plot(y~x)

model <- lm(y~x)
summary(model)
model.matrix(model)

#效果等同于T检验。

# 回归诊断
## 前提是否成立：模型结构假设，误差假设，异常样本
#  真模型是二次回归，线性回归是有偏差的
set.seed(1)
x<-seq(1,5,length.out=100)
noise<-rnorm(n=100,mean=0,sd=1)
beta0<-1
beta1<-2
y<-beta0+beta1*x^2+noise
model<-lm(y~x)
summary(model)
plot(y~x)
abline(model)

## 呈现异方差
plot(model$residuals~x)

## 增加二次项、
model2<-lm(y~x+I(x^2))
summary(model2)
plot(model2$residuals~x)

## 剔除一次项再回归
model3<- update(model2, y ~ . -x)
summary(model3)
plot(model3$residuals~x)

AIC(model,model2,model3)

############
# 在R中模拟异方差
set.seed(123456) 
x = rnorm(500,1,1) 
b0 = 1 # intercept chosen at your choice 
b1 = 1 # coef chosen at your choice 
h = function(x) 1+.4*x # h performs heteroscedasticity function (here 
#I used a linear one) 
eps = rnorm(500,0,h(x)) 
y = b0 + b1*x + eps 
plot(x,y) 
abline(lsfit(x,y)) 
abline(b0,b1,col=2) 


# E、模拟和获取统计检验结果（自学，结合lesson7_basic.R)
# Generate a random sample from specific distribution 
# n=100 from N(0,1) distribution
rnorm(100)
?rnorm
# n=100 from U(0,1) distribution
runif(100)
?runif


# Density for specific distribution
par(mfrow = c(1, 1))  
x <- seq(-4, 4, length = 100)
y1 <- dnorm(x)
plot(x, y1, type = "l", lwd = 2, col = "blue")
y2 <- dnorm(x, m = 0, sd = 2)
lines(x, y2, type = "l", lwd = 2, col = "red")


# Cumulative distribution function : To get p-value, pnorm() function.
pnorm(1.96)
pnorm(1.96, lower.tail = FALSE)


# Quantile function : To get quantiles or "critical values", qnorm( ) function. 
qnorm(0.95) # p = .05, one-tailed (upper)
qnorm(c(0.025, 0.975)) # p = .05, two-tailed


# -------------------------------------------
# R统计入门课程资料：http://www.stat.wmich.edu/~hzz3534/stat2600s1/
# 分类汇总的其他方法，参考肖凯
#http://www.xueqing.tv/lesson/4
#http://www.xueqing.tv/lesson/5
# -------------------------------------------
## 分类汇总
data(tips,package='reshape2')
#tips数据集练习，它是一个餐厅侍者收集的关于小费的数据，其中包含了七个变量，包括总费用、付小费的金额、付款者性别、是否吸烟、日期、日间、顾客人数。
head(tips)

#计算不同性别顾客是否会支付不同的小费比例。则可以按sex变量汇集数据。
#方法1 aggregate方法
aggregate(x=tips$tip,
	by=list(tips$sex),
	FUN=mean)

#stata做法： table sex,c(mean tip)

#方法2 data.table中的dcast方法。更多介绍见后面
library(data.table)
dcast(data=tips,sex~. ,fun=mean)  ## 默认的汇总 size

dcast(data=tips,sex~. ,value.var='tip',fun=mean) # 分性别汇总

dcast(data=tips,sex~smoker,value.var='tip',fun=mean) # 分性别和吸烟汇总

dcast(data=tips,sex~smoker,value.var='tip',fun=median) # 分性别和吸烟汇总

###	ddply方法
library(plyr)
dd<-ddply(.data=tips,   ## 数据对象
	.variables='sex',## 拆分变量
	.fun=function(x){  ## 计算函数
		mean(x$tip)
	})

dd

# 小费占比
ratio_fun<- function(x){
  sum(x$tip)/sum(x$total_bill)
}	
ddply(tips,.(sex),ratio_fun)

#stata做法：table sex smoker,c(sum tip sum total_bill)
ddply(tips,sex~smoker,ratio_fun)

## plyr包的主要函数（输入3种类型，输出3种类型，9种方法，加上3种特殊类型）
### ddply 输入数据框，输出数据框
### adply 输入数组，输出数据框 
# input array, split by margins

data<-as.matrix(iris[,-5])  ##  去除最后一个分类变量
result4 <- adply(.data=data,
				.margins =2, #1 =rows 2=col
				.fun=function(x){
					max<-max(x)
					min<-min(x)
					median<-median(x)
					sd<-round(sd(x),2)
					return(c(max,min,median,sd))
				})
result4
result4 <- adply(.data=data,
				.margins =2, #1 =rows 2=col
				.fun=function(x){
					each(max,min,median,sd)(x)
				}) 
result4			

#build three linear regression model in list

model<- function(x){
	lm(Sepal.Length~Sepal.Width,data=x)
}
models <- dlply(.data=iris,
				.variable='Species',
				.fun=model)

#得到一个list 里面是3个模型的结果
#得到三个模型的系数				
result5<- ldply(.data=models,
				.fun=coef)

result5

## 其他可用的汇总函数
#each函数
x<-rnorm(10)
# 求变量的多个统计量
each(max,min,median,sd)(x)
# 求数值变量的均值
colwise(mean,is.numeric)(iris)

# plyr中的其他函数
#join~merge
#mutate~transform
#summarise~transform
#arrange~order
#rename~name
#mapvalues~relevel
#count~length

#
library(nycflights13)
summary_temp <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE), std_dev = sd(temp, na.rm = TRUE))
knitr::kable(summary_temp)


# 分组之后比较均值
library(car)
tips$total_bill.rec <- recode(tips$total_bill,
                         "0:10='0to10';
                         10:29='11to29';
                         29:51='30to51'")
tips$total_bill.rec <- as.factor(tips$total_bill.rec)
# 分组之后比较均值
dcast(data=tips,total_bill.rec~. ,value.var='tip',fun=mean)

###########
#dcast分类汇总，更多介绍
# http://www.xueqing.tv/upload/april-training/day3/index.html#1
# 来自
data(tips,package='reshape2');

#tips数据集练习，它是一个餐厅侍者收集的关于小费的数据，其中包含了七个变量，包括总费用、付小费的金额、付款者性别、是否吸烟、日期、日间、顾客人数。
head(tips)
#计算不同性别顾客是否会支付不同的小费比例。则可以按sex变量汇集数据。
dcast(tips,sex~.,value.var='tip',fun=mean)
#按sex和size变量划分数据，分别计算小费金额，可以观察到用餐人数越多时，小费相应给的越多，而且男性顾客一般会比女性顾客大方一点。
dcast(tips,sex~size,value.var='tip',fun=mean)

#dcast函数的使用前提
#数据中已经存在分类变量，例如sex或者smoker
#根据分类变量划分数据
#再计算某个数值变量的指标

#同时计算出不同性别顾客的小费和总费用。
#一种是笨一点的方法，将前面用过的方法用两次，然后合并这两个结果。
dcast(tips,sex~.,value.var='tip',fun=mean)
dcast(tips,sex~.,value.var='total_bill',fun=mean)

#另一种方法是将小费和总费转成长数据，variable 标记了
tips_melt <- melt(data = tips,
              id.vars=c('sex','smoker','time','size','day'))
dcast(data = tips_melt, sex ~ variable,  
          value.var='value',fun= mean)
# 要同时考虑不同性别和吸烟习惯的顾客给小费的相对例。
tips_mean <- dcast(data = tips_melt, sex+ smoker~ variable, fun= mean)
tips_mean$rate <- with(tips_mean,tip/total_bill)
tips_mean		  

#在dcast函数中的公式同时考虑到了三个分类变量，在第二步计算了小费相对于总餐费的比率，可以清楚的看到，吸烟的女性顾客相对是最大方的，而吸烟的男性则是最小气的。

