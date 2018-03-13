# Project:    Code for lesson10
# Date:       2017-11-22
# Author:     liding

#-------------------------------------------------------------
# 给一段散点提供一个最好的拟合直线：回归原理的介绍
# 回归模型的可视化
# 分类变量
# 交互效应
# 变量变换
# 缺失值
# 综合例子1
# CGSS例子
# 回归诊断与应对
# 交叉检验

# -------------------------------------------
# --加载必要的包
# -------------------------------------------
if (!require(dplyr))install.packages('dplyr')
if (!require(sjPlot))install.packages('sjPlot')
if (!require(sjmisc))install.packages('sjmisc')
if (!require(haven)) install.packages('haven')
library(haven)
library(tidyverse)
library(stargazer)
library(modelr)

options(na.action = na.warn)

# The goal of a model is to provide a simple low-dimensional summary of a dataset.

# -------------------------------------------
# --示例回归拟合的原理
# -------------------------------------------
# 散点图
ggplot(sim1, aes(x, y)) + 
  geom_point()

# 用什么去拟合这些点呢？
# 我们可以随机生成一些直线
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() +
  geom_box()

# 拟合情况示意图
dist1 <- sim1 %>% 
  mutate(
    dodge = rep(c(-1, 0, 1) / 20, 10),
    x1 = x + dodge,
    pred = 7 + x1 * 1.5
  )

ggplot(dist1, aes(x1, y)) + 
  geom_abline(intercept = 7, slope = 1.5, colour = "grey40") +
  geom_point(colour = "grey40") +
  geom_linerange(aes(ymin = y, ymax = pred), colour = "#3366FF") 

# 定义一个函数对数据 计算预测值
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

# 计算预测值和观察值之间的差距
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

# 计算所有模型的离差情况
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

# 选出10个拟合最好的模型
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

# 这些系数在全部系数散点中的位置
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

# 选择红点附近小范围内的参数，进一步寻求拟合更好的结果
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

# 画出拟合线
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

# 可以继续这样逼近，
# 一个更好的办法是数据分析中的 Newton-Raphson method
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

# 不过，针对线性回归模型，可以直接计算
# 一步算出对应的参数，更快，更稳健
# lm
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

summary(sim1_mod)

# -------------------------------------------
# --回归结果作图
# -------------------------------------------
# 拟合直线

grid <- sim1 %>% 
  modelr::data_grid(x) 
grid

grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)


# 残差
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

# 残差分布图
ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

# 残差-x图
ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 


# -------------------------------------------
# --分类变量
# -------------------------------------------
# 查看原始数据
ggplot(sim2) + 
  geom_point(aes(x, y))

# 建模+作图
mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

#不存在的level不能计算估计值
tibble(x = "e") %>% 
  add_predictions(mod2)


# -------------------------------------------
# --交互效应 Interactions (continuous and categorical)
# -------------------------------------------
ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))

# 构建两个模型（可加模型、交互模型）
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
#mod2 <- lm(y ~ x1 + x2 + x1:x2, data = sim3)

#产生预测数据，作图 
grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

#通过残差情况，比较哪个模型拟合更好
sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)

# -------------------------------------------
# --交互效应 Interactions (two continuous)
# -------------------------------------------

# 建模+产生作图数据
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

# 交互效应图
ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

#分x1，x2的看交互效应
ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

# -------------------------------------------
# --非线性-变量转换
# -------------------------------------------

# 示例，转换差别
df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)
model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x^2) + x)

# 我们可以用泰勒展开式无线逼近任意曲线
model_matrix(df, y ~ poly(x, 2))

# 但有所不足，更好的办法使用分段样条法
#outside the range of the data, polynomials rapidly shoot off to positive or negative infinity. One safer alternative is to use the natural spline, `splines::ns()`.

library(splines)
model_matrix(df, y ~ ns(x, 2))

# 模拟一个非线性回归
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

# 拟合5个模型，可以看到外推都不好
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)


# -------------------------------------------
# --缺失值处理
# -------------------------------------------
df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)


mod <- lm(y ~ x, data = df)

# 排除缺失值，
mod <- lm(y ~ x, data = df, na.action = na.exclude)
nobs(mod)


# -------------------------------------------
# --其他模型
# -------------------------------------------
# 一般化线性模型：因变量为分类变量
stats::glm()

# 稳健线性模型：对残差进行加权处理，避免极端案例影响
MASS:rlm()



# -------------------------------------------
# --综合示例1：介绍回归的各种参数
# -------------------------------------------
## 构建一个虚拟数据
## 假定知道真实关系
set.seed(1)
x <- seq(1,5,length.out=100)
noise <- rnorm(n =100, mean =0, sd = 1)
beta0 <- 1
beta1 <- 2
y <- beta0+ beta1*x +noise
plot(y ~ x)

# 生成了一个model 对象
model <- lm(y ~x) 
plot(y~x)
abline(model)

## 回归模型的输出结果
summary(model)
model.matrix(model)

##model对象中包含的其他内容
names(model)

## 判定系数的含义
ybar <- mean(y)
yPred <- model$fitted.values
Rsquared <- sum((yPred-ybar)^2)/sum((y-ybar)^2)
sqrt(sum(model$residuals^2)/98) # 自由度n-2

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
#实际上，效果等同于T检验。

# -------------------------------------------
# --回归诊断
# -------------------------------------------
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

## 增加二次项
model2<-lm(y~x+I(x^2))
summary(model2)
plot(model2$residuals~x)

## 剔除一次项再回归
model3<- update(model2, y ~ . -x)
summary(model3)
plot(model3$residuals~x)

AIC(model,model2,model3)
# 模型3比较简洁

# -------------------------------------------
# --异方差处理
# -------------------------------------------
# 生成异方差数据
set.seed(12356) 
x = rnorm(500,1,1) 
b0 = 1 # intercept 
b1 = 1 # coef 
h = function(x) 1+.4*x # h performs heteroscedasticity function (here a linear one) 
eps = rnorm(500,0,h(x)) 
y = b0 + b1*x + eps 
plot(x,y) 
abline(lsfit(x,y)) 
abline(b0,b1,col=2) 

#残差-x分布
model<-lm(y~x)
summary(model)
plot(model$residuals~x)

# 估计无偏,但因为标准误问题，检验低效。
# 解决方案：
#（1）使用稳健标准误
#（2）使用WLS(weighted least squares)

# 修正的标准误，系数再检验
require("lmtest")
require("sandwich")
coeftest(model, vcov.=vcovHC(model, type="HC1"))

# http://www.drewdimmery.com/robust-ses-in-r/
# 或者分两步做
model$newse<-vcovHC(model)
coeftest(model,model$newse)

# 获得与stata一样的结果：to reproduce the Stata default behavior of using the robust option in a call to regress you need to request vcovHC to use the HC1 robust variance-covariance matrix
#https://stats.stackexchange.com/questions/117052/replicating-statas-robust-option-in-r

# WLS
wlsmodel = lm(y ~ x, weights=1/(1+0.4*x))

par(mfcol=c(1,1))
plot(x,y) 
abline(lsfit(x,y)) 
abline(b0,b1,col=2) 
abline(wlsmodel$coefficients,lty=3)            

# -------------------------------------------
# --CGSS2003的例子
# -------------------------------------------
# 读入数据并进行预处理

setwd("/Users/liding/E/Bdata/liding17/2017R/lesson9/")
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

#将剩余的labelled变量都转化为普通数值变量
w <- which(sapply(cgss2013, class) == 'labelled')

cgss2013[w]<- lapply(cgss2013[w], 
                     function(x) as.numeric(as.character(x)))

cgss2013 <- cgss2013 %>%
mutate(edu = fct_collapse(a7a,
          "小学"="私塾",
          "初中"=c("其他","初中","技校"),
         "高中"=c("职业高中","普通高中","中专"),
          "大学"=c("大学专科(成人高等教育)","大学专科(正规高等教育)","大学本科(成人高等教育)","大学本科(正规高等教育)","研究生以上")))

# ggplot 作图
ggplot(data=na.omit(cgss2013[,c("edu","a8a")]),aes(x=edu,y=a8a))+
  geom_boxplot() +
  theme(text=element_text(family="STKaiti"))+
  labs(x="教育水平",y="年收入")

#回归分析
lm_inc <- lm(a8a ~ I(2013-a3a) + a2, data = cgss2013)



# 查看结果
summary(lm_inc)
anova(lm_inc)
# 检验图
par(mfrow = c(2, 2))
plot(lm_inc)
#置信区间
confint(lm_inc)

# 使用sj包输出回归结果
sjp.lm(lm_inc)
sjt.lm(lm_inc)

# 具体设定输出参数，多个模型
lm_inc2 <- lm(a8a ~ I(2013-a3a) + a2 + edu, data = cgss2013)

sjt.lm(lm_inc,lm_inc2,show.ci=FALSE, show.std = TRUE, show.aic = FALSE, show.se = FALSE)

# 使用stargazer生成latex代码供latex使用
stargazer(lm_inc, title="回归结果", align=TRUE)

#stargazer包基础上自定义的函数，中文有点问题
printo<- function(x, ...){
  tab <- paste(capture.output(stargazer(x, ...)), collapse = '\n')
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  rstudioapi::viewer(tf)
}

printo(lm_inc, title="Regression", align=TRUE, no.space=TRUE, type = 'html', header = FALSE)

# 模型参数整理成dataframe
broom::glance(lm_inc)
broom::tidy(lm_inc)


# -------------------------------------------
# --政治坐标系的例子
# -------------------------------------------
#https://sejdemyr.github.io/r-tutorials/statistics/tutorial6.html

# Load packages
libs <- c("plyr", "tidyverse", "ggplot2", "arm")
sapply(libs, require, character.only = TRUE)

libs <- c("plyr", "tidyverse", "arm")
inpkg <- function(x){
  if (!require(x)) install.packages(x)	
}

sapply(libs, inpkg)


# Load data 
library(foreign)

anes <- read.dta("/Users/liding/E/Bdata/Course/2R/stanford/Tutorial2data/anescum.dta", warn.missing.labels = F)   
#system("rm -r anescum* __M*")                              
# Keep only some variables and rename
anes <- anes %>%
  dplyr::select(VCF0004, VCF0101, VCF0104, VCF0106, VCF0140, VCF0113, VCF0114, VCF0301, VCF0310, VCF0604, VCF0838, VCF0218) %>%
        rename(c(VCF0004 = "year",
                 VCF0101 = "age",
                 VCF0104 = "gender",
                 VCF0106 = "race",
                 VCF0140 = "edu",
                 VCF0113 = "south",
                 VCF0114 = "income",
                 VCF0301 = "partyid",
                 VCF0310 = "interest",
                 VCF0604 = "govtrust",
                 VCF0838 = "abortion",
                 VCF0218 = "demtherm"))


dim(anes)


#--------------------------------------------------------------
# Model 1: demtherm v gender in 2008 
levels(anes$gender) <- c(NA, "Male", "Female")

lm1 <- lm(demtherm ~ gender, data = anes)
display(lm1)


# Two ways to graphically display results:

# (1) 
ci1 <- coef(lm1)[2] + c(-1, 1) * se.coef(lm1)[2] * 1.96
ci1 <- confint(lm1, level = 0.95)[2, ]

est1 <- data.frame(est = coef(lm1)[2],
                   lb = ci1[1],
                   ub = ci1[2], 
                   model = "Model 1")

ggplot(est1, aes(x = model, y = est)) +
    geom_point() +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.1) +
    geom_hline(yintercept = 0, lty = 2, color = "red") +
    xlab("") +
    ylab("Female Democrat Thermometer Estimate (Relative to Males)")


# (2) 
pred1 <- predict(lm1,
                 newdata = data.frame(gender = c("Male", "Female")),
                 se.fit = T,
                 interval = "confidence")

pred1 <- data.frame(pred1$fit, gender = c("Male", "Female"))

ggplot(pred1, aes(x = gender, y = fit, color = gender)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
    xlab("") +
    ylab("Predicted Democrat Thermometer")



#--------------------------------------------------------------
# Model 2: add control for income and age
levels(anes$income)[1] <- NA
levels(anes$income) <- with(anes, substr(levels(income), 4, nchar(levels(income))))
anes$age[anes$age == 0] <- NA


lm2 <- lm(demtherm ~ gender + age + income, data = anes)

display(lm2)


# (1) 
ci2 <- confint(lm2, level = 0.95)[2, ] 

est2 <- data.frame(est = coef(lm2)[2],
                   lb = ci2[1],
                   ub = ci2[2], 
                   model = "Model 2")         

est <- rbind(est1, est2)

ggplot(est, aes(x = model, y = est)) +
    geom_point() +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.1) +
    geom_hline(yintercept = 0, lty = 2, color = "red") +
    xlab("") +
    ylab("Female Democrat Thermometer Estimate (Relative to Males)")


# (2)
newdta <- data.frame(gender = c("Male", "Female"),
                     age = mean(anes$age, na.rm = T), 
                     income = "34 to 67 percentile")

pred2 <- predict(lm2,
                 newdata = newdta,
                 se.fit = T,
                 interval = "confidence")

pred2 <- data.frame(pred2$fit, gender = c("Male", "Female"))

ggplot(pred2, aes(x = gender, y = fit, color = gender)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
    xlab("\nNote: Estimates condition on income and age") +
    ylab("Predicted Democrat Thermometer")


#--------------------------------------------------------------
# Model 3: Relationship over time
anes_sub <- subset(anes, year >= 1978 & year != 2002)

lm3 <- dlply(anes_sub, .(year), function(x) {
    lm(demtherm ~ gender + age + income, data = x)
})

display(lm3$"1990")


# (1) 
est_time <- ldply(lm3, function(x) {
    c(coef(x)[2], confint(x)[2, ])
})

names(est_time) <- c("year", "est", "lb", "ub")


ggplot(est_time, aes(x = year, y = est)) +
    geom_point() +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.3) +
    geom_hline(yintercept = 0, lty = 2, color = "red") +
    xlab("") +
    ylab("Female Democrat Thermometer Estimate (Relative to Males)")


# (2)
pred_time <- ldply(lm3, function(x) {
    predict(x, newdata = newdta, se.fit = T, interval = "confidence")$fit
})

pred_time$gender <- rep(c("Male", "Female"), 14)


ggplot(pred_time, aes(x = year, y = fit, color = gender)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymax = upr, ymin = lwr, fill = gender),
                    alpha = 0.15, color = NA) + 
    xlab("\nNote: Estimates condition on income and age") +
    ylab("Predicted Democrat Thermometer")


#--------------------------------------------------------------
# Model 4: Interactions
lm4 <- lm(demtherm ~ gender + age + income + age:gender, data = anes)
display(lm4)

agerange <- 18:85
newdta <- data.frame(expand.grid(gender = c("Male", "Female"),
                                 age = agerange), 
                     income = "34 to 67 percentile")

pred4 <- predict(lm4,
                 newdata = newdta,
                 se.fit = T,
                 interval = "confidence")

pred4 <- data.frame(cbind(pred4$fit, newdta))

head(pred4)

ggplot(pred4, aes(x = age, y = fit, color = gender)) +
    geom_point() +
    geom_line(aes(y = lwr), lty = 3) +
    geom_line(aes(y = upr), lty = 3) + 
    ylab("Democrat Thermometer Ratings") +
    xlab("Age")


# Relax linearity assumption on age
anes_sub <- subset(anes, age %in% 20:70)
anes_sub$age_cat <- cut(anes_sub$age, breaks = seq(20, 70, by = 5))

head(anes_sub)


lm5 <- lm(demtherm ~ gender + income + age_cat + + age_cat:gender,
          data = anes_sub)

display(lm5)


agerange <- sort(as.character(unique(na.omit(anes_sub$age_cat))))
newdta <- data.frame(expand.grid(gender = c("Male", "Female"),
                                 age_cat = agerange), 
                     income = "34 to 67 percentile")

pred5 <- predict(lm5,
                 newdata = newdta,
                 se.fit = T,
                 interval = "confidence")

pred5 <- data.frame(cbind(pred5$fit, newdta))

ggplot(pred5, aes(x = age_cat, y = fit, color = gender)) +
    geom_point() +
    geom_line(aes(y = lwr, group = gender), lty = 3) +
    geom_line(aes(y = upr, group = gender), lty = 3) +
    geom_line(aes(y = fit, group = gender), lty = 1, alpha = 0.5) + 
    ylab("Democrat Thermometer Ratings") +
    xlab("Age Category") +
    theme_bw()


### 作业3 11月29日之前提交到liding@ruc.edu.cn 
### 文件名和邮件标题都统一为“R第3作业学号姓名”的样式。
### 最好使用rmd+doc格式，不行则可以使用R+doc的格式

# 使用CGSS2013数据
# （1）依据民族 a4 产生一个新变量ethic，合并为汉族和少数民族两类；依据教育a7a，生成edu，类别合并为“文盲“,"小学"，"初中"，"高中","大学"五类。
#  (2) 比较汉族和少数民族受访者在收入a8a上是否存在显著差异；不同教育水平的受访者收入a8a是否存在显著差异。综合利用ggplot作图和统计检验呈现自己的观点。
# （3）根据出生年份a3a产生一个年龄变量age，探索收入a8a与年龄age之间的关系。可以产生一个lninc变量等于log10(a8a+1)，以之作为本题及下一题的因变量。

#  (4) 将age、ethic、edu作为解释变量，建立回归模型。探索其中是否存在交互效应、非线性效应。

# （5）数据中有一些案例的收入取值很低，您觉得应该如何处理比较好？


cgss2013 <- cgss2013 %>%
  mutate(lninc=log10(a8a+1))
ggplot(cgss2013,aes(x=2013-a3a,y=lninc))+ geom_point()



####################
#回归诊断的标准输出
#1、残差——拟合值图判断线性性
#2、QQ图判断残差正态性
#3、位置-尺度图判断同方差性
#4、残差-杠杆图探索特异点（离群-残差看、高杠杆-leverage看、强影响-cook's D）

fit <- lm(weight~height,data=women)
summary(fit)
par(mfrow=c(1,1))
plot(women$height, women$weight, main = "Women Age 30-39", 
     xlab = "Height (in inches)", ylab = "Weight (in pounds)")
abline(fit)
#诊断图
par(mfrow=c(2,2))
plot(fit)

#二次项
fit <- lm(weight~height+ I(height^2),data=women)
par(mfrow=c(1,1))
plot(women$height, women$weight, main = "Women Age 30-39", 
     xlab = "Height (in inches)", ylab = "Weight (in lbs)")
lines(women$height, fitted(fit))
#诊断图
par(mfrow=c(2,2))
plot(fit)

#特异点（模型来拟合数据而不是数据拟合模型，特异点很有价值）
fit <- lm(weight~height+ I(height^2),data=women[-c(13,15),])
par(mfrow=c(2,2))
plot(fit)

####################
#改进的回归诊断与 问题应对方法
#1、正态性：car::qqPlot(fit)  ——>变量转换、换模型
#2、误差独立性：car::durbinWatsonTest(fit)——>稳健标准误
#3、线性：car::crPlots(fit) ——>变量转换、换模型
#4、同方差：car::ncvTest(fit) car::spreadLevelPlot(fit)
#   ——>变量转换、换模型
#6、多重共线性： car::vif(fit)——>增删变量、换模型
#7、特异值：car::outlierTest(fit)   car::avPlots(fit) 
#   car::influencePlot(fit)——>删案例
#5、综合检验： gvlma::gvlma(fit)

library(car) # Jhon Fox 等
help(package="car")

plot(fit,which=4)

####################
#交叉验证（Crossvalidation）
#以预测效果来评估模型好坏
#训练数据集-验证数据集
#10重交叉验证
# Listing 8.15 - Function for k-fold cross-validated R-square
shrinkage <- function(fit, k = 10) {
  require(bootstrap)
  # define functions
  theta.fit <- function(x, y) {
    lsfit(x, y)
  }
  theta.predict <- function(fit, x) {
    cbind(1, x) %*% fit$coef
  }
  
  # matrix of predictors
  x <- fit$model[, 2:ncol(fit$model)]
  # vector of predicted values
  y <- fit$model[, 1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup = k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2 - r2cv, "\n")
}

# using shrinkage()
# 数据
states <- as.data.frame(state.x77[, c("Murder", "Population", 
                                      "Illiteracy", "Income", "Frost")])
# 模型
fit <- lm(Murder ~ Population + Income + Illiteracy + 
            Frost, data = states)
# 验证
shrinkage(fit)

fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
shrinkage(fit2)


