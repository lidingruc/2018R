# 利用RScripts演示数据分析
# 数据科学与社会研究:技术基础 2018年
# 李丁(liding@ruc.edu.cn)
# 中国人民大学社会与人口学院

#####################################
# 第一个例子
# 加载后面的分析中需要用到的包
library(dplyr)
library(readr)
library(ggplot2)
library(gganimate)
help(filter)
#读入数据
setwd("/Users/liding/E/Bdata/liding17/2018R/")
#setwd("Users\\liding\\E\\Bdata\\liding17\\2018R\\")
gapminder <- read.csv("/Users/liding/E/Bdata/liding17/2018R/data/gapminder.csv")

#初步了解数据
names(gapminder)
head(gapminder)
View(gapminder)
dim(gapminder)
table(gapminder$year)
table(gapminder$year[1:300])
table(gapminder[1:300,3])
table(gapminder[gapminder$year==2007,3])


#选择2007年数据进行分析
gap07 <- gapminder %>%
  filter(year == 2007)

# 散点图呈现GDP和预期寿命的关系
qplot(x = gdpPercap, y = lifeExp, data = gap07)

# 分大陆的关系
qplot(x = gdpPercap, y = lifeExp, color = continent, data = gap07)

# 将人口规模信息也放进去
qplot(x = gdpPercap, y = lifeExp, color = continent, size = pop,data = gap07)

# 制作动态图形的方法
getwd()
gapminder_plot <- ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop,
      frame = year) +
  geom_point(alpha = 0.4) +
  scale_x_log10()
# gapminder_plot
gganimate(gapminder_plot, convert='gm convert', filename = "gapminder-gganimate.gif")


###################################################
# 第二个例子
# 利用R读入CGSS数据进行分析
library(haven)
setwd("/Users/liding/E/Bdata/liding17/2018R/l03workflow")
cgss2003<-read_dta('../data/cgss2003.dta')

#1、替换特殊缺失值
cgss2003[cgss2003==-1] <- NA;cgss2003[cgss2003==-2] <- NA;cgss2003[cgss2003==-3] <- NA
#2、丢弃没有用到的取值标签（包括上面特殊缺失值标签）
cgss2003 <- sjlabelled::drop_labels(cgss2003) 
#3、label转为因子
cgss2003 <- sjmisc::to_label(cgss2003) 
# 将剩下的labelled变量转化为数值变量（原来带特殊值标签的连续变量在此）。
w <- which(sapply(cgss2003, class) == 'labelled')
  
cgss2003[w] <- lapply(cgss2003[w], 
                   function(x) as.numeric(as.character(x))
                      
)

#定义一个函数，方便查看数据集中有哪些变量
#这样就可以像在stata中一样挑选变量进行探索和描述
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
des(cgss2003)

library(memisc)
options(digits=3)
sex.tab <- genTable(percent(sex)~sitetype,data=cgss2003)
ftable(sex.tab,row.vars=2)
ftable(sex.tab,row.vars=1)

#出生年份，不呈现缺失值
table(cgss2003$birth) # table不能缩写

# 呈现缺失值，带总数
birthy<-table(cgss2003$birth,useNA = "ifany")
addmargins(birthy)

# 注意 NaN 表示无意义的数，例如负数开平方。与NA不同，可替换为NA



###################################################
# 第三个例子
data(package="car")
library(car)
data(Duncan)
# 文件可下载：
# http://socserv.mcmaster.ca/jfox/Courses/R/ICPSR/Duncan.txt
#  ./同级目录 ../上级目录
# setwd()  # 设定路径
# Duncan<- read.table("Duncan.txt",header= TRUE) #等价
# Duncan <- read.table(file.choose(), header=TRUE)
#stata的示例数据
#library(haven)
#dose<- read_dta("http://www.stata-press.com/data/r14/dose.dta")

?Duncan # 查看包中有关数据的背景信息
Duncan
head(Duncan)
names(Duncan)
summary(Duncan)  # generic summary function
summary(Duncan$prestige)
prestige # error! 属于个数据集中 Duncan$prestige

##########################
# attaching a data frame (best avoided)
attach(Duncan)
prestige
# the search path
# R 查找的顺序，先在全局环境中查找，然后在其他加载的环境中查找。
# 加载的数据在全局环境之后
search()
# distributions and bivariate relationships
# 双变量散点图
# windows()  # for demo, not necessary in RStudio; on Mac OS X, quartz() 在Rstudio和mac中不要用。
plot(Duncan$education ~ Duncan$prestige)

hist(prestige)

pairs(cbind(prestige, income, education))

# 两两散点图、拟合线、直方图。
pairs(cbind(prestige, income, education), 
      panel=function(x, y){
        points(x, y)
        abline(lm(y ~ x), lty="dashed")
        lines(lowess(x, y))
      },
      diag.panel=function(x){
        par(new=TRUE)
        hist(x, main="", axes=FALSE)
      }
)

# 定义为一个函数，方便以后使用
scatmat <- function(...) { # user-defined function
  pairs(cbind(...),
        panel=function(x, y){
          points(x, y)
          abline(lm(y ~ x), lty=2)
          lines(lowess(x, y))
        },
        diag.panel=function(x){
          par(new=TRUE)
          hist(x, main="", axes=FALSE)
        }
  )
}

scatmat(prestige, income, education)

# 可以标出图中点的标签，必须退出才能进行后面的
plot(education, income)
identify(education, income, row.names(Duncan)) # must exit from identify mode!

row.names(Duncan)[c(6, 16, 27)]

# fitting a regression
(duncan.model <- lm(prestige ~ income + education))

summary(duncan.model)  # again, summary generic
# 科学计数设定 options(scipen=10)
detach("Duncan")
########################