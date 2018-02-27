#######################################################
#教学目标：
#一、R作图的基本原则
#二、基础绘图
#三、ggplot2作图，映射、几何对象、分组、坐标轴、标签、样式设定等
#四、作图基础！
#五、其他作图如lattice

# 综合了肖凯、陈堰平、纽约大学图书馆、John Fox课件的内容


# 1.1、为什么要作图
#数据下载：http://www.xueqing.tv/upload/april-training/day4/data.zip
# http://www.xueqing.tv/upload/april-training/day4/index.html#1
#http://guides.nyu.edu/r

id <- rep(1:4,4)
year <- 
#########################################################
# 一图胜千言示例1：相关散点图

setwd("/Users/liding/E/Bdata/rtemp/")
data <- read.table('data/anscombe1.txt',T)
head(data)
data <- data[,-1]
head(data)
dim(data)

# 我们可以看原始数据，可以汇总各种指标。
colMeans(data)
rowMeans(data)
sapply(1:4,function(x) cor(data[,x],data[,x+4]))
# 但是，都远远不如可视化来得更为直接。
par(mfrow=c(2,2))
sapply(1:4,function(x) plot(data[,x],data[,x+4]))


#########################################################
# 一图胜千言示例2：HLM模型
# Trellis displays (implemented in lattice package; uses grid package)

library(nlme) # for data
library(lattice) # for Trellis graphics 
head(MathAchieve)
head(MathAchSchool)

# data management

Bryk <- MathAchieve[, c("School", "SES", "MathAch")]
Sector <- MathAchSchool$Sector
names(Sector) <- row.names(MathAchSchool)
Bryk$Sector <- Sector[as.character(Bryk$School)]
head(Bryk)

# examine 20 Catholic

set.seed(12345) # for reproducibility
cat <- with(Bryk, sample(unique(School[Sector=="Catholic"]), 20))
Cat.20 <- Bryk[Bryk$School %in% cat, ]

res <- xyplot(MathAch ~ SES | School, data=Cat.20, main="Catholic Schools",
              ylab="Math Achievement",
              panel=function(x, y){ 
                panel.xyplot(x, y)
                panel.loess(x, y, span=1)
                panel.lmline(x, y, lty=2)
              }
)
class(res)
res  # "printing" plots the object

remove(list=objects())  # clean up

#########################################################
# 一图胜千言示例3：3D动态图
# rgl 3D graphics package (by Daniel Adler and Duncan Murdoch)
# uses scatter3d() from car package
library(car)
scatter3d(prestige ~ log(income) + education | type, data=Prestige, 
          ellipsoid=TRUE, parallel=FALSE,revolution=TRUE)  

# data(Duncan, package="car")
# 加上revolutions=3表示自动旋转
scatter3d(prestige~education+income, data=Duncan, fit="linear", 
          residuals=TRUE, bg="white", axis.scales=TRUE, grid=TRUE, ellipsoid=TRUE, 
          id.method='mahal', id.n = 3, revolutions=3)

## 3D图
library(rgl)
x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x,y)
plot3d(x, y, z, col=rainbow(1000))


#3D 曲面图
library(plyr)
library(lattice)
func3d <- function(x,y) {
  sin(x^2/2 - y^2/4) * cos(2*x - exp(y))
}
vec1 <- vec2 <- seq(0,2,length=30)
para <- expand.grid(x=vec1,y=vec2)
result6 <- mdply(.data=para,.fun=func3d)

wireframe(V1~x*y,data=result6,scales = list(arrows = FALSE),
          drape = TRUE, colorkey = F)

#########################################################
#1.2、作图的基本原则

##1. 需要事先明确可视化的具体目标
###探索性可视化
###解释性可视化

##2. 需要考虑数据和受众群体的特点
###哪些变量最重要最有趣
###受众方面要考虑阅读者的角色和知识背景
###选择合适的映射方式

##3. 在传送足够信息前提下保持简洁

##4、将变量取值映射到图形元素上
### 坐标位置
### 尺寸
### 色彩
### 形状
### 文字

############################################################
# 二、利用基础绘图命令做统计图

if(!require(MASS)) install.packages("MASS")
data(UScereal)
head(UScereal)

############################################################
# A. 选择合适的统计图

# 单个连续变量的分布：One Continuous Variable

# Q: What is the Distribution of Variable X?
# Q: Is my Variable X normally distributed? Bimodal? Skewed?
# Q: Are there any outliers in my variable?

# Histogram
hist(UScereal$calories, breaks = 15)
hist(UScereal$calories, bin = 15)

# Boxplot
boxplot(UScereal$calories, horizontal = TRUE)


# 单个分类变量：One Categorical (Discrete) Variable

# Q: 各个类别是否均匀 evenly distributed?

barplot(table(UScereal$shelf))

plot(UScereal$shelf,type="p") # 没有意义

# 两个连续变量：Two Continuous Variables

# Q: Is there a relationship between Variable X and Variable Y?
# Q: If there is a relationship, is it linear? Quadratic? 

plot(x = UScereal$sugars, y = UScereal$calories)

plot(calories ~ sugars, data = UScereal) # formula notation

plot(UScereal[, c(2:8, 10)]) # 8个变量的scatterplot matrix


# 一个连续变量和一个分类变量
# One Continuous Variable and One Categorical Variable

# Q: Is the distribution of Variable Y, different across categories of Variable X?

boxplot(sugars ~ shelf, data = UScereal, horizontal = TRUE)


# 两个连续变量和一个分类变量
# Two Continuous Variables and One Categorical Variable

# Q: Is the relationship between Variable X and Y different across categories of Variable Z?

plot(calories ~ sugars, data = UScereal, col = shelf)

legend('topright', inset = .05, legend = c(3,2,1),
       fill = c('green', 'red', 'black'))

#2017年11月8日接着讲


##################################################
# B.添加其他图形元素

##########################
# 1、添加标题
# 方法1：在plot中用ylab, xlab and main参数
if(!require(MASS)) install.packages("MASS")
data(UScereal)

plot(calories ~ sugars, data = UScereal, ylab = 'Calories',
     xlab = 'Sugars (grams)', main = 'Nutrition of a Single Cup of Cereal')

# 方法2 使用title function添加标题元素
# 此时plot中设定 ann=FALSE 抑制原轴标题
# Turn off axes and annotations (axis labels)

plot(calories ~ sugars, data = UScereal, ann = FALSE)
title(main = 'Nutrition of a Single Cup of Cereal', ylab = 'Calories',
      xlab = 'Sugars (grams)') # add afterwards

##########################
# 2、修改图例 legend
# 在plot后使用legend function
plot(calories ~ sugars, data = UScereal, col = shelf)
legend('topleft', inset = .05, legend = c(3,2,1),
       fill = c('green', 'red', 'black'))

#########################
# 修改点形状和颜色 Point Shape and Color 
# Tip: Changing color or shape of points can be used to represent the same dimension

plot(calories ~ sugars, data = UScereal, pch = 15)

# Set a color to a factor variable, and R will use default colors
plot(calories ~ sugars, data = UScereal, pch = 19, col = shelf, bg = shelf)
legend('topright', inset = .05, legend = c(3,2,1),
       fill = c('green', 'red', 'black'))


# Use a palette of defined colors
palette(c('#e5f5f9', '#99d8c9', '#2ca25f'))

plot(calories ~ sugars, data = UScereal, pch = 19, col = shelf, bg = shelf)
legend('topright', inset = .05, legend = c(3,2,1),
       fill = c('#e5f5f9', '#99d8c9', '#2ca25f'))

# 颜色结合形状
plot(calories ~ sugars, data = UScereal, pch = shelf,col=shelf)

#如何还原默认颜色序列呢？
?palette  ## 如何查看帮助讲解1
palette("default")
plot(calories ~ sugars, data = UScereal, col=shelf)

#########################
# 3、给点加标签 Label points
# Label points with the text function
plot(calories ~ sugars, data = UScereal, pch = 15)

text(UScereal$sugars, UScereal$calories, row.names(UScereal),
     col = "red", pos = 1, cex = .5)  ##可以测试学生帮助的使用1

# The pos argument(1,2,3,4) 点的下左上右
plot(calories ~ sugars, data = UScereal, pch = 15)
text(UScereal$sugars, UScereal$calories, UScereal$mfr, col = "blue", pos = 2)

# 标记极端值 Identify Outliers

#（1）. 选出极端案例，以之作标签
plot(calories ~ sugars, data = UScereal, pch = 19)

outliers <- UScereal[which(UScereal$calories > 300),]

text(outliers$sugars, outliers$calories - 15, labels = row.names(outliers))


#（2）. 交互式选点-不太好选(windows可能好一点)
plot(calories ~ sugars, data = UScereal, pch = 19)

identify(UScereal$carbo, UScereal$calories, n = 2, labels = row.names(UScereal))     


#（3）. 修改axis limits to remove outliers from view
plot(calories ~ sugars, data = UScereal, pch = 19, ylim = c(0, 325))

########################
# 4.修改图形元素大小(text size, point size, label size etc..)

# Use cex argument family
plot(calories ~ sugars, data = UScereal, pch = 19, ann = FALSE, cex = 1.5)
outliers <- UScereal[which(UScereal$calories > 300),]
text(outliers$sugars, outliers$calories - 15,
     labels = row.names(outliers), cex = .75)
title(main = 'Nutrition of a Single Cup of Cereal', ylab = 'Calories',
      xlab = 'Sugars (grams)', cex.main = 2, cex.lab = 1.5)

# 上面我们使用得最多的是plot函数
# 它还有哪些用法和设定？R帮助文件介绍2.

###################################################
#C. Combine Graphs into the Same Window
##通过par读图形参数进行全局设定
par(mfrow = c(2, 2))

boxplot(calories ~ shelf, data = UScereal)
hist(UScereal$calories, breaks = 15)
boxplot(sugars ~ shelf, data = UScereal)
hist(UScereal$sugars, breaks = 15)

# 保存图片
dev.print(png,file="file1.png",width=480,height=640)

# 还原图形布局为单张图片
par(mfrow = c(1, 1)) 

# 查看可选项
names(par())

# 查看参数
par("col")  # graphical parameters color

###################################################
#D、示例通过基础命令叠加元素形成一个复杂的图
# 生成两个向量
cars <- c(1, 3, 6, 4, 9)
trucks <- c(2, 5, 4, 5, 12)
# 取得取值范围
g_range <- range(0, cars, trucks)

#作图，抑制坐标轴和标题
plot(cars, type="o", col="blue", ylim=g_range, 
     axes=FALSE, ann=FALSE)

# 定义x标签（1，2，3，4下左上右）
axis(1, at=1:5, lab=c("Mon","Tue","Wed","Thu","Fri"))

# 定义Y标签
axis(2, las=1, at=4*0:g_range[2])

# Create box around plot
box()

# Graph trucks with red dashed line and square points
lines(trucks, type="o", pch=22, lty=2, col="red")

grid(nx=NA,ny=NULL,lwd=2)

#  主标题 red, bold/italic font
title(main="Autos", col.main="red", font.main=4)

# Label the x and y axes with dark green text
title(xlab="Days", col.lab=rgb(0,0.5,0))
title(ylab="Total", col.lab=rgb(0,0.5,0))

# 图例设定 
legend(1, g_range[2], c("cars","trucks"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)

# ----------------------------------------------
# - 三 利用ggplot2作图 -
# ----------------------------------------------
# ggplot2 package (by Hadley Wickham)
# 请主要参看 R4DS 第3张visulisation
#主要内容
# 数据与映射，mapping
# 几何对象，geom
# 统计转换
# 位置调整
# 坐标系统
# 图层语法

library(ggplot2)

###################################################
# A. ggplot 函数

# 1、quick plot function
# 散点图
qplot(income,prestige, 
      xlab="Average Income", ylab="Prestige Score",
      geom=c("point", "smooth"), data=car::Prestige)

# 分色散点图
qplot(x = sugars, y = calories, color = as.factor(shelf),
      data = UScereal) 

# 多个图形元素
qplot(cty,hwy,
      data=mpg,
      geom=c("point", "smooth"))

# 分面
qplot(hwy,data=mpg,binwidth=0.5) +
  scale_x_continuous(breaks =10:45) +
  facet_wrap(~ drv, ncol = 1)

#2、ggplot function函数+ 图层

# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

p1 <- ggplot(UScereal)

p <- p1 + geom_histogram(aes(x = calories)) 
print(p)
summary(p)  ## 查看p的内部结构  两层内容

ggplot(UScereal)+ 
  geom_histogram(aes(x = calories)) 


###################################################
# B. 图层 Layers 

# 利用'+'在ggplot object基层上添加图层
p1 <- ggplot(UScereal, aes(x = calories))

p1 + geom_dotplot()

p1 + geom_density()

p1 + geom_histogram(binwidth = 10)


# 可以添加多个图层
p1 + 
  geom_histogram(binwidth = 10) +
  xlab('Calories in a Single Cup') +
  ylab('Frequency') + 
  ggtitle('Distribution of Calories in US Cereals') + theme_bw()


# 图层的顺序无关
p1 + geom_histogram(binwidth = 10) + xlab('Calories in a Single Cup') +
  ylab('Frequency') + ggtitle('Distribution of Calories in US Cereals') + 
  theme_bw() + theme(text = element_text(size = 30))


# 可以添加多个 geom_function
p2 <- ggplot(UScereal, aes(x = sugars, y = calories, color = mfr))

p2  + geom_point() + geom_line()

# ggplot 可以使用哪些几何函数呢？R帮助文件查看3，ggplot 进入index 查看

###################################################
#C、 五种named graph  分别用在什么情况下
# 教材I2SDR（An Introduction to Statistical and Data Sciences via R）
# 教材章节的使用（演示）

# scatterplots : geom_point() 连续变量*连续变量 注意alpha参数和geom_jitter() 
# linegraphs : geom_line()  时序变量*连续变量
# boxplots: geom_boxplot()  连续变量*分类变量
# histograms: geom_histogram()  连续变量 注意 bins或binwidth 参数
# barplots:geom_bar() 或者 geom_col() 分类变量 注意 position参数:簇状、叠加

##########
#基础条形图
p <- ggplot(mpg,aes(x=class))+
  geom_bar()

print(p)
# 按频次排序，方法1
ggplot(mpg,
       aes(x=reorder(class,class,
                     function(x) -length(x)))) +
  geom_bar()

# 按频次排序，方法2：先修改分类变量的排序，然后作图
## set the levels in order we want
theTable <- within(mpg, 
                   class <- factor(class, 
                                   levels=names(sort(table(class), 
                                                     decreasing=TRUE))))
## plot
ggplot(theTable,aes(x=class))+geom_bar( )

##按频次排序，方法3：汇总后作图
fdata <- as.data.frame(sort(table(mpg$class),decreasing = TRUE))
names(fdata)<-c("class","count")
ggplot(fdata,aes(x=class,y=count)) +
  geom_bar(stat = "identity")

#按频次排序， 方法4：汇总后作图
fdata <- as.data.frame(table(mpg$class))
names(fdata)<-c("class","count")
ggplot(fdata,aes(x=reorder(class,-count),y=count)) +
  geom_bar(stat = "identity")


mpg$year <- factor(mpg$year)

#频数叠加条形图
ggplot(mpg,aes(x=class,fill=year))+
  geom_bar(color='black')


#簇状条形图
ggplot(mpg,aes(x=class,fill=year))+
  geom_bar(color='black',
           position=position_dodge())

#百分比叠加图
ggplot(mpg,aes(x=class,fill=year))+
  geom_bar(position='fill') 

# 饼图
ggplot(mpg, aes(x = factor(1), fill = factor(class))) +
  geom_bar(width = 1)+ 
  coord_polar(theta = "y")

# 结构连续变化图
setwd("/Users/liding/E/Bdata/liding17/2017R/lesson7")
data <- read.csv('soft_impact.csv',T)
head(data)
library(tidyr)
data.melt <- gather(data,key=variable,value=value,-Year )

#用reshape2命令进行数据整理
#library(reshape2)
#data.melt <- melt(data,id='Year')

ggplot(data.melt,aes(x=Year,y=value,
                     group=variable,fill=variable))+
  geom_area(color='black',size=0.3,
            position=position_fill())+
  scale_fill_brewer()


## 直方图
ggplot(data=iris,aes(x=Sepal.Length))+ 
  geom_histogram()

ggplot(iris,aes(x=Sepal.Length))+ 
  geom_histogram(binwidth=0.1,   # 设置组距
                 fill='skyblue', # 设置填充色
                 colour='black') # 设置边框色
# 增加概率密度曲线 
ggplot(iris,aes(x=Sepal.Length)) +
  geom_histogram(aes(y=..density..),
                 fill='skyblue',
                 color='black') +
  stat_density(geom='line',color='black',
               linetype=2,adjust=2)

#调整平滑宽度，adjust参数越大，越平滑。
ggplot(iris,aes(x=Sepal.Length)) +
  geom_histogram(aes(y=..density..), # 注意要将y设为相对频数
                 fill='gray60',
                 color='gray') +
  stat_density(geom='line',color='black',linetype=1,adjust=0.5)+
  stat_density(geom='line',color='black',linetype=2,adjust=1)+
  stat_density(geom='line',color='black',linetype=3,adjust=2)

#面积曲线-分类
ggplot(iris,aes(x=Sepal.Length,fill=Species)) +
  geom_density(alpha=0.5,color='gray')



#箱子图-分类
ggplot(iris,aes(x=Species,y=Sepal.Length,fill=Species)) +
  geom_boxplot()

#小提琴图(扩展)
ggplot(iris,aes(x=Species,y=Sepal.Length,fill=Species)) +
  geom_violin()


#小提琴叠加点图(扩展)
ggplot(iris,aes(x=Species,y=Sepal.Length,
                     fill=Species)) +
  geom_violin(fill='gray',alpha=0.5)+
  geom_dotplot(binaxis = "y", stackdir = "center")


###################################################
# D. 选择和修改美学特征
# Aesthetics: x position, y position, size of elements, shape of elements, color of elements
# elements: geometric shapes such as points, lines, line segments, bars and text
# geomitries have their own aesthetics i.e. points have their own shape and size


# 在ggplot function中设定color 与 manufacturer 对应:
p2 <- ggplot(UScereal, aes(x = sugars, y = calories, color = mfr))

p2 + geom_point() 

my_colors <- c('#9ebcda', '#8c96c6', '#8c6bb1', '#88419d', '#810f7c', '#4d004b')

p2 + geom_point() + scale_color_manual(values = my_colors) 


# 或者在geom_point function中设定:
p2 <- ggplot(UScereal, aes(x = sugars, y = calories))

p2 + geom_point(aes(color = mfr)) 


# 给点添加标签 Adding Labels to points

# 使用 geom_text() layer
p2 + geom_point(aes(color = mfr)) + 
  geom_text(aes(label = row.names(UScereal)), hjust = 1.1)

p2 + geom_point(aes(color = mfr)) + 
  geom_text(aes(label = ifelse(calories>300,row.names(UScereal),"")), hjust = 1.1)


# 改变 point size
p2 + geom_point(aes(color = mfr), size = 4) 


# 编辑 legend

# Use the scale_color_manual() layer, and the color argument in the labs() layer 
p2 + geom_point(aes(color = mfr), size = 5) +labs(color = 'Manufacturer') + 
  scale_color_manual(values = c('blue', 'green', 'purple', 'navyblue', 'red', 'orange'),
                     labels = c('General Mills', 'Kelloggs', 'Nabisco', 'Post', 'Quaker Oats', 'Ralston Purina'))  + theme(text = element_text(size = 30)) 


###################################################
# E. 分面 Faceting  - divide a plot into subplots based on the valuesof one or more discrete variables

# Tip: Use facets to help tell your story
# Q: How is the distribution of sugar across different shelves?
# Q: Are cereals with higher sugar content on lower shelves/at a child's eye level?

p3 <- ggplot(UScereal, aes(x = sugars))

p3 + geom_histogram(binwidth = 4)

# Each graph is in a separate row of the window
p3 + geom_histogram(binwidth = 4) + facet_grid(shelf ~ .)

# Each graph is in a separate column of the window
p3 + geom_histogram(binwidth = 4) + facet_grid(. ~ shelf)

# Finished product 
p3 + geom_histogram(fill = '#3182bd', color = '#08519c', binwidth = 4) +
  facet_grid(shelf ~ .) + theme(text = element_text(size = 20)) + 
  labs(title = 'Are Sugary Cereals on Lower Shelves?',
       x = 'Sugars (grams)', y = 'Count')


###################################################
#F、在图中添加自定义的统计量，例如Box Plots 添加中位数
p4 <- ggplot(UScereal, aes(mfr, calories))
p4 + geom_boxplot()
p4 + geom_boxplot(notch = TRUE)
p4 + geom_violin()

p4 + geom_boxplot(outlier.shape = 8, outlier.size = 4, fill = '#3182bd') + coord_flip() + 
  labs(x = 'Manufacturer', y = 'Calories') + theme_bw() + 
  scale_x_discrete(labels = c('General Mills', 'Kelloggs', 'Nabisco', 'Post', 'Quaker Oats', 'Ralston Purina'))

# Add median value to boxplot
p4_meds <- UScereal %>% group_by(mfr) %>% summarise(med = median(calories))

p4 + geom_boxplot(outlier.shape = 8, outlier.size = 4, fill = '#8c96c6') + 
  labs(x = 'Manufacturer', y = 'Calories') + theme_bw() + 
  scale_x_discrete(labels = c('General Mills', 'Kelloggs', 'Nabisco', 'Post', 'Quaker Oats', 'Ralston Purina')) + 
  geom_text(data = p4_meds, aes(x = mfr, y = med, label = round(med,1)), size = 4, vjust = 1.2)


###################################################
#G、图表风格
library("ggthemes")
p2 + geom_point(aes(color = mfr), size = 4) +
  theme_stata()



###################################################
#ggplot作图练习:使用mpg数据构建图形，下面的命令都在做什么？
#请添加上适当的批语

#
p<-ggplot(data=mpg,mapping=aes(x=cty,y=hwy)) + geom_point()
# 
p+geom_text(aes(label=manufacturer),hjust=0, vjust=0)
#
p+geom_text(aes(label=ifelse(cty>30,manufacturer,'')),hjust=0,vjust=0)

# 不用factor函数有什么不同吗？试一试
p<-ggplot(data=mpg,mapping=aes(x=cty,y=hwy,colour=factor(year))) 
p + geom_point()

# 
p + stat_smooth() ## 平滑散点图

#下面的图，如果变成两条拟合曲线，怎么做？)

p<-ggplot(data=mpg,mapping=aes(x=cty,y=hwy)) +
	geom_point(aes(colour=factor(year))) +
	stat_smooth()
print(p)

# 如何来控制Scale标度呈现样式，修改相关的颜色参数试试？
p<-ggplot(data=mpg,mapping=aes(x=cty,y=hwy)) +
	geom_point(aes(colour=factor(year))) +
	stat_smooth() +
	scale_color_manual(values=c('blue2','red4'))
print(p)

#facet_wrap是在做什么？
p<-ggplot(data=mpg,mapping=aes(x=cty,y=hwy)) +
	geom_point(aes(colour=factor(year))) +
	stat_smooth() +
	scale_color_manual(values=c('blue2','red4')) +
	facet_wrap(~year,ncol=1)
print(p)

# 下面的命令做了哪些图形改进？
p<-ggplot(data=mpg,mapping=aes(x=cty,y=hwy)) +
	geom_point(aes(colour=class,size=displ),
				alpha=0.5,position = 'jitter') +
	stat_smooth() +
	scale_size_continuous(rang = c(4,10)) +
	facet_wrap(~year,ncol=1) +
	opts(title='汽车型号与油耗')+
	labs(y='每加仑高速公路行驶距离',
		x='每加仑城市公路行驶距离',
		size='排量',
		colour ='车型')
print(p)




###################################################
#lattice包
library(lattice)
num<-sample(1:3,size=50,replace=T)
barchart(table(num))
qqmath(rnorm(100))
#单维散点
stripplot(~Sepal.Length | Species,data=iris,layout=c(1,3)) # |表示条件
#密度
densityplot(~ Sepal.Length,groups=Species,data=iris,plot.points=FALSE)
#箱子图
bwplot(Species~ Sepal.Length, data = iris)
#散点图
xyplot(Sepal.Width~Sepal.Length,groups=Species,data=iris)
#矩阵散点
splom(iris[1:4])  # 矩阵散点图
#分面直方图
histogram(~Sepal.Length | Species,data=iris,layout(c(1,3)))


###################################################
##其他作图包
##REmap – 动态地图
#bigvis – 大数据集的可视化
#ggsci – 为ggplot2提供科技期刊所用的绘图风格
#rCharts – 生成动态交互图

# Scatterplot Matrix
install.packages('GGally')
library(GGally)
ggpairs(UScereal[, c(2, 8, 9, 11)],
        upper = list(continuous = 'smooth', combo = 'facetdensity', discrete = 'blank') ,
        lower = list(continuous = 'cor', combo = 'box'))

# Maps
# http://bcb.dfci.harvard.edu/~aedin/courses/R/CDC/maps.html
# http://rstudio.github.io/leaflet/
# maps, choroplethr, 

#玫瑰图
set.seed(1)
#随机生成100次风向，并汇集到16个区间内
dir <- cut_interval(runif(100,0,360),n=16)
#随机生成100次风速，并划分成4种强度
mag <- cut_interval(rgamma(100,15),4) 
sample <- data.frame(dir=dir,mag=mag)
#将风向映射到X轴，频数映射到Y轴，风速大小映射到填充色，生成条形图后再转为极坐标形式即可
p <- ggplot(sample,aes(x=dir,fill=mag))+
  geom_bar()+ coord_polar()

#马赛克图（用矩形面积表示份量）	  
library(vcd)
mosaic(Survived~ Class+Sex, data = Titanic,shade=T, 
       highlighting_fill=c('red4',"skyblue"),
       highlighting_direction = "right")

## 层次树图
library(treemap)
data <- read.csv('data/apple.csv',T)
treemap(data,
        index=c("item", "subitem"),
        vSize="time1206",
        vColor="time1106",
        type="comp",
        title='苹果公司财务报表可视化',
        palette='RdBu')


library(maps)
data(us.cities) 
big_cities <- subset(us.cities,long> -130)
ggplot(big_cities,aes(long,lat))+borders("state",size=0.5,colour="grey70")+geom_point(colour="black",alpha=0.5,aes(size = pop)) 

p <-ggplot(us.cities,aes(long,lat))+
  borders("state",colour="grey70")
p+geom_point(aes(long,lat,size=pop),data=us.cities,colour="black",alpha=0.5)
