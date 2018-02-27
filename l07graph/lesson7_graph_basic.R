############################################################
# 二、绘图基础 (traditional S/R graphics)

args(plot.default)  # default plot method
# 默认方法的内容  type="p"表示散点图
# 帮助文件中可以看各个参数有哪些选项
?plot
?plot.default

# points, lines, axes, frames

# windows()  # 打开独立的图形窗口  for windows system
# X11() # for Mac system

plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")  # coordinate system

plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", axes=FALSE) # w/o axes

# 查看可选项
names(par())

# 查看参数
par("col")  # graphical parameters color


# 查看帮助
?par

# 例如，将作图窗口分为1行*2列
par(mfrow=c(1, 2))  # array of plots

###################################################
## 点线形状、轴标签、线型
# The pch argument changes the shape of the points
plot(1:25, pch=1:25, xlab="Symbol Number", ylab="")  # symbols
lines(1:25, type="S", lty="dashed") # type p l b c o s S h

## 字符、轴、外边框
plot(26:1, pch=letters, xlab="letters", ylab="",
     axes=FALSE, frame.plot=TRUE)
# no plot 
plot(c(1, 7), c(0, 1), type="n", axes=FALSE,  # lines
     xlab="Line Type (lty)", ylab="")

# add frame
box() 

###################################################
# 设置坐标轴方位、标签位置、线型
axis(1, at=1:6)  # x-axis 1 在南 2在西 3在北 4 在东
axis(2, at=c(0,0.5,1)) 
for (lty in 1:6) 
  lines(c(lty, lty, lty + 1), c(0, 0.5, 1), lty=lty)

# 坐标系、直线
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")

abline(0, 1) # intercept and slope 加参照线
abline(c(1, -1), lty="dashed")  # 过两点
# horizontal and vertical lines: 
abline(h=seq(0, 1, by=0.1), v=seq(0, 1, by=0.1), col="gray")

###################################################
# # 标题 Titles 
plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="",
     main="(a)",frame.plot=TRUE) 

# Tip: We want titles to meaningful, non-repetitive and include units when applicable

# 方法1：在plot中用ylab, xlab and main参数
if(!require(MASS)) install.packages("MASS")
data(UScereal)
plot(calories ~ sugars, data = UScereal, ylab = 'Calories',
     xlab = 'Sugars (grams)', main = 'Nutrition of a Single Cup of Cereal')


# 方法2 使用title function
# 此时plot中设定 ann=FALSE 抑制原轴标题
plot(calories ~ sugars, data = UScereal, ann = FALSE)
title(main = 'Nutrition of a Single Cup of Cereal', ylab = 'Calories',
      xlab = 'Sugars (grams)') # add afterwards

###################################################
# 图例 legend
# 在plot后使用legend function

plot(c(1,5), c(0,1), axes=FALSE, type="n", xlab="", ylab="",
     frame.plot=TRUE)

# 手工确定位置,字符，线型、符号、颜色
legend(locator(1), legend=c("group A", "group B", "group C"),
       lty=1:3, pch=1:3, col=c("blue", "green", "red"))

# 右上方
legend("topright", legend=c("group A", "group B", "group C"),
       lty=1:3, pch=1:3, col=c("blue", "green", "red"), inset=0.01)

plot(calories ~ sugars, data = UScereal, col = shelf)
legend('topright', inset = .05, legend = c(3,2,1),
       fill = c('green', 'red', 'black'))


###################################################
# # 文本位置
text(x=c(0.2, 0.5), y=c(0.2, 0.7),  
     c("example text", "another string"))

plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="",
     frame.plot=TRUE, main="(b)")
# 手动添加left-click 3 times，请点击右侧图形三次。
text(locator(3), c("one", "two", "three"))  

locator() 
# 获得鼠标点击坐标，esc退出 


###################################################
# arrows and line segments（线段）
?arrows  # 可以查看可以设定的参数
# 箭头
plot(c(1, 5), c(0, 1), axes=FALSE, type="n", xlab="", ylab="")
arrows(x0=1:5, y0=rep(0.1, 5),   # 起点
       x1=1:5, y1=seq(0.3, 0.9, len=5), code=2) 
# 终点 已经箭头类型，1起点箭头,2终点箭头， 3两头有箭头，
title("(a) arrows")

# 线段
plot(c(1, 5), c(0, 1), axes=FALSE, type="n", xlab="", ylab="")
segments(x0=1:5, y0=rep(0.1, 5),
         x1=1:5, y1=seq(0.3, 0.9, len=5))
title("(b) segments")

# restore single panel
par(mfrow=c(1, 1)) 

###################################################
# 其他更漂亮的箭头（从略）
# nicer arrows: p.arrows() in the sfsmisc package
# note different arguments, unidirectional arrows
# question: how can you easily get bidirectional arrows?
# library(shape) 中的Arrows

if(!require(sfsmisc)) install.packages("sfsmisc")
plot(c(1, 5), c(0, 1), axes=FALSE, type="n", xlab="", ylab="")
p.arrows(x1=1:5, y1=rep(0.1, 5),
         x2=1:5, y2=seq(0.3, 0.9, len=5), fill="black")
# 反向箭头
p.arrows(x1=1:5, y1=seq(0.3, 0.9, len=5),
         x2=1:5, y2=rep(0.1, 5),
         fill="black")

# 多边形 polygon
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
polygon(c(0.2, 0.8, 0.8), c(0.2, 0.2, 0.8), col="red")
polygon(c(0.2, 0.2, 0.8), c(0.2, 0.8, 0.8))


####################################################
# curve

?plotmath   # 如何在图中输入公式

curve(x*cos(25/x), 0.01, pi, n=1000)

curve(sin, 0, 2*pi, ann=FALSE, axes=FALSE, lwd=2)
axis(1, pos=0, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c(0, expression(pi/2), expression(pi),
              expression(3*pi/2), expression(2*pi)))  # expression

axis(2, pos=0) # 坐标位置
curve(cos, add=TRUE, lty="dashed", lwd=2) # 添加到已有图上
legend(pi, 1, lty=1:2, lwd=2, legend=c("sine", "cosine"), bty="n")


#####################################################
# colors
pie(rep(1, length(palette())), col=palette())

palette()

# Hex 16进位： 0 1 2 ...9 A B C D E F
# 00 表示0， FF=16^2-1
# RGB原色 TT 透明度
rainbow(10) # "#RRGGBBTT" (red, green, blue, transparency)
pie(rep(1, 10), col=rainbow(10))

# 设定特定的颜色
pie(rep(1, 3), col=c("#FF0000FF","#00FF00FF","#0000FFFF"))

# 不同的灰
gray(0:9/9) # "#RRGGBB"
pie(rep(1, 10), col=gray(0:9/9))

# 定义好的color
length(colors())
head(colors(), 20) # first 20 named colors
pie(rep(1, 20), labels=head(colors(), 20), col=head(colors(), 20))


# for palettes based on psychophysical principles, see colorspace package

######################################################
# 利用这些基础命令，可以做出各种各样的示意图来
######################################################
# 标准正态分布示意图1（标定取值大于1.96部分带阴影）

# 设定边距
oldpar <- par(mar = c(5, 6, 4, 2) + 0.1)    # leave room on the left
oldpar  # old parameter saved
# z 1000个取值
z <- seq(-4, 4, length=1000)
# 密度取值
p <- dnorm(z)
# 作图
plot(z, p, type="l", lwd=2,
     main=expression("The Standard Normal Density Function" ~~ phi(z)),  ## 加空格用符号 ~~
     ylab=expression(phi(z) ==
                       frac(1, sqrt(2*pi)) * ~~ e^- ~~ frac(z^2, 2)))
abline(h=0, col="gray")
abline(v=0, col="gray")
# 可以一次添加两条线 abline(h=0, v=0 ), col="gray")
# 增加面积部分，得到坐标
z0 <- z[z >= 1.96]    # define region to fill
# 左侧起点
z0 <- c(z0[1], z0)
# 上方点，加一个0
p0 <- p[z >= 1.96]
p0 <- c(0, p0)

# 画出阴影部分
polygon(z0, p0, col="gray")
# 手工确定示意公式标签的头尾首尾位置
coords <- locator(2)    
arrows(coords$x[1], coords$y[1], coords$x[2], coords$y[2], code=1,
       length=0.125)
text(coords$x[2], coords$y[2], pos=3,   # text above tail of arrow
     expression(integral(phi(z)*dz, 1.96, infinity) == .025))

##########################
#标准正态分布示意图1（标定-3-3整数位置上的竖线）

par(oldpar)  # restore graphics parameters

# 利用上面参数画出合适大小的空画布
plot(z, p, type="n", xlab="", ylab="", axes=FALSE,
     main=expression("The Standard Normal Density Function" ~~ phi(z)))

# 坐标轴
axis(1, pos=0, at=-3:3)
abline(h=0)
axis(2, pos=0, at=.1*1:3)
abline(v=0)

# 曲线
curve(dnorm, -4, 4, n=1000, add=TRUE, lwd=2)

# 轴标签和曲线标签放在什么位置
text(locator(2), c("z", expression(phi(z))), xpd=TRUE)
# 画出竖直线
for (z0 in -3:3) lines(c(z0, z0), c(0, dnorm(z0)), lty=2)


#####################
# explaining nearest-neighbour kernel regression
# 示例核平滑的算法
oldpar <- par(mfrow=c(2,2), las=1)   # 2 x 2 array of graphs

library(car) # for data

UN <- na.omit(UN)
gdp <- UN$gdp
infant <- UN$infant.mortality
ord <- order(gdp)   # 得到GDP的位次，作为排序依据
gdp <- gdp[ord]
infant <- infant[ord]

x0 <- gdp[150]          # focal x = x_(150)
dist <- abs(gdp - x0)   # distance from focal x
h <- sort(dist)[95]     # bandwidth for span of .5 (where n = 190)
pick <- dist <= h       # observations within window

# upper-left panel

plot(gdp, infant, xlab="GDP per Capita", ylab="Infant-Mortality Rate",
     type="n", main="(a) Observations Within the Window\nspan = 0.5")
points(gdp[pick], infant[pick], col="blue")
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0, col="red")    # focal x
abline(v=c(x0 - h, x0 + h, col="blue"), lty=2)  # window
text(x0, par("usr")[4] + 10, expression(x[(150)]), xpd=TRUE, col="red") 
# above plotting region

# upper-right panel

plot(range(gdp), c(0,1), xlab="GDP per Capita",
     ylab="Tricube Kernel Weight",
     type="n", main="(b) Tricube Weights")
abline(v=x0, col="red")
abline(v=c(x0 - h, x0 + h), lty=2, col="blue")

# function to calculate tricube weights

tricube <- function(x, x0, h) {
  z <- abs(x - x0)/h
  ifelse(z < 1, (1 - z^3)^3, 0)
}

tc <- function(x) tricube(x, x0, h) # to use with curve

curve(tc, min(gdp), max(gdp), n=1000, lwd=2, add=TRUE)
points(gdp[pick], tricube(gdp, x0, h)[pick], col="blue", pch=16)
abline(h=c(0, 1), col="gray")

# lower-left panel

plot(gdp, infant, xlab="GDP per Capita", ylab="Infant-Mortality Rate",
     type="n", main="(c) Weighted Average (Kernal Estimate)")
points(gdp[pick], infant[pick], col="blue")
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0, col="red")
abline(v=c(x0 - h, x0 + h), lty=2, col="blue")
yhat <- weighted.mean(infant, w=tricube(gdp,  x0, h))  # kernel estimate
lines(c(x0 - h, x0 + h), c(yhat, yhat), lwd=3, col="red") # line at kernel estimate
text(x0, yhat, expression(widehat(y)[(150)]), adj=c(0, 0), col="red")

# lower-right panel

plot(gdp, infant, xlab="GDP per Capita", ylab="Infant-Mortality Rate",
     main="(d) Complete Kernel Estimate")
yhat <- numeric(length(gdp))
for (i in 1:length(gdp)){   # kernel estimate at each x
  x0 <- gdp[i]
  dist <- abs(gdp - x0)
  h <- sort(dist)[95]
  yhat[i] <- weighted.mean(infant, w=tricube(gdp, x0, h))
}
lines(gdp, yhat, lwd=2, col="red")

par(oldpar)  # restore plotting parameters

#######################################################
#一页多图，位置的细节性调整
# using par(mfrow=c(m, n))

par(mfrow=c(2, 2))

# 二次函数+随机波动
x <- seq(0, 1, length=200)
Ey <- rev(1 - x^2)
y <- Ey + 0.1*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(a) monotone, simple", 
     cex.main=1, xlab="", ylab="")
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

# 对数函数+随机波动
x <- seq(0.02, 0.99, length=200)
Ey <- log(x/(1 - x))
y <- Ey + 0.5*rnorm(200)
plot (x, y, axes=FALSE, frame=TRUE, main="(b) monotone, not simple", 
      cex.main=1, xlab="", ylab="")
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

# 二次函数+随机波动
x <- seq(0.2, 1, length=200)
Ey <- (x - 0.5)^2
y <- Ey + 0.04*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(c) non-monotone, simple", 
     cex.main=1, xlab="", ylab="")
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

# 上图布局不平衡，下面试图将第三个图放在正中间

# 先设定边缘间距：外边框 行数，留给顶端标题，四边
par(oma=c(0, 0, 1, 0), mar=c(2, 3, 3, 2)) 

# 重新做上面三个图、修改了一些特征，如点的颜色改灰
# 左上图
par(fig=c(0, .5, .5, 1))
# 数字表示比例，横向放在0-0.5。纵向放在0.5-1
# par(fig=c(x1, x2, y1, y2))

x <- seq(0, 1, length=200)
Ey <- rev(1 - x^2)
y <- Ey + 0.1*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(a) monotone, simple", 
     cex.main=1, xlab="", ylab="", col="gray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

# 右上图 
par(fig=c(.5, 1, .5, 1)) # top-right panel
par(new=TRUE)  # 在原图中开一个新窗口
x <- seq(0.02, 0.99, length=200)
Ey <- log(x/(1 - x))
y <- Ey + 0.5*rnorm(200)
plot (x, y, axes=FALSE, frame=TRUE, main="(b) monotone, not simple", 
      cex.main=1, xlab="", ylab="", col="gray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

# 下方图
par(fig=c(.25, .75, 0, .5)) # bottom panel
par(new=TRUE)
x <- seq(0.2, 1, length=200)
Ey <- (x - 0.5)^2
y <- Ey + 0.04*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(c) non-monotone, simple", 
     cex.main=1, xlab="", ylab="", col="gray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)
title("Nonlinear Relationships", outer=TRUE) # 标题

# clean up
remove(list=objects())  
par(mfrow=c(1, 1))
par(mar = c(5, 6, 4, 2) + 0.1)

###################################################
# 通过叠加元素形成一个复杂的图
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

