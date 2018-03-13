
# 基础
library(tidyverse)  #hadley 组合
library(lubridate)  #日期处理
library(foreign)    #读取外部数据
library(haven)      #读取外部数据
library(readxl)     #读取EXCEL数据
library(devtools)   #安装其他来源的包
library(rmarkdown)  #结果呈现 rmarkdown
library(knitr)      #结果呈现 rmarkdown
library(extrafont)  #额外字体


#可视化
library(gganimate)    #安装可能较难，动态
library(animation)    #动态图
library(lattice)      #多变量作图 
library(RColorBrewer) #调色板
library(rgl)          #三维动态图
library(scales)       #调整映射坐标

#数据和分析
library(data.table)  #数据处理
library(DT)          #数据处理
library(reshape2)    #数据变形、合并
library(car)         #统计配套
library(arm)         #统计模型
library(plyr)        #数据处理
library(MASS)        #统计配套
library(effects)     #模型效应展示
library(lme4)        #固定效应模型
library(sem)         #结构方程模型
library(stargazer)   #统计模型输出
library(modelr)      #模型结果整理
library(splines)     #样条
library(lmtest)      #线性回归检验
library(sandwich)    #稳健协方差矩阵
library(sjPlot)      #sj绘图
library(sjmisc)      #sj统计


#空间分析相关
library(maps)         ## Projections
library(maptools)     ## Data management
library(sp)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(splancs)      ## Kernel Density
library(spatstat)     ## Geostatistics
library(pgirmess)     ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(spgwr)        ## GWR
library(rgdal)        ## 读入地图数据
library(tmap)         ## 制作主题地图
library(sf)           ## 数据处理
library(viridis)      ## 作图
library(raster)       ## 数据处理
library(geosphere)    ## 空间计算


# 网络分析相关
library(igraph)      ## 社会网数据作图
library(visNetwork)  ## 可视化
library(network)     ## 网络数据处理
 



#文本处理相关
library(rJava);       # 安装较难
library(tm)           # 文本挖掘 
library(tmcn);        #中文文本挖掘
library(Rwordseg);    #分词
library(wordcloud)    #词云
library(wordcloud2)   #词云
library(jiebaR)       #结巴分词
library(topicmodels)  #主题模型


# 爬虫相关
library(rvest)        #获取数据
library(httr)         #获取、解析
library(RCurl)        #获取
library(rjson)        #json解析
library(XML)          #解析
library(xml2)         #解析
library(stringi)      #文本
library(RSelenium)    #动态抓取、需要配套
library(selectr)      #css标记提取


#批量安装包 示意
install.packages(c("car", "devtools", "DiagrammeR", "effects", "ggplot2", "knitr", "lme4", "matlib", "MBESS", "mi", "polycor", "readxl", "rgl", "rmarkdown", "sem", "sfsmisc"))

libs <- c("plyr", "tidyverse", "arm")
inpkg <- function(x){
  if (!require(x)) install.packages(x)	
}

sapply(libs, inpkg)



#安装github上的包
library(devtools)
install_github("huashan/ezdf")
install_github('huashan/pander')

# 如果没有则安装
if (!require(tidyverse)) install.packages('tidyverse')	
if (!require(data.table))install.packages('data.table')
if (!require(sjPlot))install.packages('sjPlot')
if (!require(sjmisc))install.packages('sjmisc')
if (!require(haven)) install.packages('haven')
if (!require(maps)) install.packages("maps")
if (!require(maptools)) install.packages("maptools")
if (!require(sp)) install.packages("sp")
if (!require(spdep)) install.packages("spdep")
if (!require(gstat)) install.packages("gstat")
if (!require(splancs)) install.packages("splancs")
if (!require(spatstat)) install.packages("spatstat")
if (!require(lattice)) install.packages("lattice")
if (!require(pgirmess)) install.packages("pgirmess")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(classInt)) install.packages("classInt")
if (!require(spgwr)) install.packages("spgwr")


#查看包里的数据
data(tips,package='reshape2')


#查看包的帮助
help(package="tidyverse")


