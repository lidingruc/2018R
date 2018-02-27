#http://www.afenxi.com/post/5413
#三种绘制地图的方法：
#绘制基础地图
#基于空间数据格式（shapefile）绘制地图以及
#如何调用百度地图和谷歌地图的数据来绘制地图。

##基础地图
#install.packages("ggplot2")
#install.packages("maps")
library(ggplot2)
library(maps) # 为了获取数据

##
##  # ATTENTION: maps v3.0 has an updated 'world' map.        #
##  # Many country borders and names have changed since 1990. #
##  # Type '?world' or 'news(package="maps")'. See README_v3. #

# 美国地图数据
states_map <- map_data("state")
head(states_map)

##        long      lat group order  region subregion
## 1 -87.46201 30.38968     1     1 alabama      <NA>
## 2 -87.48493 30.37249     1     2 alabama      <NA>
## 3 -87.52503 30.37249     1     3 alabama      <NA>
## 4 -87.53076 30.33239     1     4 alabama      <NA>
## 5 -87.57087 30.32665     1     5 alabama      <NA>
## 6 -87.58806 30.32665     1     6 alabama      <NA>

# geom_polygon()
ggplot(states_map, aes(x=long,y=lat,group=group)) +
  geom_polygon(fill="white",colour="black") +
  labs(title = "USA Map")

#中国地图
if (!require('mapdata')) install.packages("mapdata")
library(mapdata)
map("china", col = "red4", ylim = c(18,54), panel.first = grid())
title("China Map")

# 世界地图
# 世界地图数据
world_map <- map_data("world")
head(world_map)
#plot(world_map)
#par(mfcol=c(1,1))
# 绘制指定区域的地图数据
# 绘制欧洲足球五大联赛所在地
euro <- map_data("world", region = c("UK","France", "Spain","Germany", "Italy"))
ggplot(euro, aes(x=long, y = lat, group=group,fill=region)) +
  geom_polygon(colour="black") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits=c(40,60)) +
  scale_x_continuous(limits=c(-25,25)) +
  labs(title = " Euorpe's Big Five Football Leagues")


#分层设色
head(USArrests) # 1973年的数据

##            Murder Assault UrbanPop Rape
## Alabama      13.2     236       58 21.2
## Alaska       10.0     263       48 44.5
## Arizona       8.1     294       80 31.0
## Arkansas      8.8     190       50 19.5
## California    9.0     276       91 40.6
## Colorado      7.9     204       78 38.7

crimes <- data.frame(state= tolower(rownames(USArrests)), USArrests)
# 合并数据集
crime_map <- merge(states_map,crimes,by.x="region",by.y = "state")
# head(crime_map)

library(plyr) # 加载数据清洗软件包

##
## Attaching package: 'plyr'
##
## The following object is masked from 'package:maps':
##
##     ozone

# 按照 group, order排序
crime_map <- arrange(crime_map,group,order)
# head(crime_map)

ggplot(crime_map, aes(x=long,y=lat, group = group, fill = Assault)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic") +
  labs(title = "USA Map")

# 更改配色
ggplot(crimes, aes(map_id = state, fill = Assault)) +
  geom_map(map = states_map, colour = "black") +
  scale_fill_gradient(low="#FFFFFF", high = "#BB4444") +
  expand_limits(x = states_map$long, y = states_map$lat)

# 离散颜色标度
qa <- quantile(crimes$Assault, c(0,0.2,0.4,0.6,0.8,1.0))
qa

##    0%   20%   40%   60%   80%  100%
##  45.0  98.8 135.0 188.8 254.2 337.0

# 新增一个分位数类别变量
crimes$Assault_q <- cut(crimes$Assault, qa, labels = c("0-20%", "20-40%","40-60%",
                                                       "60-80%", "80-100%"),
                        include.lowest = TRUE)
states <- ddply(states_map, .(region),summarise, lat = mean(lat,na.rm = TRUE),
                long = mean(long,na.rm = TRUE))
crimes <- merge(crimes, states, by.x = "state", by.y = "region")

# 绘制离散分类地图
p <- ggplot(crimes, aes(map_id = state, fill = Assault_q)) +
  geom_map(map = states_map, colour = "black") +
  scale_fill_brewer(palette = "Set2") +
  expand_limits(x = states_map$long, y =states_map$lat) +
  coord_map("polyconic") +
  labs(fill="Assault Rate\nPercentile", title = "USA Map")
p
# 加入州名对应的标签
p + geom_text(aes(x=long,y=lat,label=state),size=3,colour="black") +
  theme_bw() +
  xlab("long") + ylab("lat")

# 创建空白背景地图
theme_clean <- function(base_size=12){
  require(grid)
  theme_grey(base_size)
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(0, "cm"),
    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(0,0,0,0), "lines"),
    complete = TRUE
  )
}
p + theme_clean()

## Loading required package: grid

###############################
### 用shp文件作图
###地图下载常用的网站：
#http://www.diva-gis.org/gdata
#http://www.naturalearthdata.com/
#https://www.zhihu.com/question/19592414

## Loading required package: sp
## Checking rgeos availability: TRUE
library(maptools)

setwd("/Users/liding/E/Bdata/rtemp/analysis/example/ch17/")
# 载入空间数据并转化成数据框
china_shp <- readShapePoly("bou2_4p.shp")
# 返回一个 SpatialPolygonsDataFrame 对象
# str(china_shp)

china_map <- fortify(china_shp)

## Regions defined for each Polygons

# 绘制中国地图
p <- ggplot(china_map, aes(x = long, y = lat, group = group)) +
  geom_path() +
  labs(title = "China Map")
# 绘制空白背景的地图
p + theme_clean()

################

#火星坐标系统是一种国家保密插件，也叫做加密插件或者加偏或者SM模组，其实就是对真实坐标系统进行人为的加偏处理，按照特殊的算法，将真实的坐标加密成虚假的坐标，而这个加偏并不是线性的加偏，所以各地的偏移情况都会有所不同。而加密后的坐标也常被人称为火星坐标系统。
#所有的电子地图、导航设备，都需要加入国家保密插件。第一步，地图公司测绘地图，测绘完成后，送到国家测绘局，将真实坐标的电子地图，加密成“火星坐标”，这样的地图才是可以出版和发布的，然后才可以让GPS公司处理。第二步，所有的GPS公司，只要需要汽车导航的，需要用到导航电子地图的，都需要在软件中加入国家保密算法，将COM口读出来的真实的坐标信号，加密转换成国家要求的保密的坐标。这样，GPS导航仪和导航电子地图就可以完全匹配，GPS也就可以正常工作了。

#####
# 利用高德地图API获取行政边界
if (!require('RCurl')) install.packages("RCurl")
if (!require('rjson')) install.packages("rjson")
if (!require('plyr')) install.packages("plyr")
if (!require('stringr')) install.packages("stringr")
if (!require('ggplot2')) install.packages("ggplot2")

# get data
url <- "http://restapi.amap.com/v3/config/district?key=88e5280248b518ac3f7eaec5d6f68183&keywords=原州区&level=city&subdistrict=3&extensions=all"
json <- getURL(url)
list <- fromJSON(json)

# 中心点
center<-list$districts[[1]]$center
cname<-list$districts[[1]]$name
cpoint <- as.numeric(unlist(str_split(center , ",")))
lon <-cpoint[[1]]  
lat <-cpoint[[2]] 

cpoint <- data.frame(lon = lon, lat = lat,cname= cname)
#下属区域
dist <- list$districts[[1]]$districts[[1]]$districts
dname<- sapply(dist, function(x)x[3])
dname<-unlist(dname)
dpoint <- sapply(dist, function(x)x[4])
dpoint <- str_split(dpoint , ",")
lon <- sapply(dpoint, function(x)x[1])
lat <- sapply(dpoint, function(x)x[2])
dpoint <- data.frame(lon = lon, lat = lat,dname= dname)

dpoint[, 1:2] <- sapply(dpoint[, 1:2], function(x)as.numeric(as.character(x)))

# 边界线
line <- list$districts[[1]]$polyline
line <- str_replace(line, '\"', "")
line <- str_split(line, ";")
line <- line[[1]]
line <- str_split(line, ",")
lon <- sapply(line, function(x)x[1])
lat <- sapply(line, function(x)x[2])
point <- data.frame(lon = lon, lat = lat)

amap_table <- data.frame()
point$order <- 1:dim(point)[1]
point[, 1:3] <- sapply(point[, 1:3], function(x)as.numeric(as.character(x)))
amap_table <- rbind(amap_table, point)

# plot the map
#par(family='STKaiti')
ggplot(amap_table, aes(x = lon, y = lat, group = 1)) +
  geom_polygon(colour = 'grey10', fill = 'grey70') +
  geom_point(data=dpoint,aes(x=lon,y=lat)) +
  geom_text(data=dpoint,aes(label = dpoint$dname),family='STKaiti') +
  theme_bw()
#  geom_point(data=cpoint,aes(x=lon,y=lat),colour="red") +

write.table(beijing, "beijing.csv", row.names=FALSE, sep=",")

write.table(amap_table, "guyuan.csv", row.names=FALSE, sep=",")
# 计算点之间的距离
library(sp)
library(rgeos)
sp.dpoint<-dpoint
coordinates(sp.dpoint) <- ~lon+lat
sp.cpoint<-cpoint
coordinates(sp.cpoint) <- ~lon+lat

class(sp.dpoint)
#计算距离
d <- gDistance(sp.dpoint,sp.cpoint, byid=T)
d2km <- function(d){
  out <- d*60*1.852
  return(out)
}
dkm<-d2km(d) ## 1 degree
colnames(dkm)<-dpoint$dname
dkm[1,]

###百度地图API
library(devtools)
install_github('badbye/baidumap')
install_github('lchiffon/REmap')
devtools::install_github("dkahle/ggmap")
#devtools::install_github("hadley/ggplot2")

options(baidumap.key = '9c9978a893e2dd6c3be3d3241f8e8f61')
library(baidumap)
library(ggmap)
#  lizi https://github.com/dkahle/ggmap

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)


#http://blog.csdn.net/lotterychampion/article/details/51404364

#1. getBaiduMap函数
#2.getCoordinate函数:根据地址得到经纬度
#3.getLocation：通过经纬度得到地址
#4.getPlace函数：返回地图搜索结果
#5.getRoute函数:通过搜索得到路线
# 感觉精度不是特别准
q <- getBaiduMap('固原市彭堡镇', width=1000, height=1000, zoom=13, scale = 1, messaging=FALSE,color='bw')
pengpu<-c('彭堡镇姚磨村','彭堡镇硝沟村','彭堡镇申庄村','彭堡镇闫堡村','彭堡镇彭堡村','彭堡镇蒋口村','彭堡镇河东村','彭堡镇吴磨村','彭堡镇撒门村','彭堡镇臭水沟村','彭堡镇石碑村','彭堡镇里沟村','彭堡镇别庄村','彭堡镇杨忠堡村','彭堡镇曹洼村')
pengpugeo<-as.data.frame(getCoordinate(pengpu,formatted = T)) 
ggmap(q) + geom_point(data = pengpugeo, aes(longtitude, latitude)) #绘制地图

#获取北京大学的地图信息
#可以同时多个地点
getCoordinate(c('北京大学', '清华大学', '人民大学'), formatted = T)  

getCoordinate("北京市昌平区南口镇龙虎台村",formatted=T)
getLocation(c(118.12845, 24.57232),formatted = T)

#同样可以返回多个位置
getLocation(c(118.12845, 24.57232,116.31234,40.56125),formatted = T)

#fromJSON函数的用法
library(rjson)
js <- getLocation(c(116.31617,39.99775),output='json')  #json格式
fromJSON(js) #返回一个列表，包含了该地址下的

## 对于含多个经纬度的矩阵
loc = matrix(c(117.93780, 24.55730, 117.93291, 24.57745, 117.23530, 24.64210, 117.05890, 24.74860), byrow=T, ncol=2)

### 得到json格式
location_json = getLocation(loc, output='json')


### 设计一个函数返回district，即所属的区
getDistrict = function(x_json){
  x_list = fromJSON(x_json)     #json转化为list
  x_list$result$addressComponent$district  #返回所属的区
}

location_district = sapply(location_json, getDistrict) #运用上面的函数到这个json对象上
location_district

#查找北京的大学
bj_college = getPlace('大学','北京')
head(bj_college)

## Mcdonald's in shanghai
sh_mcdonald = getPlace('麦当劳', '上海')
head(sh_mcdonald)

pengbu = getPlace('彭堡', '固原')
head(pengbu)

# 路径
bjMap = getBaiduMap('北京',color = 'bw')
df = getRoute('首都国际机场', '北京南苑机场')
ggmap(bjMap) + geom_path(data = df, aes(lon, lat), alpha = 0.5, col = 'red')
head(df)  #此处对getRoute有一个直观认识，得到的是路线上的经纬度

####################
#莆田医院的可视化（R语言版——可以运行）
##http://www.xueqing.tv/cms/article/199
##获取数据
raw = readLines("http://news.ifeng.com/mainland/special/ptxyy/",
                encoding = "UTF-8")
rawHospital = raw[123 : 238]
## 数据整理
extFun = function(x){
  split = strsplit(x, "','")
  out = strsplit(split[[1]][2], "<br/>")
  return(out)
}
extFun(rawHospital[1])
hosList = sapply(rawHospital,extFun)
hospital = c()
for(i in 1:length(hosList)) hospital = append(hospital, hosList[[i]])
## 获取经纬度
library(baidumap)
blackHospital = getCoordinate(hospital, formatted = T)
blackHospital = na.omit(blackHospital)
plotdata = data.frame(lon = blackHospital[,1],
                      lat = blackHospital[,2],
                      city = rownames(blackHospital))
## 绘制地图
library(REmap)
remapB(markPointData = data.frame(plotdata$city),
       markPointTheme = markPointControl(symbol = "pin",
                                         effect=F,
                                         symbolSize = 5,
                                         color="red"),
       geoData = plotdata)

# 例子2 https://ask.hellobi.com/blog/ecnuliuyang/5613

set.seed(20161126)
pop <- runif(19,0,1)
stu<-data.frame( district = mapNames('shanghai'), values = round(pop*100) )
stu<-stu[order(stu[,2],decreasing =T),]
head(stu)

library(REmap)
markLine_data <- data.frame(origin=rep("华东师范大学闵行校区",5), destination=stu[1:5,1] )
markPoint_data <- as.character(unlist(markLine_data[,2]))
remapC(stu, maptype = '上海',
       title="模拟的迁徙图", subtitle="迁徙人数前5的目标省份",
       theme=get_theme( theme="Dark", titleColor = "#FFFFFF"),
       maxdata = 100,mindata=0,
       color=c('#FF0000','#C0C0C0'),   #颜色渐变:红－>灰
       markLineData=markLine_data,   
       markLineTheme=markLineControl( symbolSize = 6, effect =T, color="white", lineWidth=4, lineType="dashed" ),
       markPointData=markPoint_data,
       markPointTheme=markPointControl( symbol = 'heart', symbolSize=12, effect=T, effectType = 'bounce', color="white")
)

library(baidumap)
ll<-matrix(0,19,2)
for(i in 1:19)  ll[i,]<- get_city_coord(as(stu$district[i],'character')) # 比 getCoordinate 精度高
data<-cbind(stu,ll)[,c(3,4,2)]
theme <- get_theme(theme ="none", titleColor ="red", backgroundColor ="white", borderColor ="blue", labelShow = T)
remapH(data, title = '模拟热力图',
       maptype = '上海',
       theme = theme,
       blurSize = 60,
       color = "blackred",
       minAlpha= 1,
       opacity = 1
)



# 使用google地图，可能需要翻墙
#http是普通超文本协议，其信息室明文传送，而https就是安全超文本传输协议，需要证书和提供安全连接，https是嵌套了SSL加密的http连接，其内容会由SSL先加密，然后再传送。
ad1 <- as.numeric(geocode("福建省厦门市思明南路422",source = "google"))
xmu <- get_map("厦门市思明区",zoom = 13, maptype = "roadmap")
ggmap(xmu, extent = "normal") +
  geom_point(aes(x=ad1[1], y =ad1[2]))

## ggmap实例
#https://web.archive.org/web/20130311084317/http://xccds1977.blogspot.com/2012/06/ggmap.html
#最近又发现了一个比较好玩的包ggmap。从名字上可以猜测出来，它的作用就是将ggplot2和map相结合。这样R语言用户能方便的获取各种静态地图数据，并在其基础上使用强大的ggplot绘图工具。ggmap包整合了四种地图资源，分别是Google、OpenStreetMaps、Stamen和Cloudmade。为了演示ggmap的作用，本例是从地震信息网获取最近一周的地震数据，得到其经纬度，然后以散点形式绘制在google地图上，另外也显示地震发生的密度估计。这个思路本质上和之前的一篇博文是一致的，但用ggmap包实现起来更为简单。

# 加载扩展包
library(ggmap)
library(animation)
library(RCurl)
library(XML)
# 从网页上抓取数据，并进行清理
webpage <-'https://web.archive.org/web/20130305074751/http://data.earthquake.cn:80/datashare/globeEarthquake_csn.html'
#注意，得到的是文本
html<-getURL(webpage,.encoding='GB2312')
tables <- readHTMLTable(html,stringsAsFactors = FALSE)
raw <- tables[[9]]
data <- raw[-1,c(1,3,4)]
names(data) <- c('date','lan','lon')
data$lan <- as.numeric(data$lan)
data$lon <- as.numeric(data$lon)
data$date <- as.Date(data$date,  "%Y-%m-%d")
# 用ggmap包从google读取地图数据，并将之前的数据标注在地图上
# google 不可用，用stamen
chi2<-get_map(location = 'china', zoom=4,maptype='terrain',source = "stamen")

ggmap(chi2)+
  geom_point(data=data,aes(x=lon,y=lan),colour = 'red',alpha=0.7)+
  stat_density2d(aes(x=lon,y=lan,fill=..level..,alpha=..level..),
                 size=2,bins=4,data=data,geom='polygon')+
  options(legend.position = "none")

#更好玩的作法就是根据地震发生的日期生成不同的静态图，然后用animaiton包将其整合为一个gif动画。
# 通过brew安装了GraphicsMagick
# https://github.com/aheckmann/gm/wiki/Installing-ImageMagick---GraphicsMagick
# https://brew.sh/
# https://stackoverflow.com/questions/17756587/installing-graphicsmagick-on-mac-os-x-10-8

# 将GraphicMagick的路径放到PATH变量中
# 修改方法：http://architectryan.com/2012/10/02/add-to-the-path-on-mac-os-x-mountain-lion/#.WV8AgNOGMnU
# 路径在 /usr/local/Cellar/graphicsmagick/1.3.25/bin/gm  

# animation安装新版本：https://github.com/yihui/animation/releases/tag/v2.5

# 在R中查看系统属性
#Sys.getenv()
#Sys.setenv(https_proxy="")
#Sys.setenv(https_proxy="https://duotai:yR_nIc-U2XBs@marriott.h.xduotai.com:10369")
# 为了生成动画，先准备好一个绘图函数
Sys.getenv("PATH")
# Sys.setenv(PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/opt/local/bin")

plotfunc <- function(x) {
  df <- subset(data,date <= x)
  df$lan <- as.numeric(df$lan)
  df$lon <- as.numeric(df$lon)
  p <- ggmap(chi,extent='device') +
    geom_point(data=df,aes(x=lon,y=lan),colour = 'red',alpha=0.7)
}
# 获取地震的日期
time <- sort(unique(data$date))
# 生成并保存动画，HTML格式
#for( i in time) print(plotfunc(i))
saveHTML(for( i in time) print(plotfunc(i)),htmlfile='earthquake.html')

#参数设定
ani.options(gm.convert="/usr/local/Cellar/graphicsmagick/1.3.25/bin/")
ani.options(interval=1)
saveGIF(for( i in time) print(plotfunc(i)),movie.name = "ekanimation.gif",convert='gm convert')

## 一个简答的图
saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
},convert ='gm convert',movie.name='aplot')

#ggmap包中还有其它一些非常有用的函数。例如geocode函数可以根据地名字符串来查询经纬度，gglocator类似于基本包中的locator，它根据鼠标的点选来返回其坐标值。另外一个是mapdist函数，可以返回两点之间的地图距离和行驶时间。结合这几个函数可以直接在R中绘制地图，选择你的出发地和目标地，然后获得两地之间的距离。当然你还可以配合GPS等其它数据，创造出其它有意思的图形。


##ggmap 示例
library(dplyr)
# only violent crimes
violent_crimes <- filter(crime, 
                         offense != "auto theft", offense != "theft", offense != "burglary"
)
# rank violent crimes
violent_crimes$offense <- factor(
  violent_crimes$offense,
  levels = c("robbery", "aggravated assault", "rape", "murder")
)

# restrict to downtown
violent_crimes <- filter(violent_crimes,
                         -95.39681 <= lon & lon <= -95.34188,
                         29.73631 <= lat & lat <=  29.78400
)
#点图
qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite", color = I("red"))
#等高线
qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite", geom = "density2d", color = I("red"))
#热力
robberies <- violent_crimes %>% filter(offense == "robbery")

qmplot(lon, lat, data = violent_crimes, geom = "blank", zoom = 15, maptype = "toner-background", darken = .7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Robbery\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 650)

# 分面、分色
qmplot(lon, lat, data = violent_crimes, maptype = "toner-background", color = offense) + 
  facet_wrap(~ offense)

library(rCharts) 
names(iris) = gsub("\\.", "", names(iris))
p1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point') 
p1

## 计算不同地点的交通距离
#https://stackoverflow.com/questions/1042885/using-google-maps-api-to-get-travel-time-data
#https://developers.google.com/maps/documentation/distance-matrix/start
# google必须展示地图
#https://www.quora.com/Alternative-to-Google-Maps-Distance-Matrix-API
#https://developer.mapquest.com/documentation/directions-api/route-matrix/post/
  
  
  
  
  