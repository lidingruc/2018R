# 导入上海地图 #
library(maptools)
shanghai<- readShapeSpatial("/town")
shanghai.t<- readShapeSpatial("/county")

# 导入社会组织数据：STATA格式#
library(foreign)
t0<-read.dta("/NGO-SH-2013.dta")
t0<-subset(t0,t0$fyear<=2010)
levels(t0$orgtype)<-c("Social Group","NGO","Foundation")

#将STATA数据转化为地图 #
t1<-SpatialPointsDataFrame(data.frame(as.numeric(t0$lon),as.numeric(t0$lat)),t0)

# 计算每个街道中的社会组织数目 #
k=over(t1,shanghai)
t1@data=data.frame(t1@data,k$CODE)
names(t1)[names(t1)=="k.CODE"]="CODE"
t1@data$n=1

number=tapply(t1@data$n,data.frame(t1@data$CODE,t1@data$orgtype),length)
number[is.na(number)]=0
dimnames(number)[2][[1]]<-c("y0","y1","y2") 
number<-data.frame(number)
number$code=as.numeric(row.names(number))

#画地图 #
plot(shanghai, border = "grey60",lwd=.5)
plot(t1, add = TRUE, pch = 19, cex = 0.1,alpha=.5,col="tomato1")
plot(shanghai.t,add=T, border="royalblue")
title(main="2010年上海社会组织分布",font.main= 4, col.main= "blue")

kk<-table(t0$fyear,t0$orgtype)
kk<-data.frame(kk)
names(kk)<-c("year","type","freq")
kk<-kk[as.numeric(as.character(kk$year))>=1990,]
kk<-subset(kk,kk$type=="NGO")

library(ggplot2)
mytheme<-theme_bw()+
  theme(panel.border=element_blank(),
        axis.line=element_line(colour="black"))

ggplot(kk,aes(x=year,y=freq,shape=type,group=type))+
  geom_line()+
  geom_point(size=4,colour="blue")+
  mytheme+ 
  ggtitle("Social Organization")

plot(density(number$y1), main="2010年上海民办非企业组织",xlab="", ylab="密度")

# 读取自变量数据：Excel格式 #
popu<-read.csv("/population2010.csv")
popu[,c(2:11)]<-popu[,c(2:11)]/10000

shanghai=shanghai[!is.na(shanghai$CODE),]
shanghai@data["code"]=as.numeric(as.character(shanghai@data[,"CODE"]))

shanghai@data = merge(shanghai@data,popu,by.x="code",by.y="code",all.x=TRUE,sort=F)
shanghai@data = merge(shanghai@data,number,by.x="code",by.y="code",all.x=TRUE,sort=F)

# 生成新的自变量 #
area=sapply(slot(shanghai,"polygons"),slot,"area")
shanghai@data=data.frame(shanghai@data,area=area)
shanghai@data$density=(shanghai@data$population/shanghai@data$area)/100
shanghai@data$male.r=shanghai@data$male/shanghai@data$female
shanghai@data$hh.r=shanghai@data$hhnum/shanghai@data$population
shanghai@data$popu1.r=shanghai@data$popu1/shanghai@data$population
shanghai@data$popu3.r=shanghai@data$popu3/shanghai@data$population
shanghai@data$migrant.r=(shanghai@data$population-shanghai@data$local)/shanghai@data$local

shanghai@data$rural=substr(as.character(shanghai@data$CODE),7,7)
shanghai@data$rural[shanghai@data$rural=="5"]="0"
shanghai@data$rural[shanghai@data$rural=="2"]="1"

shanghai@data$center<-0
shanghai@data$center[round(shanghai@data$code/1000000)<310112]<-1

# 生成空间邻近矩阵数据和空间权重矩阵数据 #
library(spdep)
shanghai_nb <- poly2nb(shanghai)
shanghai_wt <- nb2listw(shanghai_nb, zero.policy=T)

plot(shanghai, border = "grey60")
plot(shanghai_nb, coordinates(shanghai), add = TRUE, pch = 19, cex = 0.3);title(main="上海")

# 空间相关性检验：Global Moran's I #
moran.test(shanghai$y1,listw=shanghai_wt,zero.policy=T)
moran.plot(shanghai$y1,listw=shanghai_wt,labels=F,zero.policy=T,main=paste("Moran I 检验: P=",round(moran.test(shanghai$y1,listw=shanghai_wt,zero.policy=T)$p.value,4),sep=""),xlab="民办非企业组织数目",ylab="民办非企业组织数目的空间滞后")

# 空间回归模型 # 
y1.lm<-lm(y1~population+popu1+popu3+local+area+rural+center,data=shanghai)
y1.lag2<-stsls(y1~population+popu1+popu3+local+area+rural+center,data=shanghai,listw=shanghai_wt,zero.policy=T)

#######################################
##扩展内容：中国地图
library(maptools)
# 全国地图要素的下载地址
# http://219.238.166.215/mcp/index.asp
quanguo<- readShapeSpatial("/Users/liding/DATA/map/Chinacountiesstata/counties_china.shp")
plot(quanguo, border = "grey60",lwd=.5)

#怎么解析地址的经纬度
#利用百度地图API
# http://lbsyun.baidu.com/index.php?title=webapi
# 高德地图
# http://lbs.amap.com/api/webservice/summary/

