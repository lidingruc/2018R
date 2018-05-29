#制作带有南海诸岛的中国地图的方法
#李丁，中国人民大学社会与人口学院
#两种思路：
#第一种：制作两张图片（南海诸岛可以直接用国家图），然后将两张图片拼接在一起，使用ggplot2作图
#第二种：类似于argis一样，直接在地图上再做一张小地图
#使用基础的plot作图

#不管是用ggplot2还是plot函数，最好不直接作图于绘图窗口
#太慢

#加载一些必要的包
#地图处理和制作
library(raster)
library(rgdal)
library(tidyverse)
library(mapproj)
library(jpeg)
library(grid)
library(maptools)
library(sp)
#读入外部数据
library(readxl)
# 拼接图片
library(jpeg)
library(png)
library(grid)
library(magick)

# 数据分组 取色
library(RColorBrewer)
library(classInt) 

#读入分县市地图，省一级的地图比较容易做
setwd("/Users/liding/E/Bdata/Course/7spa/map/chinaadm/")
mland<-rgdal::readOGR("CHN/CHN_adm2.shp")
#mland <- raster::aggregate(mland,by="ID_2")
tw<-rgdal::readOGR("TWN/TWN_adm0.shp")
tw <- raster::aggregate(tw)
mc<-rgdal::readOGR("MAC/MAC_adm0.shp")
mc <- raster::aggregate(mc)
hk<-rgdal::readOGR("HKG/HKG_adm0.shp")
hk <- raster::aggregate(hk)

#分省及全国地图
guo<-rgdal::readOGR("BASIC/国家.shp")
lguo<-rgdal::readOGR("BASIC/中国线.shp")
l9<-rgdal::readOGR("BASIC/九段线.shp")
sheng<-rgdal::readOGR("BASIC/中国政区.shp")
nh<-rgdal::readOGR("BASIC/南海诸岛及其它岛屿.shp")
sh<-rgdal::readOGR("BASIC/省会城市.shp")

#合并地理单元数量
china_map<- bind(mland, tw, mc,hk)

#可以写出合并后的结果，中文字符编码问题，以后解决
#如果解决好，以后不用读入多个地图
#writeOGR(china_map, "BASIC/","counties", driver="ESRI Shapefile",encoding="UTF-8",overwrite_layer=TRUE)
#china_map2<-rgdal::readOGR("BASIC/counties.shp")

# 现成的南海地图
nhimg <- image_read("BASIC/南海诸岛.png")
#保留想保留下来的ID信息
#china_map <- raster::aggregate(china_map,by=c("ID_2","NL_NAME_2"))

#读入外部统计数据  
mydata <- read_excel("~/Downloads/市级平均数据.xlsx", 
                     sheet = "Average")
names(mydata) <- c("prov","NL_NAME_2","ename","fic","fii","ti","ainc","cap","dci")
# 地图统计单元数据中ID_2,NL_NAME_2
x <- china_map@data
# ID加上行id,这个行id-与地理元素单元list的顺序一致
xs <- data.frame(id=row.names(x),x)
#香港、澳门、台湾没有ID_2,"NL_NAME_2"的信息还需要完善（留到以后）
xs[,"ID_2"] <- xs[,"id"]


# 清理地理统计单元ID变量中不合适的数据
xs$NL_NAME_2 <- as.character(xs$NL_NAME_2)
xs$NL_NAME_2[xs$NL_NAME_2 =="北京|北京"] <-"北京市"
xs$NL_NAME_2[xs$NL_NAME_2 =="重慶|重庆"] <-"重庆市"
xs$NL_NAME_2[xs$NL_NAME_2 =="常德市|常德市"] <-"常德市"
xs$NL_NAME_2[xs$NL_NAME_2 =="长沙市|長沙市"] <-"长沙市"
xs$NL_NAME_2[xs$NL_NAME_2 =="郴州市|郴州市"] <-"郴州市"
xs$NL_NAME_2[xs$NL_NAME_2 =="衡阳市|衡陽市"] <-"衡阳市"
xs$NL_NAME_2[xs$NL_NAME_2 =="怀化市|懷化市"] <-"怀化市"
xs$NL_NAME_2[xs$NL_NAME_2 =="娄底市|婁底市"] <-"娄底市"
xs$NL_NAME_2[xs$NL_NAME_2 =="邵阳市|邵陽市"] <-"邵阳市"
xs$NL_NAME_2[xs$NL_NAME_2 =="湘潭市|湘潭市"] <-"湘潭市"
xs$NL_NAME_2[xs$NL_NAME_2 =="湘西土家族苗族自治州|湘西土家族苗族自治州"] <-"湘西土家族苗族自治州"     
xs$NL_NAME_2[xs$NL_NAME_2 =="益阳市|益陽市"] <-"益阳市"      
xs$NL_NAME_2[xs$NL_NAME_2 =="永州市|永州市"] <-"永州市"
xs$NL_NAME_2[xs$NL_NAME_2 =="岳阳市|岳陽市"] <-"岳阳市"
xs$NL_NAME_2[xs$NL_NAME_2 =="张家界市|張家界市"] <-"张家界市"
xs$NL_NAME_2[xs$NL_NAME_2 =="株洲市|株洲市"] <-"株洲市"
xs$NL_NAME_2[xs$NL_NAME_2 =="滨州"] <-"滨州市"
xs$NL_NAME_2[xs$NL_NAME_2 =="上海|上海"] <-"上海市"
xs$NL_NAME_2[xs$NL_NAME_2 =="运城县"] <-"运城市"
xs$NL_NAME_2[xs$NL_NAME_2 =="天津|天津"] <-"天津市"
xs$NL_NAME_2[xs$NL_NAME_2 =="巴音郭愣蒙古自治州"] <-"巴音郭楞蒙古自治州"
xs$NL_NAME_2[345] <-"台湾"
xs$NL_NAME_2[346] <-"澳门"
xs$NL_NAME_2[345] <-"香港"

#地理统计单元数据和外部统计数据合并
# by NL_NAME_2
data<-plyr::join(xs,mydata,type="full")
#离散颜色标度分割：
qa <- quantile(na.omit(data$fic),c(0,0.2,0.4,0.6,0.8,1.0))
data$fic_q<-cut(data$fic,qa,labels= c("0-20%","20-40%","40-60%","60-80%","80-100%"),include.lowest = TRUE)
table(data$fic_q)

# 地图元素单元数据，有list 顺序信息，即前面的id信息
map_data <- fortify(china_map)
# by id，将地理统计单元的ID信息合并到地理元素单元中
map_data <- plyr::join(map_data, data, type ="full")


# 制成地图，为拼接外部来源图片做准备
p <- ggplot()+
  geom_polygon(data=map_data,aes(long,lat,group=group,fill=fic_q),colour=NA,size=0.25) + #地区
  geom_polygon(data=sheng,aes(long,lat,group=group,fill=NA),colour="black",size=0.25) + # 省界
  scale_fill_brewer(palette="RdYlGn")+
  coord_map() +  
  theme(
    legend.position=c(0.08, 0.20),
    legend.text = element_text(size=rel(2)),
    legend.title = element_text(size=rel(2)),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background=element_rect(I(0),linetype=0),
    plot.title = element_text(family="STKaiti",size=32,
                hjust=0.5,  colour = "red",face = "bold")
  ) + labs(title = "中国各县财政收入")

#  theme_nothing() 
# 画底图(大陆),保存 比较慢,8分钟
# ggsave(p,filename = "中国地图.png",
#                width = 5, height = 4, dpi = 300)
#  
#最好将地图输出到文件中而不是使用ploty打印在窗口——很慢
plot2 <- function(theplot, name, ...) {
  name <- paste0(name, ".png")
  png(filename=name,width = 960, height = 960, units = "px", pointsize = 12)
  print(theplot)
  dev.off()
} #plotting function
plot2(p, name="地图")

##############
# 方案1、图片拼接的方式
#如果是其他格式的南海诸岛，转为png格式先
#library("animation")
#gm.convert("南海诸岛.jpg", output = "南海诸岛.png")
#读入刚刚做好的地图
cmap <-image_read("地图.png") 
# 合并地图
out <- image_composite(cmap, image_scale(nhimg, "x200"), offset = "+800+600")
# Show preview
image_browse(out)
# Write to file
image_write(out, "中国+南海.png")

## 方案2，在p上直接加图，不用输出地图再读入
# install.packages("cowplot", dependencies = TRUE)
library(cowplot)
h <- ggdraw(p)  ## 有点慢 需要将上面的P重做一遍
ger <- grid::rasterGrob(nhimg, interpolate=TRUE)
#draw_grob(grob, x = 0, y = 0, width = 1, height = 1, scale = 1）
#合并和输出
out1 <- h + draw_grob(ger, 0.80, 0.12, 0.15, 0.3,1)
plot2(out1, "中国+南海1")

# 在直角坐标系中，可以用annotation_custom直接加图
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + geom_point()+  theme_bw()+
  annotation_custom(ger, xmin=6, xmax=Inf, ymin=-Inf, ymax=20)+
  scale_colour_manual(values=c("#CC6600", "#999999", "#FFCC33", "#000000","#CC5500", "#988999", "#FFCC11"))


#方案3 带南海地图的中国地图，速度最快
#http://blog.sciencenet.cn/blog-39344-847675.html
#先安装包（sp, raster 和 rgdal）
#LINUX / UNIX 还要安装依赖库，如gdal库和proj.4。 
#library(sp) 
#library(raster)#投影时要用到这个包
#library(rgdal)#读取矢量边界要用到此包 
#library(RColorBrewer)
#library(classInt) 

#需要将前面合并地图数据部分的操作移过来 
#读入地图、处理数据、保证数据id与地图id一直
# 直接从data开始作图
plotvar <- data$fic
nclr <- 5
plotclr <- brewer.pal(nclr,"RdYlGn")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
#


# 大陆、香港、澳门、台湾合并而成的地图
projection(china_map)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
#投影赋值，是WGS84投影 
ext<-extent(china_map) 
#算出来能框住中国的最小矩形。
#经纬度范围分别是：x最小值,x最大值，y最小值，y最大值。
#这四个值可用ext[1],ext[2],ext[3] 和 ext[4]提取

my.op<-par()    #记住做图参数的默认值。 
png(filename="中国地图+南海p.png",width = 960, height = 960, units = "px", pointsize = 12)

palette(plotclr)
plot(china_map,col=data$fic_q,border=NA,    #刚才读入的地图
     xlim=c(ext[1],ext[2]),    #定义大图中要显示的经度（x轴）范围，即x最小值和最大值 
     ylim=c(ext[3]+8,ext[4])    #定义要显示的纬度范围，即y的最大、最小值。
     #里面13这个参数可根据自己的审美观调整 
     #现在（ext[3]+13）的位置应该是海南岛稍微往下一点 
)
plot(sheng, col=NA,border="black", add=TRUE) # 省界
#图例
legend("bottomleft", c('647-1022', '1022-1294', '1294-1656', '1656-2266',"2266-27466"), fill=plotclr, bty='n',cex = 2)

par(fig=c(.79,.99,.04,.30),    #定义小图的位置和大小。
    #注意其中的四个坐标用的是（0,1）区间的相对值。         
    new=TRUE,    #这一句，表示在前图上再加个图，而不是把前图抹去。
    mar=c(0,0,0,0)    # margin的不要，统统去掉 
) 
plot(sheng,    #再画中国地图，这次是显示在小图里面的。 
     xlim=c(105,125),    #定义小图里要显示的经度范围。其中的+24,-13都可修改 
     ylim=c(3,24)   #定义小图中要显示的纬度范围。-30是可修改的参数 
) 
plot(l9,  add=TRUE,  #再画中国地图，这次是显示在小图里面的。 
     xlim=c(105,125),    #定义小图里要显示的经度范围。其中的+24,-13都可修改 
     ylim=c(3,24)   #定义小图中要显示的纬度范围。-30是可修改的参数 
)   
plot(nh,   add=TRUE,  #显示在小图里面的。 
     xlim=c(105,125),    #定义小图里要显示的经度范围。其中的+24,-13都可修改 
     ylim=c(3,24)   #定义小图中要显示的纬度范围。-30是可修改的参数 
) 
box(lty="1373",col="black") 
#给小图加个框。”1373“代表框的类型，”red“是指框的颜色。
#这俩参数可根据个人喜好修改。 

dev.off()

par(my.op)    #本句与主题无关。让做图参数退回默认值。 

#-----------程序结束 


###
#如果是省一级的数据
sid <- sheng@data[c("ADCODE99","NAME")]
sid<- data.frame(id=row.names(sid),sid)
shengdata <- read.csv("/Users/liding/E/Bdata/liding17/2018R/data/shengdata.csv",sep = ",",encoding="utf-8")
shengdata<-plyr::join(sid,shengdata,by="NAME",type="full")
smap_data <- fortify(sheng)
smap_data <- plyr::join(smap_data, shengdata, type ="full")

ggplot(data = smap_data, aes(x = long, y = lat,  group = id, fill = kidgarden)) + 
  geom_polygon(colour = 'black') + 
  scale_fill_gradient(low = 'green', high = 'blue') + 
  labs(title ="Numbers of kidgardens in China") +  
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid = element_blank()) +
  coord_map() 


###

###################
# 经纬度tab文件转shp文件，下一步解决
# 包括读入arcgis的 mdb数据库然后进行转换
# 如果能够解决就可以将中国大陆、港澳台的数据合并在一起
# 实际上上面可以合并在一起了
# https://stackoverflow.com/questions/23784427/convert-table-of-coordinate-to-shape-file-using-r
filePath="/Users/liding/DATA/中国地图/CNborder/nine-dash-line.dat"
UTMcoor=read.csv(file=filePath,sep = " ")
coordinates(UTMcoor)=~X+Y
proj4string(UTMcoor)=CRS("++proj=utm +zone=48") # set it to UTM
UTMcoor.df <- SpatialPointsDataFrame(UTMcoor, data.frame(id=1:length(UTMcoor)))
LLcoor<-spTransform(UTMcoor.df,CRS("+proj=longlat"))
LLcoor.df=SpatialPointsDataFrame(LLcoor, data.frame(id=1:length(LLcoor)))
writeOGR(LLcoor.df, dsn="/Users/liding/DATA/中国地图/CNborder/" ,layer="t1",driver="ESRI Shapefile")
writeSpatialShape(LLcoor.df, "t2")


#********
# sf 包处理地图可能命令更简洁，介绍
library(sf) 
library(tidyverse) 
library(viridis) 
library(rvest)
library(ggplot2)

nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
# limit to first 2 counties
nc <- nc[1:2,]
# convert to SpatialPolygonsDataFrame
nc_sp <- as(nc, "Spatial")

class(nc)

glimpse(nc)

as_tibble(nc)

class(nc_sp)

str(nc_sp)

(nc_geom <- st_geometry(nc))

st_geometry(nc) %>% class()

attributes(nc_geom)

nc_geom[[1]] %>% class

setwd("/Users/liding/E/Bdata/Course/7spa/map/chinaadm/CHN/")
nc <- st_read("CHN_adm2.shp",quiet = TRUE)

library(readxl)
cdata <- read_excel("~/Downloads/市级平均数据.xlsx", 
                    sheet = "Average")
names(cdata) <- c("prov","city","ename","fic","fii","ti","ainc","cap","dci")

nc %>% 
  transmute(city = as.character(NL_NAME_2)) %>% 
  inner_join(cdata, by = "city") %>%
  ggplot() +
  geom_sf(aes(fill = fic)) +
  ggtitle("County-level fisical income in China") +
  theme_bw()



# 主题图 tmap
library(rgdal)
library(tmap)
qtm(US)


### 合并两张图片的例子
#https://stackoverflow.com/questions/9917049/inserting-an-image-to-ggplot2
library(ggplot2)
library(magick)
library(here) # For making the script run without a wd
library(magrittr) # For piping the logo

# Make a simple plot and save it
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point() + 
  ggtitle("Cars") +
  ggsave(filename ="Cars.png",
         width = 5, height = 4, dpi = 300)
## Call back the plot
plot <- image_read("Cars.png")
# And bring in a logo
logo_raw <- image_read("http://hexb.in/hexagons/ggplot2.png") 

# Scale down the logo and give it a border and annotation
# This is the cool part because you can do a lot to the image/logo before adding it
logo <- logo_raw %>%
  image_scale("100") %>% 
  image_background("grey", flatten = TRUE) %>%
  image_border("grey", "600x10") %>%
  image_annotate("Powered By R", color = "white", size = 30, 
                 location = "+10+50", gravity = "northeast")

# 拼接 Stack them on top of each other
final_plot <- image_append(image_scale(c(plot, logo), "500"), stack = TRUE)
# And overwrite the plot without a logo
image_write(final_plot, "Cars2.png")

# 叠加一个小logo
out <- image_composite(plot, image_scale(logo_raw, "x100"), offset = "+1300+1000")
image_browse(out)

##遥感卫片处理
#https://stackoverflow.com/questions/28907825/r-crop-geotiff-raster-using-packages-rgdal-and-raster
#https://gis.stackexchange.com/questions/131409/how-to-read-and-write-tiff-image-and-locate-pixels-and-bands-of-images-in-r
## install.packages("rgdal")
## install.packages("raster")
library("rgdal")
library("raster")

# 查看信息
InFile = "/Users/liding/Downloads/urban2000/URBAN_2000_0.tif"
dataType(raster(InFile))
GDALinfo(InFile)

# 选取部分写出  
library(raster)    # For extent(), xmin(), ymax(), et al.
library(gdalUtils) # For gdal_translate()

# 输出文件
outFile <- "subset.tif"
# 选择范围
ex <- extent(c(-75, -70, -55, -50))
# 写出
gdal_translate(InFile, outFile,
               projwin=c(xmin(ex), ymax(ex), xmax(ex), ymin(ex)))


 readTIFF


# 另一种做法
## Read in as three separate layers (red, green, blue)
s <- stack(InFile)
nlayers(s)
plot(s)
## Crop the RasterStack to the desired extent
ex  <- raster(xmn=-75, xmx=-70, ymn=-55, ymx=-50)
projection(ex) <- proj4string(s)
s2 <- crop(s, ex)

## Write it out as a GTiff, using Bytes
writeRaster(s2, outFile, format="GTiff", datatype='INT1U', overwrite=TRUE)




tobecroped <- raster("/Users/liding/Downloads/urban2000/URBAN_2000_0.tif")
extent(tobecroped)
ex  <- raster(xmn=-75, xmx=-70, ymn=-55, ymx=-50)

projection(ex) <- proj4string(tobecroped)
output <- "/Users/liding/Downloads/output.tif"
crop(x = tobecroped, y = ex, filename = output)

plot(tobecroped)
# rasterImage

# 一些基本概念
#raster  栅格图片
#tile 瓦片
#brand（波段）
#layer（图层）
#ROI（感兴趣的区域）
#GeoTIFF metadata
# 层次数据模型（HDF）或其对地观测扩展（HDF-EOS）
#dem就是digital elevation model，这里的tiff文件就是带有高程信息

# HDF 文件
#https://shekeine.github.io/modis/2014/08/25/MODIS_HDF_reprojection_subsetting_R

# HDF-EOS to multi-band GeoTIFF in R  TIF 文件
# https://shekeine.github.io/modis/2014/08/27/HDF_to_multi-band_geotiff

# False colour composite (FCC) 
#https://shekeine.github.io/visualization/2014/09/27/sfcc_rgb_in_R

# 帮助理解栅格数据
#Basic raster functions example:
rm(list = ls())

# create three identical RasterLayer objects
r1<- r2 <- r3 <- raster(nrow=10, ncol=10)

# Assign random cell values
n <- ncell(r1)
values(r1) <- runif(n)
values(r2) <- c(rep(3, n/2), rep(5, n/2))
values(r3) <- runif(n)
values(r1) <- values(r3)<- runif(n)        # same as previous line

# combine three RasterLayer objects into a RasterStack
s <- stack(r1, r2, r3)
s
plot(s)

# now change extent
# also see setextent() for more flexibility
e <- extent(s)
e
e <- extent(0,5,50,60)
extent(s) <- e
plot(s)

# simple raster math
r1 <- r1+10
rsqrt <- sqrt(r1)
extent(r1)
extent(rsqrt)

s2 <- stack(r1,rsqrt)
plot(s2)

# extract a layer
lyr <- raster(s2, layer=2)
plot(lyr)

# summary stats – “Summary methods” in documentation:  mean,
# median, min max, range, prod, sum, any, all.  These always
# return a raster layer.

mean(s2)

#  cellStats returns single number for each layer.

cellStats(s2,"mean") 


