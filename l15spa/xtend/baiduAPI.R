# http://api.map.baidu.com/geocoder/v2/?ak=E4805d16520de693a3fe707cdc962045&callback=renderOption&output=json&address=百度大厦&city=北京市
# http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps


library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address) {
  root <- "http://api.map.baidu.com/geocoder/v2/?ak=0QxKBNPPD2BrnnRkNtkoG3XI&callback=renderOption&output=json&address="
  u <- paste(root,address)
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  require("plyr")
  if(verbose) cat(address,"\n")
  u <- aaply(address,1,construct.geocode.url)
  doc <- aaply(u,1,getURL)
  doc<-gsub("renderOption&&renderOption\\(", "", doc)
  doc<-gsub("\\)", "", doc)

  json <- alply(doc,1,fromJSON,simplify = FALSE)
  coord = laply(json,function(x) {
    if(x$status=="0") {
      lat <- x$result$location$lat
      lng <- x$result$location$lng
      return(c(lat, lng))
    } else {
      return(c(NA,NA))
    }
  })
  if(length(address)>1) colnames(coord)=c("lat","lng")
  else names(coord)=c("lat","lng")
  return(data.frame(address,coord))
}
# 
gGeoCode(c("百度大厦&city=北京","百度大厦&city=北京"))

# ak 4L6UzXBVZxG1jWopGBxRdq5kdW3L36Du
# ak F61715ded469c6f72e6ac945b5d97f53
# ak pA2N9r3tH7RFaElIfst6CzlY
# ak Gm78kDIrmzcLX7aI2MQjEYRT
# ak p4NXS0SEGdz5u0qGMwu7j5rY
# ak 0QxKBNPPD2BrnnRkNtkoG3XI
# ak 38dbc1cdbb8170d3f08a4db190de55d2
# ak Lppm8s01iNb6ZD852nzkfMd0
# ak qbCxFEtYfMrCk1PF1RA4hhbb
# ak 67d28f499072d22dba8a978e2431794f
# ak 47ec1faf88785f97cb6b9f95bd433fa9
# ak C93b5178d7a8ebdb830b9b557abce78b
# ak 9c9978a893e2dd6c3be3d3241f8e8f61
# ak WGcKO4YO9ZqyV9EtTtLrOwSd
# ak bjj7HFkUUoHs07rnVNM07TQg
# ak C93b5178d7a8ebdb830b9b557abce78b
# ak 7ydgj9xW7bVXGF1ZLW9DAcEW


address<-read.csv("D:/DATA/宗教/宗教场所登记信息/宗教场所信息.csv",header=TRUE)

ptm <- proc.time()
construct.geocode.url <- function(address) {
  root <- "http://api.map.baidu.com/geocoder/v2/?ak=bjj7HFkUUoHs07rnVNM07TQg&callback=renderOption&output=json&address="
  u <- paste(root,address)
  return(URLencode(u))
}

address1<-as.character(address$add[1:1000])
addgeo1<-gGeoCode(address1)
proc.time() - ptm

addgeo<-rbind(addgeo,addgeo11)
address<-cbind(address,addgeo)


# http://stackoverflow.com/questions/12835942/fast-replacing-values-in-dataframe-in-r

addgeo$address<-as.character(addgeo$address)

addgeo[5086,1]<-"辽宁省广宁乡观音洞村"
addgeo[7932,1]<-	"重庆市永川区来苏镇柏树桥村圆圆山村民小组"
addgeo[29783,1]<-	"河北邢台市任县辛店镇白家村"
addgeo[38366,1]<-	"日喀则拉孜县曲下县"

addgeo$lat[5086]<- 41.592383985675
addgeo$lng[5086]<- 121.79535955088

addgeo$lat[7932]<- 29.27668433
addgeo$lat[29783]<- 37.165702322708
addgeo$lat[38366]<- 29.09431047
  
addgeo$lng[7932]<- 105.7940624
addgeo$lng[29783]<- 114.91493450329
addgeo$lng[38366]<- 87.64424506

setwd('D:/DATA/宗教/宗教场所登记信息/')
write.table(address, file = "address.csv", sep = ",", col.names = NA,
            qmethod = "double")

address<-read.csv("D:/DATA/宗教/宗教场所登记信息/address.csv",header=TRUE)

library(ggplot2)
library(ggmap)

# getting the map
mapgilbert <- get_map(location = c(lon = 108.739495, lat = 34.6008), zoom = 4,
                      maptype = "roadmap", color = "bw",scale = 2)

# plotting the map with some points on it
# 佛道散点
ggmap(mapgilbert) +
  geom_point(data = address, aes(x = lng, y = lat, colour = zongjiao, alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  scale_fill_manual(values=c("red", "blue"))
dev.copy2pdf(file = "fodao.pdf", height=10, width=10)

# 门派散点
ggmap(mapgilbert) +
  geom_point(data = address, aes(x = lng, y = lat, colour = paixi, alpha = 0.6), size = 0.5, shape = 20) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  scale_fill_manual(values=c("red", "blue"))
dev.copy2pdf(file = "menpai.pdf", height=10, width=10)

# 佛教
ggmap(mapgilbert) +
  geom_point(data = subset(address,paixi=="巴利语系"|paixi=="藏语系"|paixi=="汉语系"), aes(x = lng, y = lat, colour = paixi, alpha = 0.6), size = 0.5, shape = 20) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE,title = "派系")+
  scale_fill_manual(values=c("red", "blue")) +
  scale_color_discrete("Faction",
                       labels = c("Pali", "Sino-Tibetan","Sinitic"))
dev.copy2pdf(file = "佛教.pdf", height=10, width=10)


# 道教
ggmap(mapgilbert) +
  geom_point(data = subset(address,paixi=="正一"|paixi=="全真"), aes(x = lng, y = lat, colour = paixi, alpha = 0.6), size = 0.5, shape = 20) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  scale_fill_manual(values=c("red", "blue"))+
  scale_color_discrete("Faction",
                       labels = c("Is sending", "All pie"))

dev.copy2pdf(file = "道教.pdf", height=10, width=10)



# 区域  不成功

# library(ReadImages)
library(RgoogleMaps)
library(MASS)
library(foreign)
library(reshape)
library(plyr)
library(ggplot2)
library(xtable)
library(tikzDevice)
        
LatitudeRange<- c(10,60)
LongitudeRange<- c(80,140)
LatitudeRange


DensityList<- kde2d(address$lng, address$lat, n = 320, lims = c(LongitudeRange, LatitudeRange))
PlotData.Density<- expand.grid(lon = seq.int(LongitudeRange[1], LongitudeRange[2], length.out = 320),
                                             lat = seq.int(LatitudeRange[1],  LatitudeRange[2],  length.out = 320))
PlotData.Density$density<- melt(DensityList$z)$value

DensityColorScale<- scale_colour_gradient2(low = 'white', mid = 'darkgreen', high = 'red', midpoint = median(PlotData.Density$density))
DensityColorScale$train(PlotData.Density$density)

PlotData.Density$densityColor <- DensityColorScale$map(PlotData.Density$density)
PlotData.Density$density0to1  <- pmin(PlotData.Density$density / max(PlotData.Density$density), .9)

Plot.Big   <- ggplot()
Plot.Big   <- Plot.Big + geom_tile(aes(x = x, y = y, fill = fill), data =mapgilbert)
Plot.Big   <- Plot.Big + geom_tile(aes(x = lon, y = lat, fill = densityColor, alpha = density0to1), data = PlotData.Density)
Plot.Big   <- Plot.Big + scale_x_continuous('Longitude', limits = LongitudeRange) + scale_y_continuous('Latitude', limits = LatitudeRange)
Plot.Big   <- Plot.Big + scale_alpha(c(0, .9))
Plot.Big   <- Plot.Big + scale_fill_identity()
Plot.Big   <- Plot.Big + opts(legend.position = 'none')
Plot.Bid   <- Plot.Big + ylab('') + xlab('')
Plot.Big   <- Plot.Big + coord_equal()


# 试图从百度地图中抓取POI信息
# place POI
# http://api.map.baidu.com/place/v2/search?q=%E6%95%99%E5%A0%82&region=%E5%8C%97%E4%BA%AC&output=json&ak=C93b5178d7a8ebdb830b9b557abce78b


library(RCurl)
construct.poicode.url <- function(page_num="0") {
  root <- "http://api.map.baidu.com/place/v2/search?q=教堂&region=北京&output=json&ak=C93b5178d7a8ebdb830b9b557abce78b&page_num="
  u <- paste(root,page_num,sep = "")
  return(URLencode(u))
}

# out <- do.call(rbind, lapply(json, function(x)data.frame(as.vector(x$results)))) 
# library(httr)
# res<-GET( "http://api.map.baidu.com/place/v2/search?q=%E6%95%99%E5%A0%82&region=%E5%8C%97%E4%BA%AC&output=json&ak=C93b5178d7a8ebdb830b9b557abce78b")

library(jsonlite)
gGeopoi <- function(page_num="0",verbose=FALSE) {
  require("plyr")
  if(verbose) cat(address,"\n")
  u <- aaply(page_num,1,construct.poicode.url)
  doc <- aaply(u,1,getURL)
  json <- jsonlite::fromJSON(doc)
  if(json$status==0) poidata = json$results
  return(data.frame(region,poidata))
}
poi<-gGeopoi("0")
poi<-rbind(poi,gGeopoi("1"))



############################### 
library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="0") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
  }
}

gGeoCode("Philadelphia, PA")

# 批量,这个是可以用的

library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  require("plyr")
  if(verbose) cat(address,"\n")
  u <- aaply(address,1,construct.geocode.url)
  doc <- aaply(u,1,getURL)
  json <- alply(doc,1,fromJSON,simplify = FALSE)
  coord = laply(json,function(x) {
    if(x$status=="OK") {
      lat <- x$results[[1]]$geometry$location$lat
      lng <- x$results[[1]]$geometry$location$lng
      return(c(lat, lng))
    } else {
      return(c(NA,NA))
    }
  })
  if(length(address)>1) colnames(coord)=c("lat","lng")
  else names(coord)=c("lat","lng")
  return(data.frame(address,coord))
}
gGeoCode(c("Philadelphia, PA","New York, NY"))



# 
geocode <- function(address,reverse=FALSE)  {
  require("RJSONIO")
  baseURL <- "http://maps.google.com/maps/api/geocode/json?sensor=false&"
  
  # This is not necessary, 
  # because the parameter "address" accepts both formatted address and latlng
  
  conURL <- ifelse(reverse,paste0(baseURL,'latlng=',URLencode(address)),
                   paste0(baseURL,'address=',URLencode(address)))  
  con <- url(conURL)  
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con) 
  status <- data.json["status"]
  if(toupper(status) == "OK"){
    t(sapply(data.json$results,function(x) {
      list(address=x$formatted_address,lat=x$geometry$location[1],
           lng=x$geometry$location[2])}))
  } else { 
    warning(status)
    NULL 
  }
}

geocode("Dupont Cir NW, Washington, DC 20036, USA")

geocode("38.910262, -77.043565")



# 
library(XML)
url = 'http://maps.googleapis.com/maps/api/geocode/xml?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&sensor=true'
doc = xmlTreeParse(url, useInternal=TRUE)
lat = as.numeric(xmlValue(getNodeSet(doc, '//location/lat')[[1]]))
lng = as.numeric(xmlValue(getNodeSet(doc, '//location/lng')[[1]]))


# 
getDocNodeVal=function(doc, path){
  sapply(getNodeSet(doc, path), function(el) xmlValue(el))
}

gGeoCode=function(str){
  library(XML)
  u=paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
  doc = xmlTreeParse(u, useInternal=TRUE)
  str=gsub(' ','%20',str)
  lng=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lat")
  lat=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lng")
  c(lat,lng)
}

# ggmap中的google地址
rm(geocode)
library(ggmap)

geocode('the white house', messaging = TRUE)
geocode(c('the white house','the eiffel tower'), messaging = TRUE)

geocode('the eiffel tower', output = 'all')
# ggmap 计算距离
mapdist('CDC', 'the white house', mode = 'walking')

# 密度热力图
houston <- get_map('houston', zoom = 14) 
HoustonMap <- ggmap(houston, extent = 'device', legend = 'topleft')

HoustonMap + stat_density2d(aes(x = lon, y = lat, 
                                fill = ..level.. , alpha = ..level..),size = 2, bins = 4, 
                            data = violent_crimes, geom = 'polygon') 
  scale_fill_gradient('Violent\nCrime\nDensity') +
  scale_alpha(range = c(.4,.75), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
  
sessionInfo()

