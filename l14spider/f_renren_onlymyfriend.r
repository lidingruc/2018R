

# 只看我的好友

f_renren_onlymyfriend <- function(name="******", pwd="******"){

library(RCurl)
library(rjson)
memory.limit(4000)

myH <- c(
"User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12",
"Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
"Accept-Language"="zh-cn,zh;q=0.5",
#"Accept-Encoding"="gzip,deflate",
"Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7",
"Keep-Alive"="115",
"Connection"="keep-alive",
#"Host"="status.renren.com",
"Content-Type"="application/x-www-form-urlencoded; charset=UTF-8",
#"Content-Length"=as.character(nchar(xx)*2+1),
"Pragma"="no-cache",
#"Referer"="http://status.renren.com/ajaxproxy.htm",
"Cache-Control"="no-cache"
)
d <- debugGatherer()
cH <- getCurlHandle(debugfunction=d$update, verbose=T, ssl.verifyhost=F, 
ssl.verifypeer=F, followlocation=T, cookiefile="cc.txt")
pinfo <- c(
"email"=name,
"password"=pwd,
"origURL"="http://www.renren.com/Home.do",
"domain"="renren.com"
)
pinfo <- iconv(pinfo, from="GB18030", to="UTF-8")
ttt <- postForm("http://passport.renren.com/PLogin.do", httpheader=myH,
.params=pinfo, curl=cH, style="post")
getCurlInfo(cH)[["cookielist"]]

h <- getURL("http://www.renren.com", curl=cH, .encoding="gbk")
write(h, "temp.txt")
hh <- readLines("temp.txt", encoding="UTF-8")
file.remove("temp.txt")
rm(h)
hh <- hh[grep("user : ", hh)]
hh <- gsub("[^0-9]", "", hh)
uid <- hh
# uid
# "41021031"

thisurl <- paste("http://friend.renren.com/GetFriendList.do?curpage=0&id=", uid, sep="")
h <- getURL(thisurl, curl=cH, .encoding="gbk")
write(h, "temp.txt")
hh <- readLines("temp.txt", encoding="UTF-8")
file.remove("temp.txt")
rm(h)

friend <- data.frame(u0=NULL, id0=NULL, u1=NULL, id1=NULL)

# 多少页
imax <- 0
if(length(grep("最后页", hh)) > 0){
imax <- strsplit(hh[grep("最后页", hh)[1]], "curpage=")[[1]][2]
imax <- strsplit(imax, "&amp")[[1]][1]
}
if(length(grep("最后页", hh)) == 0 & length(grep("下一页", hh)) > 0){
nextpage <- grep("下一页", hh)[1]
hh_pages <- hh[(nextpage-30) : (nextpage - 1)]
lastpage <- hh_pages[max(grep("curpage=", hh_pages))]
imax <- strsplit(lastpage, "curpage=")[[1]][2]
imax <- strsplit(imax, "&amp")[[1]][1]
}
imax <- as.numeric(imax)

# 我的名字
u0 <- strsplit(hh[grep("的好友</title>", hh)], "的好友</title>")[[1]][1]
u0 <- strsplit(u0, "人人网 - ")[[1]][2]

# 搜索每个好友的名字。每个好友都可以即时聊天。忘记为什么了，有的只有‘打个招呼’
ii <- grep("即时聊天", hh)
if(length(ii) == 0){
hh <- hh[grep("打个招呼", hh)[1]]
ff <- strsplit(hh, "event,'")[[1]][2]
ff <- strsplit(ff, "'[)]")[[1]][1]
ff1 <- strsplit(ff, "','")[[1]][1]
ff2 <- strsplit(ff, "','")[[1]][2]
friendnew <- data.frame(u0=0, id0=0, u1=ff2, id1=ff1)
friend <- rbind(friend, friendnew)
}
if(length(ii) >= 1){
hh <- hh[ii]
for(iii in 1:length(ii)){
ff <- strsplit(hh[iii], ":talkto[(]")[[1]][2]
ff <- strsplit(ff, "'[)];return false")[[1]][1]
ff1 <- strsplit(ff, ", '")[[1]][1]
ff2 <- strsplit(ff, ", '")[[1]][2]
friendnew <- data.frame(u0=0, id0=0, u1=ff2, id1=ff1)
friend <- rbind(friend, friendnew)
}
}

# 页面循环
if(imax >= 1){
for(pagei in 1:imax){
thisurl <- paste("http://friend.renren.com/GetFriendList.do?curpage=", pagei, "&id=", uid, sep="")
h <- getURL(thisurl, curl=cH, .encoding="gbk")
write(h, "temp.txt")
hh <- readLines("temp.txt", encoding="UTF-8")
file.remove("temp.txt")
rm(h)

# 搜索每个好友的名字。每个好友都可以即时聊天。忘记为什么了，有的只有‘打个招呼’
ii <- grep("即时聊天", hh)
if(length(ii) == 0){
hh <- hh[grep("打个招呼", hh)[1]]
ff <- strsplit(hh, "event,'")[[1]][2]
ff <- strsplit(ff, "'[)]")[[1]][1]
ff1 <- strsplit(ff, "','")[[1]][1]
ff2 <- strsplit(ff, "','")[[1]][2]
friendnew <- data.frame(u0=0, id0=0, u1=ff2, id1=ff1)
friend <- rbind(friend, friendnew)
}
if(length(ii) >= 1){
hh <- hh[ii]
for(iii in 1:length(ii)){
ff <- strsplit(hh[iii], ":talkto[(]")[[1]][2]
ff <- strsplit(ff, "'[)];return false")[[1]][1]
ff1 <- strsplit(ff, ", '")[[1]][1]
ff2 <- strsplit(ff, ", '")[[1]][2]
friendnew <- data.frame(u0=0, id0=0, u1=ff2, id1=ff1)
friend <- rbind(friend, friendnew)
}
}
}
}

# 我的名字和ID
friend$u0 <- u0
friend$id0 <- uid

# 如果我没有好友 ="=
if(nrow(friend) <= 0){
frienddata <- friend
}
# 如果我有好友
if(nrow(friend) > 0){
friend <- unique(friend)
rownames(friend) <- 1:nrow(friend)

frienddata <- friend
for(i in 1:4){
frienddata[, i] <- as.character(frienddata[, i])
}

# 循环查找共同好友
for(num in 1:nrow(friend)){
userid <- friend$id1[num]
username <- friend$u1[num]
if(!(userid %in% frienddata$id0)){
friendnewurl <- paste("http://friend.renren.com/shareFriends?p={%22init%22:true,%22uid%22:true,%22uhead%22:true,%22uname%22:true,%22group%22:true,%22net%22:true,%22param%22:{%22guest%22:", userid, "}}", sep="")
h <- getURL(friendnewurl, curl=cH, .encoding="gbk")
h2 <- fromJSON(h)$candidate
if(length(h2) > 0){
newid <- sapply(1:length(h2), function(i){h2[[i]]$id})
friendnew <- data.frame(u0=username, id0=userid, u1=0, id1=newid)
frienddata <- unique(rbind(frienddata, friendnew))
print(c(num, as.character(username), nrow(friendnew), date()))
# gc()
}
if(length(h2) <= 0){
print(paste('no shareFriends with ', username, sep=''))
}
}
}

# 备份好友数据
friend <- frienddata

frienddata[, 2] <- as.character(frienddata[, 2])
frienddata[, 4] <- as.character(frienddata[, 4])

# 我的好友
myfrienddata <- frienddata[frienddata[, 2] == uid, ]

# 排除注销的账号及没有共同好友的情况
frienddata_gg <- frienddata[frienddata[, 4] %in% myfrienddata[, 4] & frienddata[, 2] != uid, c(2, 4)]
# myfrienddata <- myfrienddata[myfrienddata[, 4] %in% c(frienddata_gg[, 1], frienddata_gg[, 2]), ]

library(igraph)

people <- unique(data.frame(id=myfrienddata[, 4], name=myfrienddata[, 3]))
gg <- graph.data.frame(d=frienddata_gg, directed=F, vertices=people)
is.simple(gg)
gg <- simplify(gg)
is.simple(gg)

dg <- degree(gg)
gg <- subgraph(gg, which(dg > 0) - 1)

# 子群划分
com <- walktrap.community(gg, steps=5)
subgroup <- split(com$labels, com$membership)
V(gg)$sg <- com$membership + 1
V(gg)$color <- rainbow(max(V(gg)$sg))[V(gg)$sg]
# 中间度
V(gg)$bte <- betweenness(gg, directed=F)
top <- quantile(V(gg)$bte,0.99)
V(gg)$size <- 5
V(gg)[bte>=top]$size <- 15
V(gg)$label <- NA
V(gg)[bte>=top]$label <- V(gg)[bte>=top]$name
V(gg)$cex <- 1
V(gg)[bte>=top]$cex <- 2

png("renren_friend_community_betweenness.png",width=900,height=900)
par(mar=c(0,0,0,0))
set.seed(14)
plot(gg,
layout=layout.fruchterman.reingold,
vertex.size=V(gg)$size, vertex.color=V(gg)$color,
vertex.label=V(gg)$label, vertex.label.cex=V(gg)$cex,
edge.color=grey(0.8)
)
dev.off()
print(V(gg)[bte>=top]$name)
return(list(friend=friend, subgroup=subgroup))
}
}





