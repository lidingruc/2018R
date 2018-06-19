#######################################
#《用R进行数据爬取：入门与示例》 
# 李丁 中国人民大学社会与人口学院
# liding@ruc.edu.cn
# 2018年1月1日检查改定
#第一部分：用httr或RCurl获取网页，phantomjs渲染网页
#第二部分：用XML、xml2、selectr、正则表达式解析网页
#第三部分：通过循环进行翻页

#######################################
#第一部分：网页获取
#（1）httr包类似于python的Requests
#rvest也是hadley创建整合了httr、xml2，能够同时使用XPath和selectr
#（2）RCurl包是curl在R中的对应，基于C的基础包。很快！
#https://r-how.com/packages/httr
# (3) 用phamtonjs渲染后再获取

##########################
#示例1.1：httr抓取网页
#########################
# 抓取小猪短租上面的信息，可以先在浏览器中打开下面的网址，看看相关页面！
setwd("/Users/liding/E/Bdata/liding17/2018R/l16spider")
library(httr)
library(xml2)
url<-"http://bj.xiaozhu.com/fangzi/5098280314.html"
html<-GET(url)  # 使用默认设定，很简单

# 查看返回对象内容
class(html)
headers(html)
cookies(html)
str(html)


# content提取返回对象中的网页内容
doc<-content(html,"raw", encoding = "utf-8") # raw十六进制码

doc<-content(html,"text")  # text 文本
cat(doc,file="xiaozhu.html") # 可以列印出去

doc<-content(html,"parsed") # parsed 解析
class(doc)
xml_find_all(doc,"//h4")[1]
xml_find_all(doc,"//div[@class='pho_info']/h4") #xml2函数提取标题4


##########################
#示例1.2：Rcurl抓取网页
#########################
library(RCurl)
########（1）默认请求到本地
html<-getURL(url)
class(html) #注意:得到的是文本

#通过XML包的htmlParse解析 
doc <- XML::htmlParse(html,encoding = "utf-8")
doc["//h4"]
XML::getNodeSet(doc, path ="//h4") # path=可以省略

#通过xml2的read_xml读入及解析
doc <- read_html(html,encoding = "utf-8")
class(doc)
xml2::xml_find_all(doc,"//h4") 

#########（2）RCurl请求的一般化形式

#设定错误信息收集函数
debugInfo <- debugGatherer()   
# 伪装浏览器报头
headers<-c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36") #设定curl句柄函数
handle <- getCurlHandle(                        debugfunction = debugInfo$update,                        followlocation = TRUE,                        cookiefile = "",                              verbose = T  )
url<- "http://bj.xiaozhu.com/fangzi/5098280314.html"

# 请求网页
response<-getURL(
  url,  #URL地址
  #局部配置参数（作用于本次请求）
  .opts=list(header=TRUE,httpheader = headers),
  curl=handle,   #curl句柄，初始化配置参数（.opts内声明的配置参数会覆盖curl中的默认参数）
  .encoding="utf-8" #编码参数
)

# 以上是getURL的一般形式。如果不需要维持回话，函数会默认构造一个curl句柄，无需特别设定。但.opts 参数是当前请求实际应用的配置参数信息，需要注意。

class(response) # getURL返回的是文本
# 两种解析，相同输入，不同输出。
doc <-xml2::read_html(response, encoding = "utf-8")
doc <- XML::htmlParse(html,encoding = "utf-8")

#查看返回信息（）
cat(debugInfo$value()[1])  #服务器地址及端口号
cat(debugInfo$value()[2])  #服务器返回的相应头信息
cat(debugInfo$value()[3])  #返回的请求头信息


##########################
#示例1.3：Rcurl登录请求
#########################
#RCurl 进行登录，设置cookies等等
# 使用RCurl包进行登录操作
library(RCurl)
library(rjson)
# 登录人人网的例子
# 设定账号和密码 
# 下面的账号来自github网站仅供学习使用
# 若为隐私信息请不要分享到github
# https://github.com/JackonYang/renren

name<-"yyttrr3242342@163.com"
pwd<-"bmeB500bmeB500"

#memory.limit(4000)  # mac版本不用设定
#伪造报头，可以通过浏览器分析请求信息得到
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
# 设置debug容器与句柄
d <- debugGatherer()
cH <- getCurlHandle(debugfunction=d$update, verbose=T, ssl.verifyhost=F, 
                    ssl.verifypeer=F, followlocation=T, cookiefile="cc.txt")
#设置登录信息+伪造初始页面
pinfo <- c(
  "email"=name,
  "password"=pwd,
  "origURL"="http://www.renren.com/SysHome.do",
  "domain"="renren.com"
)
# 登录信息编码转换
pinfo <- iconv(pinfo, from="GB18030", to="UTF-8")

# 尝试登录 post 方法
ttt <- postForm("http://www.renren.com/Login.do?rf=r", httpheader=myH,
          .params=pinfo, curl=cH, style="post")

#查看cookies
getCurlInfo(cH)[["cookielist"]]

#已经登录。获取主页找到找到自己的用户名和id
response <- getURL("http://www.renren.com", curl=cH, .encoding="gbk")
# 写出成为文本文件，利用正则表达式提取出用户ID
write(response , "temp.txt")
doc <- readLines("temp.txt", encoding="UTF-8")
# 移除临时文件
file.remove("temp.txt")
rm(response )

hh <- doc[grep("ruid", doc)]
hh <- gsub("[^0-9]", "", hh)
uid <- hh

#已经登录。获取好友列表页面。
thisurl <- paste("http://friend.renren.com/GetFriendList.do?curpage=0&id=", uid, sep="")
response <- getURL(thisurl, curl=cH, .encoding="gbk")

cat(response,file="a.txt")
# write(response , "temp.txt") # 调试时，可以写出去看源码（通常自己编写时需要分析html源码）

doc <- XML::htmlParse(response)
doc["//title"]
doc["//div[@class='info']//a"]



#########################
# 示例1.4：phantomjs渲染后，rdom包
# 案例来自杜雨 https://zhuanlan.zhihu.com/p/31268830
# 已经验证
#########################
# pantomjs无头浏览器,渲染后取格式化网页
# 需要安装phantomjs，并修改环境变量，参见
# http://note.youdao.com/noteshare?id=25579f889483fca14ffb134775b96309

library(magrittr)

###########
## RCurl默认获取完全得不到内容，需要伪造报头
library(RCurl)
URL<-"https://www.aqistudy.cn/historydata/monthdata.php?city=北京" 
response <- getURL(URL)

URL <- URL%>% xml2::url_escape(reserved ="][!$&'()*+,;=:/?@#") 

# 伪造报头之后仍然没有内容
header<-c("User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36") 

response <- getURL(URL,httpheader=header,.encoding="UTF-8")
write(response , "temp.txt")

# 也确实提取不出内容
response%>% htmlParse(encoding ="UTF-8") %>% readHTMLTable(header=TRUE)

###########
##rvest直接获取也是不行的，里面没有数据
library("rvest") 
URL<-"https://www.aqistudy.cn/historydata/monthdata.php?city=北京" 

#可以打印出来查看 
response<-GET(URL)
doc <- content(response,"text")
write(doc , "temp.txt")
# 只能找到表头
URL %>%  read_html(encoding ="UTF-8") %>% html_table(header=TRUE) %>% `[[`(1)  

###########
## rdom包
# 检查phantomjs 浏览器
stopifnot(Sys.which("phantomjs") != "")

library("rdom") # 这个包封装了phantomjs
library(XML)
library(stringi)
library(tidyverse)
# 北京市空气质量
URL <- URL%>% xml2::url_escape(reserved ="][!$&'()*+,;=:/?@#") 

tbl <- rdom(URL) %>% readHTMLTable(header=TRUE) %>% `[[`(1)
# names(tbl) <- names(tbl) %>% stri_conv(from="utf-8") # 变量名如果乱码
DT::datatable(tbl)

#########################
# 示例1.5：Rselenium+ phantomjs 
# 渲染+ 模拟翻页（复杂翻页）
# 案例来自:https://mp.weixin.qq.com/s/6sZRDA8sm_a1qu46EGlusA
# 已经验证和修改
#########################
# 1、需要安装java才能运行这种方式
# 2、下载selenium，放到某个目录即可
# 下载地址 http://selenium-release.storage.googleapis.com/index.html?path=3.3/
# 示例：https://ask.hellobi.com/blog/datamofang/10742
# 说明：https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html
# mac brew 安装测试 #http://www.cnblogs.com/richaaaard/p/5097792.html


# 在cmd line中启动，比较推荐
# 下面这种能启动但R似乎不能引用
# PATH="/Users/liding/anaconda/phantomjs/bin:$PATH" java -jar /Users/liding/anaconda/selenium-server-standalone-3.3.1.jar
# 下面这种比较好
# java -jar /Users/liding/anaconda/selenium-server-standalone-3.3.1.jar



# 在R中默认启动selenium，启动过一次就可以。但出现卡死现象，请重启系统。
# system("PATH=\"/Users/liding/anaconda/phantomjs/bin:$PATH\" java -jar \"/Users/liding/anaconda/selenium-server-standalone-3.3.1.jar\"")

library("magrittr")
library("xml2")
library("XML")
library("RSelenium") 

pJS <- phantom()
remDr <- remoteDriver(browserName = "phantomjs") 

remDr$open() #访问登录的页面
remDr$navigate("https://www.aqistudy.cn/historydata/monthdata.php?city=%E5%8C%97%E4%BA%AC") 

#使用XML包解析
mytable<-remDr$getPageSource()[[1]] %>% htmlParse(encoding ="UTF-8") %>% readHTMLTable(header=TRUE,which =1) 
#使用xml2包解析
mytable<-remDr$getPageSource()[[1]] %>% read_html(encoding ="UTF-8") %>% html_table(header=TRUE) %>% `[[`(1)

#关闭remoteDriver对象
remDr$close()

pJS$stop()

#########################
# 示例1.6：Rselenium+ phantomjs 
# 渲染后抓取动态网页 
# 案例来自 https://m.hellobi.com/post/10742
# 2018年1月1日测试可行，进行了修改
#########################
library("magrittr")
library("xml2")
library("XML")
library("RSelenium") 

# cmd 启动
# java -jar /Users/liding/anaconda/selenium-server-standalone-3.3.1.jar

#给plantomjs浏览器伪装UserAgent
eCap <- list(phantomjs.page.settings.userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:29.0) Gecko/20120101 Firefox/29.0")

###伪装浏览器UserAgent,因为plantomjs是专门用于web端页面测试的，通常都是在自己的web项目中测试web端功能，直接拿去抓别人的网站，默认的UA就是plantomjs，不太礼貌

###连接phantomjs服务
remDr <- remoteDriver(remoteServerAddr="localhost",
                      port=4444L,
                      browserName = "phantomjs", extraCapabilities = eCap)

remDr$open()

# 测试两个网页
remDr$navigate("https://www.aqistudy.cn/historydata/monthdata.php?city=%E5%8C%97%E4%BA%AC") 

remDr$navigate("https://www.lagou.com/zhaopin")

#### 正式开始
remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = eCap)

#自动化抓取函数：
myresult<-function(remDr,url){
  ###初始化一个数据框，用作后期收据收集之用！
  myresult<-data.frame() 
  ###调用后台浏览器（因为是plantomjs这种无头浏览器（headless），所以你看不到弹出窗口）
  remDr$open()
  ###打开导航页面（也就是直达要抓取的目标网址）
  remDr$navigate(url) 
  ###初始化一个计时器（用于输出并查看任务进度）
  i = 0
  while(TRUE){
    #计时器开始计数：
    i = i+1
    #返回当前页面DOM
    pagecontent<-remDr$getPageSource()[[1]]
    #以下三个字段共用一部分祖先节点，所以临时建立了一个根节点（节省冗余代码）css+xpath+正则字符截取
    con_list_item       <- pagecontent %>% read_html() %>% xml_find_all('//ul[@class="item_con_list"]/li')
    #职位名称
    position.name       <- con_list_item %>% xml_attr("data-positionname") 
    #公司名称
    position.company    <- con_list_item %>% xml_attr("data-company") 
    #职位薪资
    position.salary     <- con_list_item %>% xml_attr("data-salary") 
    #职位详情链接
    position.link       <- pagecontent %>% read_html() %>% xml_find_all('//div[@class="p_top"]/a') %>% xml_attr("href")
    #职位经验要求
    position.exprience  <- pagecontent %>% read_html() %>% xml_find_all('//div[@class="p_bot"]/div[@class="li_b_l"]') %>% xml_text(trim=TRUE) 
    #职位所述行业
    position.industry   <- pagecontent %>% read_html() %>% xml_find_all('//div[@class="industry"]') %>% xml_text(trim=TRUE) %>% gsub("[[:space:]\\u00a0]+|\\n", "",.)
    #职位福利
    position.bonus      <- pagecontent %>% read_html() %>% xml_find_all('//div[@class="list_item_bot"]/div[@class="li_b_l"]') %>% xml_text(trim=TRUE) %>% gsub("[[:space:]\\u00a0]+|\\n", "/",.)
    #职位工作环境
    position.environment<- pagecontent %>% read_html() %>% xml_find_all('//div[@class="li_b_r"]') %>% xml_text(trim=TRUE) 
    #收集数据
    mydata<- data.frame(position.name,position.company,position.salary,position.link,position.exprience,position.industry,position.bonus,position.environment,stringsAsFactors = FALSE)
    #将本次收集的数据写入之前创建的数据框
    myresult<-rbind(myresult,mydata)
    #系统休眠0.5~1.5秒
    Sys.sleep(runif(1,0.5,1.5))
    #判断页面是否到尾部
    if ( pagecontent %>% read_html() %>% xml_find_all('//div[@class="page-number"]/span[1]') %>% xml_text() !="30"){
      #如果页面未到尾部，则点击下一页，注意翻页命令
      remDr$findElement('xpath','//div[@class="pager_container"]/a[last()]')$clickElement()
      #但因当前任务进度
      cat(sprintf("第【%d】页抓取成功",i),sep = "\n")
    } else {
      #如果页面到尾部则跳出while循环
      break
    }
  }
  #跳出循环后关闭remDr服务窗口
  remDr$close() 
  #但因全局任务状态（也即任务结束）
  cat("all work is done!!!",sep = "\n")
  #返回最终数据
  return(myresult)
}

url <- "https://www.lagou.com/zhaopin"
myresult <- myresult(remDr,url)
#预览
DT::datatable(myresult)


#######################################
#第二部分：网页解析和元素提取
#可用用XML提取，
#可以用xlm2提取
#都是基于‘libxml2’ C library
#可以用selectr提取

##########################
#示例2.1：使用xml2解析、提取网页
#########################
library(httr)
library(xml2)
url<-"http://bj.xiaozhu.com/fangzi/5098280314.html"
html<-GET(url)  # 使用默认设定获得
doc <- read_html(html)

#实际上xml2包中的read_html可以直接读入并解析网页
doc <- read_html(url)

xml_find_first(doc,"//h4")

xml_find_all(doc,"//h4")

xml_find_all(doc,"//h4")[1]

xml_find_all(doc,"//h4/em")

xml_text(xml_find_all(doc,"//h4/em"))

xml_find_all(doc,"//title")
xml_name(xml_find_all(doc,"//title"))
xml_text(xml_find_all(doc,"//title"))

xml_find_all(doc,"//head")

# 同时查找两个
xml_find_all(doc, c("//h6 | //h4"))
xml_find_all(doc, "//h6 | //h4")

xml_find_all(doc, "//a[@class='lorder_name']")[[1]]

xml_children(doc)[[1]]

#xml2示例2
#https://blog.rstudio.org/2015/04/21/xml2/
library(xml2)
x <- read_xml("<foo>
              <bar>text <baz id = 'a' /></bar>
              <bar>2</bar>
              <baz id = 'b' /> 
              </foo>")

xml_name(x)
#> [1] "foo"
xml_children(x)
#> {xml_nodeset (3)}
#> [1] <bar>text <baz id="a"/></bar>
#> [2] <bar>2</bar>
#> [3] <baz id="b"/>

# Find all baz nodes anywhere in the document
baz <- xml_find_all(x, ".//baz")
baz
#> {xml_nodeset (2)}
#> [1] <baz id="a"/>
#> [2] <baz id="b"/>
xml_path(baz)
#> [1] "/foo/bar[1]/baz" "/foo/baz"
xml_attr(baz, "id")
#> [1] "a" "b"

##########################
#示例2.2：使用XML解析、提取网页
#########################
library(RCurl)
library(XML)
url<-"http://bj.xiaozhu.com/fangzi/5098280314.html"
html<-getURL(url)
doc<-htmlParse(html, encoding = "utf-8")

#简单网页可以直接解
doc<-htmlParse(url, encoding = "utf-8")
# 可以直接切片(HTMLInternalDocument)
doc["//h6"][1]

# 也可以用getNodeSet
#头
getNodeSet(doc, "//head")
#全部节点
getNodeSet(doc, "//h6")
#节点中的第一个
getNodeSet(doc, "//h6")[[1]]

# 进一步提取
nodi = getNodeSet(doc, path =c("//h6","//h4"))
nodi = getNodeSet(doc, path =c("//h6 | //h4"))
# 提取内容
xmlValue(nodi[[1]])
# 提取属性
xmlGetAttr(nodi[[3]],'class')
# 直接打印
xmlValue(getNodeSet(doc, "//title")[[1]])


#用apply函数批量提取内容
#取值
sapply(nodi, xmlValue) 
#属性
sapply(nodi, xmlAttrs) 
#父节点
sapply(nodi, xmlParent) 
#变成一个list
sapply(nodi, xmlToList) 

#内部的size
sapply(nodi, xmlSize) 
# 节点name
sapply(nodi, xmlName) 

#附录：关于htmlTreeParse和htmlParse的差异：
#http://stackoverflow.com/questions/20684507/in-r-xml-package-what-is-the-difference-between-xmlparse-and-xmltreeparse
doc<-htmlTreeParse(html, encoding = "utf-8")
xmlValue(doc$children[[1]])


##########################
#示例2.3：使用selectr提取网页
#########################

# 使用的是xml2的解析的结果 xml_document
doc<-read_html(html, encoding = "utf-8")
library(selectr)
querySelector(doc,"h4")
querySelectorAll(doc,"h4,h6")

# 使用的是XML的解析的结果 HTMLInternalDocument
doc<-htmlParse(html, encoding = "utf-8")
querySelectorAll(doc, c("h6","h4"))
querySelectorAll(doc, c("h6,h4"))

##########################
#示例2.4：使用rvest提取网页
#########################
#实际上是‘xml2’ 和 ‘httr’ 的组合,可以用管道操作 %>%
#https://zhuanlan.zhihu.com/p/22940722?refer=rdatamining
library(rvest)
#使用的其实就是xml2中的read_html进行了读取和解析
doc <- read_html(url)
# html_nodes 结合了xml和nodeset的模样 
# 命令类似xml_find_all() xml_text() xml_name() 

doc %>% html_nodes("h4") %>% html_text()

#等效的
html_nodes(doc,"h4,h6")
html_node(doc,"h4,h6")

xml_nodes(doc,"h4,h6")
xml_node(doc,"h4,h6")

# 使用CSS selector
#https://sjp.co.nz/projects/selectr/
library(selectr)
querySelectorAll(doc, "div.con_l > div.pho_info > h4")

xpath <- css_to_xpath("div.con_l > div.pho_info > h4")
xpath


#########################
#示例2.5 提取多页信息：xpath
#来源于：http://stackoverflow.com/questions/24576962/how-write-code-to-web-crawling-and-scraping-in-r
# 原命令已经失效，此处已经修改
# 莫奈特画作属性下载，全部画作，详情页
#########################
library(XML)
library(httr)

# 单步分解：方便理解
# 网页地址固定部分
url <- "http://www.wikiart.org/en/claude-monet/mode/all-paintings-by-alphabet/?page="

# 第二页，总共22页
i <- 2

# 设置一个空的list准备存网页链接
hrefs <- list()

# 获取索引页内容，打印出来查看实际获得的代码
response <- GET(paste0(url,i))
doc <- content(response,type="text")
cat(doc,file="monet.html")

# 解析网页、获取对应的链接
doc <- htmlParse(response)
doc["//ul[@class='title']/li[1]/a/@href"]

# 将新获得链接与已经获得的部分合并
hrefs    <- c(hrefs,doc["//ul[@class='title']/li[1]/a/@href"])

# 构建一个详情页的网址
url      <- "http://www.wikiart.org"
paste0(url,hrefs[1])

#获取一个详情页
response <- GET(paste0(url,hrefs[1]))
doc <- content(response,type="text")
cat(doc,file="monet1.html")

doc <- htmlParse(response)

# 打开获取代码，查找相关信息，可以看到

# 画名在 h1下面的span中

doc["//h1/span[@itemprop='name']"]
# 创作时间
doc["//span[@itemprop='dateCreated']"]
# 作者
doc["//div[@class='artwork-title']/a[@class='artist-name']"]
# 风格
doc["//div[@class='info-line'][2]/a/span"]
# 流派
doc["//span[@itemprop='genre']"]

###   正式命令，
# 有22页，全部获取时间较长，建议只获取前2页。

url <- "http://www.wikiart.org/en/claude-monet/mode/all-paintings-by-alphabet/?page="
hrefs <- list()
for (i in 1:2) {
  response <- GET(paste0(url,i))
  # doc      <- content(response,type="text/html")
  doc <- htmlParse(response)
  hrefs    <- c(hrefs,doc["//ul[@class='title']/li[1]/a/@href"])
}

url      <- "http://www.wikiart.org"

# 定义xpath锚点集合
xPath    <- c(pictureName = "//h1/span[@itemprop='name']",
              date        = "//span[@itemprop='dateCreated']",
              author      = "//div[@class='artwork-title']/a[@class='artist-name']",
              style       = "//div[@class='info-line'][2]/a/span",
              genre       = "//span[@itemprop='genre']")

# 定义信息提取函数
get.picture <- function(href) {
  response <- GET(paste0(url,href))
  doc <- htmlParse(response)
  info     <- sapply(xPath,function(xp)ifelse(length(doc[xp])==0,NA,xmlValue(doc[xp][[1]])))
}


# 批量获取22页信息，获取时间较长。
pictures <- do.call(rbind,lapply(hrefs,get.picture))

head(pictures)
write.csv(pictures,file="monet.csv")


## 网页分析发现，这些信息也在头部信息中，我们可以想办法获取这些信息
#  <meta name="description" content="Apple Trees in Bloom, 1873 by Claude Monet. Impressionism. landscape" />


# 网页分析可以发现，信息在json中，可以在其中获得详情网页地址。
# https://www.wikiart.org/en/claude-monet/mode/all-paintings-by-alphabet?json=2&page=1

library(rjson)
help(library="rjson")

library(RCurl)

url <- "https://www.wikiart.org/en/claude-monet/mode/all-paintings-by-alphabet?json=2&page=1"
response <- getURL(url)
dat <- fromJSON(response)
hrefs <- list()
for (i in 1:20){
 hrefs <- c(hrefs, dat$Paintings[[i]]$paintingUrl[1])
}
# 再嵌套一个循环即可获得所有详情页链接




#########################
# 示例2.6：利用正则表达式进行信息提取
# 基本逻辑：
# 将网页读入成为一行一行的文本
# 通过特定字符串确定信息所在的行
# 然后截取相关的字符串
#########################
## 首先了解正则表达式
# 邮件地址的表达式
# 从下面三个字符中抽取邮件地址
word <- c('abcnoboby@stat.berkeley.edu','text with no email','first me@mything.com alsoyou@yourspace.com')

pattern <-'[-A-Za-z0-9_.%]+@[-A-Za-z0-9_.%]+\\.[A-Za-z]+'

#标定位置
(gregout <- gregexpr(pattern,word))
# 抽取
substr(word[1],gregout[[1]],gregout[[1]]+attr(gregout[[1]],'match.length')-1)

#通常我们会定义一个抽取函数,更方便
getcontent <- function(s,g){
  substring(s,g,g+attr(g,'match.length')-1)
}
#使用函数
getcontent(word[1],gregout[[1]])

#########################
#开始抓取豆瓣电影中250部最佳电影的资料：
library(httr)
library(xml2)
url<-'https://movie.douban.com/top250?format=text%27'
#readLines,注意先检验上面的网址是否与浏览器中完全一致
web <-readLines("doubao.txt",encoding="UTF-8")

#如果readLiness不能直接用，所以用了上面的命令
# 获取网页原代码，以行的形式存放在web变量中
html<-GET(url)
web<-content(html,"text")
setwd("/Users/liding/E/Bdata/liding17/2017R/l145spider")
writeLines(web,"doubao.txt")
web <-readLines("doubao.txt",encoding="UTF-8")

###批量获取250个电影分布的10个网页合并成一个大文本
###直接读入成为问本行
url<-'https://movie.douban.com/top250?format=text%27'
web <- readLines(url,encoding="UTF-8")
for(i in 0:9){
  url1<-paste('https://movie.douban.com/top250?start=',25*i,'&filter=',sep="")
  web1 <- readLines(url1,encoding="UTF-8")
  web<-c(web,web1)  
}

# 定义一个函数
getcontent <- function(s,g){
  substring(s,g,g+attr(g,'match.length')-2)
}

# 找到包含电影名称的行编号
name <- web[grep(' <div class="hd">',web)+2]

# 用正则表达式来提取电影名
gregout <- gregexpr('>\\W+<',name)

movie.names = 0
for(i in 1:length(gregout)){
  movie.names[i]<-getcontent(name[i],gregout[[i]])
}
movie.names <- sub('>','',movie.names)


# 找到包含电影发行年份的行编号并进行提取
year <- web[grep('<div class="star">',web)-4]
movie.year <- substr(year,29,32)

# 找到包含电影评分的行编号并进行提取
score <- web[grep('<span class="rating_num" property="v:average">',web)]
movie.score <- substr(score,79,81)

# 找到包含电影评价数量的行编号并进行提取
rating <- web[grep('<span class="rating_num" property="v:average">',web)+2]

library(stringr)
movie.rating <- substr(rating,32,str_length(rating)-10)
movie.rating <- sub('<span>','',movie.rating)

# 合成为数据框
movie <-data.frame(names=movie.names,year=as.numeric(movie.year),score=as.numeric(movie.score),rate=as.numeric(movie.rating))

# 绘散点图
library(ggplot2)

p <-ggplot(data=movie,aes(x=year,y=score))
p+geom_point(aes(size=rate),colour='lightskyblue4',
             position="jitter",alpha=0.8)+
  geom_point(aes(x=1997,y=8.9),colour='red',size=4)


#########################
#第三部分： 翻页
#示例3.1：简单翻页
#XML 批量化提取多页table，并编译GIS地址
#########################
#抓取美国大学名单列表，共300页
#https://collegestats.org/colleges/all/?pg=1
library(RCurl)
library(XML)
urlst <- "https://collegestats.org/colleges/all/?pg="
data<-data.frame()
for(i in 1:3){
  html<-getURL(paste0(urlst,i),.encoding='utf-8')
  tables <- readHTMLTable(html,stringsAsFactors = FALSE)
  data<- rbind(data,tables[[1]])
  cat(sprintf("第【%d】页抓取成功",i),sep = "\n")
  Sys.sleep(3)
}
DT::datatable(data)
# 修改变量名
write.table(data, "usacolleges.csv", row.names=FALSE, sep=",")

# library(readr)
# ucdata <- read_csv("usacolleges.csv")

#编译地址
library(ggmap)
sadd <- paste(data[,2],data[,3],data[,4],sep =" ")

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

#maps.google.com 
gGeoCode(c("Philadelphia, PA","New York, NY"))

geoadd <- gGeoCode(sadd)


#########################
#示例3.2: 简单翻页
# rvest提取抓取新浪调查页面，css Selector
#########################

#http://survey.news.sina.com.cn/list_all.php?dpc=1&state=going&page=2
library(plyr)
library(rvest)
library(stringr)
library("data.table")
library(dplyr)

urlst <- "http://survey.news.sina.com.cn/list_all.php?dpc=1&state=going&page="
Alldata<-data.frame()
# 共有739页，示例抓取3页
for(i in 1:3){
  web<-read_html(paste0(urlst,i),encoding='gb2312')
  sbiaoti<-web%>%html_nodes("li.clearfix div.item-wrap a")%>%html_text()
  sdate<-web%>%html_nodes("li.clearfix  span.date")%>%html_text()
  ssort<-web%>%html_nodes("li.clearfix  span.sort")%>%html_text()
  #链接
  link<-web%>%html_nodes("li.clearfix div.item-wrap a")%>%html_attrs()
  link1<-c(1:length(link))  #初始化一个和link长度相等的link1
  for(i in 1:length(link))
    link1[i]<-link[[i]][1]
  # link1  #查看link1
  
  data<-matrix(nrow=40,ncol=4)  # 定义一个40行，4列的矩阵
  data[,1]<-sbiaoti
  data[,2]<-ssort
  data[,3]<-sdate
  data[,4]<-link1
  #给列命名
  Alldata<- rbind(Alldata,data)
  Sys.sleep(1)
}
colnames(Alldata)<-c("biaoti","sort","sdate","link") 
DT::datatable(Alldata)  #查看Alldata数据前6行
write.csv(Alldata,file="sinasurveylist.csv",quote=F,row.names = F)  #保存csv文件中


#########################
#示例3.3：XML 提取爬民政部社会组织信息
#使用post方法进行查询
#经过分析Formdata可以发现，可以实现简单翻页
#2018年1月1日确认，修改
#########################
library(XML)
library(httr)
library(RCurl)

#伪造报头，可以通过浏览器分析请求信息得到
myH <- c(
  "User-Agent"="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="zh-cn,zh;q=0.5",
  #"Accept-Encoding"="gzip,deflate",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7",
  "Keep-Alive"="115",
  "Connection"="keep-alive",
  "Host"="hm.baidu.com",
  "Content-Type"="application/x-www-form-urlencoded; charset=UTF-8",
  #"Content-Length"=as.character(nchar(xx)*2+1),
  "Pragma"="no-cache",
  "Referer"="http://status.renren.com/ajaxproxy.htm",
  "Cache-Control"="no-cache"
)
# 设置debug容器与句柄
d <- debugGatherer()
cH <- getCurlHandle(debugfunction=d$update, verbose=T, ssl.verifyhost=F, 
                    ssl.verifypeer=F, followlocation=T, cookiefile="cc.txt")

#设置登录信息+伪造初始页面
pinfo <- c(
    "status"= "2",
    "tabIndex"="1",
    "regNum"="-1",
    "page_flag"="true",
    "pagesize_key"="macList",
    "current_page"="2",
    "total_count"="2310",
    "goto_page"="2"
)
# 登录信息编码转换
pinfo <- iconv(pinfo, from="GB18030", to="UTF-8")

# 用post方法获得第二页
ttt <- postForm("http://www.chinanpo.gov.cn/search/orgcx.html", httpheader=myH,
                .params=pinfo, curl=cH, style="post")

# 测试获得第2页的表格
tables <- readHTMLTable(ttt,stringsAsFactors = FALSE)
data<- tables[[2]]


## 用getURL方式将参数放在网址中获得第2页
## 关键是goto_page 参数
url <- "http://www.chinanpo.gov.cn/search/orgcx.html?legalName=&orgName=&corporateType=&managerDeptCode=&registrationNo=&unifiedCode=&orgAddNo=&ifCharity=&ifCollect=&status=2&regNumB=&regNumD=&tabIndex=1&regNum=-1&page_flag=true&pagesize_key=macList&goto_page=2&current_page=1&total_count=2310&to_page="
response <- getURL(url)
tables <- readHTMLTable(response,stringsAsFactors = FALSE)
# 数据在第二个表中
data<- tables[[2]]
DT::datatable(data)


# 力量获获取其中的15页
library(RCurl)
library(xml2)

# 网址分为三部分，前后两个部分固定
urlst <- "http://www.chinanpo.gov.cn/search/orgcx.html?legalName=&orgName=&corporateType=&managerDeptCode=&registrationNo=&unifiedCode=&orgAddNo=&ifCharity=&ifCollect=&status=2&regNumB=&regNumD=&tabIndex=1&regNum=-1&page_flag=true&pagesize_key=macList&goto_page="

urled <- "&current_page=1&total_count=2310&to_page="

data<-data.frame()
for(i in 1:15){
  html<-getURL(paste0(urlst,i,urled),.encoding='GB2312')
  tables <- readHTMLTable(html,stringsAsFactors = FALSE,header=TRUE)
  data<- rbind(data,tables[[2]][-1,])
  cat(sprintf("第【%d】页抓取成功",i),sep = "\n")
  Sys.sleep(1)
}
# 修改变量名
names(data) <- c('num','name','id','af','co','re','st')
data$num <- as.numeric(data$num)
DT::datatable(data)
write.table(data, "chinanpo.csv", row.names=FALSE, sep=",")


