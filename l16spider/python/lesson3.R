########
#采XML的方式
#http://www.omegahat.net/RSXML/gettingStarted.html
library(XML)

url <-"http://bj.xiaozhu.com/fangzi/5098280314.html"
#doc <- htmlTreeParse(url,encoding='utf-8')  #注意返回的结果是不一样的

#htmlParse可以抓取http的页面，不能抓取https的页面
doc <- htmlTreeParse(url, useInternalNodes=T,encoding='utf-8')

#xml文档可以像变量一样引用
doc["//title"]
doc["//title/text()"]
#内容列表
doc["//head"]
doc["//head/*"]
doc["//head/meta"]
ti<-doc["//head/title"]
#特征
doc["//head/meta[@name='keywords']"]
doc["//head/link[@rel='stylesheet']"]
#提出内容
doc["//h4[@class='zhima_score']/text()"]

#getNodeSet方式
getNodeSet(doc,"//head")
nodh4<-getNodeSet(doc, "//h4")
getNodeSet(doc, "//h4/em")
#xpathApply与getNodeSet等价
xpathApply(doc,"//h4/em")
xpathApply(doc,"//h6")
xpathSApply(doc,"//h6")

#获取文本内容(返回的内容属性是不同的)
xpathApply(doc,"//h4/em/text()")
sapply(getNodeSet(doc, "//h4/em") ,xmlValue)

#获取属性
getNodeSet(doc,"//title")
sapply(getNodeSet(doc, "//title") ,xmlName)
sapply(getNodeSet(doc, "//title") ,xmlValue)

#多个节点
h6<-getNodeSet(doc, "//h6")

summary(getNodeSet(doc, "//h6"))
length(getNodeSet(doc, "//h6/text()"))


#注意两者返回的内容不同
getNodeSet(doc, "//h6/text()")
doc["//h6/text()"]

sapply(getNodeSet(doc, "//h6") ,xmlValue)

#获取多个标记点
getNodeSet(doc, "//h6/text() | //h4/text()")

# 加入class变量
getNodeSet(doc, '//a[@class="lorder_name"]')
# class变量
getNodeSet(doc, '//*[@class="bg_box"]')

# class变量
getNodeSet(doc, text("5分"))

xmlValue(xmlNode(html,"title"))

getNodeSet(doc, c("//h6","//h4"))
getNodeSet(doc, "//a[@class='lorder_name']")[[1]]
biaoti<-xmlValue(getNodeSet(doc, "//h6")[[1]])
Encoding(biaoti) = "UTF-8"
biaoti


library(selectr)
querySelectorAll(doc, "h4,h6")
url<-"http://bj.xiaozhu.com/fangzi/5112134313.html"
html<-getURL(url)
doc<-htmlParse(html)
nodi=getNodeSet(doc,'//*[@id="introducePart"]/div[5]/div[2]/div[1]/ul/li')
a<-sapply(nodi, xmlValue)
b<-sapply(nodi, function(x) xmlGetAttr(x,"class")) 
names(a)<-b
a["s_ico_no"]

c<-as.data.frame(cbind(a,b))
c[c$b!="s_ico_no",1]





