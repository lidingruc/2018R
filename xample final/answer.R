# 2017年数据科学与社会研究：基于R的实践
# 期末考试操作部分的答案 
# 李丁 liding@ruc.edu.cn
#########################
# 第一题
library(tidyverse)
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
# 1
pdata <- data.frame(patientID, age, diabetes, 
                          status)
pdata
# 2
pdata$diabetes <- as.factor(pdata$diabetes )
# 3
pdata$status <- factor(pdata$status,levels=c("Poor","Improved","Excellent"),ordered=TRUE)

table(pdata$status)
# 4
names(pdata)[1] <- "pID"
# 5
pdata$diabetes[pdata$age==52] <- "Type2"

# 6
pdata$age2=age-18

#7
pdata <- pdata %>% mutate(status=forcats::fct_recode(status,"Fair"="Poor","Fair" = "Improved","Excellent" = "Excellent"))

# 8
setwd("/Users/liding/E/Bdata/liding17/考试2017R/final")
write.csv(pdata,file="pdata.csv")

#########################
###第二题
#（1）读入数据
library(haven)
cred <- read_spss("credit_card.sav")
#转换为因子变量
cred <- sjmisc::to_label(cred)

#（2） 作图（1） 均值图
cred %>% 
  filter(type=="Entertainment")%>% 
ggplot() + 
  geom_bar(aes(month, spent, fill =year), position = "dodge", stat = "summary", fun.y = "mean")

# 作图（2） boxplot
cred %>% 
  filter(type=="Entertainment")%>% 
  ggplot() + 
  geom_boxplot(mapping = aes(x = month, y = spent,color=year))

#（3、4）分类汇总，创建pspent变量
cred2 <- cred %>% group_by(custid,year,dob,gender,card ,card_date)%>%summarize(spent = sum(spent),items=sum(items))%>% mutate(pspent=spent/items)

#（5）当做独立样本的T检验
t.test(data=cred2,spent~year)
t.test(data=cred2,pspent~year)

# （6）长变宽
# 方法1
library(tidyr)
wred <- cred2 %>%
  gather(variable, value, -(c(custid:card_date))) %>%
  unite(temp, variable,year) %>%
  spread(temp, value)

# 方法2
library(reshape2)
md <- melt(cred2, id=c("custid","year","dob","gender","card","card_date"))
wred <- dcast(md, custid~variable+year)

# 方法3
library(data.table)
wred <- data.table::dcast(setDT(cred2), custid + dob + gender +card +card_date ~ year, value.var = c("spent", "items","pspent")) 

#（5）配对样本T检验，这个年份比较更正确
t.test(wred$spent_2007,wred$spent_2008,paired =TRUE)
t.test(wred$pspent_2007,wred$pspent_2008,paired =TRUE)

# （7）pspent2008的影响因素
# 
wred$age <- 2007-year(wred$dob)
wred$cdage <- 2007-year(wred$card_date)
wred$age2=wred$age^2

#方差分析
aov(pspent_2008~ card,wred  )
#回归分析
model1 <- lm(pspent_2008~ card + gender +age +age2+ cdage,data=wred)
model2 <- lm(pspent_2008~ card + gender +age +age2+ cdage+pspent_2007,data=wred)
summary(model1)

# 在没有控制2007年支出单价的情况，持卡类型持卡用户支付客单价有所不同，相对于美国运通卡的持卡用户，visa、万事达、discover持卡用户支付单价更低一些。而性别、年龄、持卡年龄的影响明显。


# 如果用foreign包读入数据
library(foreign)
cred <- read.spss("credit_card.sav",to.data.frame =TRUE)
# spss日期转换R格式 
# SPSS的日期以1582年10月14日为基准
cred$dob <- cred$dob++ISOdate(1582,10,14)
cred$card_date <- cred$card_date++ISOdate(1582,10,14)


#########################
#第三题
library(RCurl)
library(XML)
########（1）默认请求到本地
url <- "http://bj.xiaozhu.com/fangzi/24730694703.html"
html<-getURL(url)
class(html) #注意:得到的是文本

#通过XML包的htmlParse解析 
datas <- data.frame()
library(xml2)
doc <- htmlParse(html,encoding = "utf-8")
biaoti <- doc["//div[@class='pho_info']/h4"] 
xiaoqu <- doc["//div[@class='pho_info']/p/span[@class='pr5']"]
danjia <- doc["//div[@class='day_l']/span"]
pingjia <- doc["//em[@class='score-rate']"]
fangzhu <- doc["//a[@class='lorder_name']"]

data <- c(biaoti,xiaoqu,danjia,pingjia,fangzhu)
data <- sapply(data, xmlValue) 
datas <- rbind(datas,data)
names(datas) <- c("biaoti","xiaoqu","danjia","pingjia","fangzu")
datas


