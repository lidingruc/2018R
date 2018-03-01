# -------------------------------------------
# --作业3
# -------------------------------------------
# 读入数据并进行预处理
library(haven)
library(tidyverse)
setwd("/Users/liding/E/Bdata/liding17/2017R/lesson9/")
cgss2013 <- read_spss("cgss2013.sav") 


#定义一个函数方便查看数据集中的变量和变量标签
des <- function (dfile) {
  lbl = sapply(dfile, attr, 'label')
  if (is.list(lbl)) {
    lbl[sapply(lbl, is.null)] = ''
    lbl[sapply(lbl, length) > 1] = ''
    lbl = unlist(lbl)
  }
  Encoding(lbl) = 'UTF-8'
  dfile_var = data.frame(var =names(dfile), lbl = lbl) 
  View(dfile_var)
}  
# 通过View数据框的方式查看有哪些变量
des(cgss2013)


#替换用户定义缺失值为系统缺失值
cgss2013[cgss2013==-1] <- NA
cgss2013[cgss2013==-2] <- NA
cgss2013[cgss2013==-3] <- NA
cgss2013[cgss2013==9999997] <- NA
cgss2013[cgss2013==9999998] <- NA
cgss2013[cgss2013==9999999] <- NA

# 删除没有用到的取值标签
cgss2013 <- sjlabelled::drop_labels(cgss2013) 

#将还有取值标签的变量转为因子，无取值标签的变量仍然为labelled类型
cgss2013 <- sjmisc::to_label(cgss2013) 

#将剩余的labelled变量都转化为普通数值变量
w <- which(sapply(cgss2013, class) == 'labelled')

cgss2013[w]<- lapply(cgss2013[w], 
                     function(x) as.numeric(as.character(x)))

# 变量处理
cgss2013 <- cgss2013 %>%
  mutate(edu = fct_collapse(a7a,
                            "小学"="私塾",
                            "初中"=c("其他","初中","技校"),
                            "高中"=c("职业高中","普通高中","中专"),
                            "大学"=c("大学专科(成人高等教育)","大学专科(正规高等教育)","大学本科(成人高等教育)","大学本科(正规高等教育)","研究生以上")),
         ethic= fct_collapse(a4,
                             "汉族"="汉",
                             "少数民族"=c("蒙","满","回","藏","壮","其他")),
         lninc=log10(a8a+1),
         age=2013-a3a
  )

## 方差分析
aov.inc <-  aov(cgss2013$a8a~cgss2013$edu)
summary(aov.inc)

#https://www.statmethods.net/stats/anova.html
#R provides Type I sequential SS, not the default Type III marginal SS reported by SAS and SPSS. 
drop1(aov.inc,~.,test="F") # type III SS and F Tests

# 求均值 方法1
model.tables(aov.inc,"mean")

#回归系数作图，不同于均值
plot(aov.inc$coefficients,type="b")

# 求均值，方法2
means<- round(tapply(cgss2013$a8a,cgss2013$edu, mean,na.rm=TRUE), digits=2) 

#Bartlett test检验方差相等
bartlett.test(a8a~edu,cgss2013)

#Fligner-Killeen test检验方差相等
fligner.test(a8a~edu,cgss2013)

# HH::hovPlot(a8a~edu,data=cgss2013)
# 事后分组对比检验
TukeyHSD(aov.inc)


#作箱子图，事先删除缺失值
ggplot(data=na.omit(cgss2013[,c("edu","a8a")]),
       aes(x=edu,y=a8a))+
       geom_boxplot() +
       theme(text=element_text(family="STKaiti"))+
       labs(x="教育水平",y="年收入")

#仅删除分类变量缺失值
ggplot(data=subset(cgss2013,!is.na(edu)),aes(x=edu,y=a8a))+
  geom_boxplot(na.rm=TRUE) +
  theme(text=element_text(family="STKaiti"))

# 直方图是一种参考
ggplot(data = cgss2013, mapping = aes(x = a8a, fill = ethic)) + geom_histogram(position='identity') +
  theme(text=element_text(family="STKaiti"))

# 回归模型
lm_inc <- lm(a8a ~ ethic + edu +age, data = cgss2013)
summary(lm_inc)

ggplot(lm_inc,aes(x = ethic, y = a8a))+geom_point()
ggplot(lm_inc,aes(x = edu, y = a8a))+geom_point()

#交互效应
lm_inc2 <- lm(a8a ~ ethic + edu, data = cgss2013)
lm_inc2 <- lm(a8a ~ ethic * edu, data = cgss2013)

summary(lm_inc2)

lm_inc3 <- lm(a8a ~ ethic*age +  edu, data = cgss2013)
summary(lm_inc3)

lm_inc4 <- lm(a8a ~ edu*age + ethic, data = cgss2013)
summary(lm_inc4)



## 对数之后作图
cgss2013 %>%
  filter(!is.na(ethic)) %>%
  ggplot(aes(x=ethic,y=lninc))+
  geom_boxplot(shape=NA) +
  theme(text=element_text(family="SimSun"))+
  labs(x="民族",y="年收入对数")

#在boxplot中 增加中位数统计值
eth_meds <- cgss2013  %>%
  filter(!is.na(ethic),!is.na(lninc)) %>% 
  group_by(ethic) %>% 
  summarise(med = median(lninc))

cgss2013 %>%
  filter(!is.na(ethic),!is.na(lninc)) %>%
  ggplot(aes(x=ethic,y=lninc))+
  geom_boxplot() +
  geom_text(data = eth_meds, 
            aes(x = ethic, y = med, label = round(med,1)), 
            size = 4, vjust = 1.2)+
  theme(text=element_text(family="SimSun"))+
  labs(x="民族",y="年收入对数")

#回归分析
lm_inc <- lm(a8a ~ I(2013-a3a) + a2, data = cgss2013)

cgss2013 %>%
  filter(!is.na(ethic),!is.na(lninc)) %>%
  group_by(ethic) %>%
  summarize(mofinc=mean(lninc))
  
  
