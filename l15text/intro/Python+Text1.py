
# coding: utf-8

# # Python Text
# 
# ## Wednesday 24th August 2016
# ## Tianguang Meng

# - [1 Input Text](#1-Input-Text)
# - [2 Text Detection](#2-Text-Detection)
# - [3 Text Computation](#3-Text-Computation)
# - [4 Text Data](#4-Text-Data)
# - [5 Text Reading](#5-Text-Reading)

# ### 1-Input Text

# In[1]:

news = '习近平抵达捷克进行国事访问'


# In[2]:

print(news)


# In[3]:

texts = ['云南去年5800余名公职人员受处分','国务院成立山东疫苗案调查组','东北三省取消玉米临储']


# In[4]:

print(texts[1])


# ## 2-Text Detection

# In[6]:

len("习近平抵达捷克进行国事访问") #length of the string 


# In[9]:

len("Hello Tsinghua!") #length of the string 


# In[10]:

'Alice' == "Alice"


# In[12]:

'Alice' == "alice"


# In[13]:

"That's a nice hat."


# In[14]:

print("1\t2") #\t is tab


# In[15]:

print("云南去年5800余名公职人员受处分\n国务院成立山东疫苗案调查组\n东北三省取消玉米临储") #end-line character


# ## 3-Text Computation

# In[16]:

"White"+" "+"Rabbit"


# In[17]:

"alice".capitalize()


# In[18]:

print ("alice".upper())


# In[19]:

print ("ALICE".lower())


# In[20]:

"There were doors all around the hall".count('e')


# In[21]:

"There were doors all round the hall".count('all')


# In[22]:

"人民币在过去半年总是上演躺着也要中枪的剧情。英国脱欧一役中，人民币并没有太多暴露于脱欧的风险头寸，然后投资者寻求避险，人民币遭到“错杀”，在岸人民币兑美元跌至5年低位。另外在今年年初，美联储加息的预期让人民币连跌4日，最终还是央妈出手才逼退空头。上周三人民币兑美元的开盘价贬至6.6857，创2010年11月来新低，而人民币兑一篮子货币也达到了2014年10月以来最低。离岸人民币汇率一度跌破6.7，在岸人民币汇率极其逼近6.7。上周前三天内，人民币对美元汇率中间价累计贬值545点，较英国退欧后创下的低点再跌329点。最近三个月，在岸人民币兑美元汇率，以及离岸人民币兑美元汇率也贬值了3.25%。年初至今，人民币兑一篮子货币汇率已经贬值了5.9%。".count('人')


# In[23]:

print ("   White Rabbit   ".strip())
print ("\t White Rabbit  \t ".strip('\t '))


# In[24]:

"Rabbit".startswith("R")


# In[25]:

"Rabbit".endswith("t")


# In[26]:

print ("a" in "Rabbit")#returns a boolean 
print ("doors" in "There were doors all round the hall") #returns a boolean


# ## 4-Text Data

# In[27]:

id_number={'张三':'54201','李四':'87302','王五':'32201'}


# In[28]:

print (id_number['李四'])


# In[29]:

category={}
category["苹果"]="fruit"
category["香蕉"]="fruit"
category["草莓"]="fruit"
category["土豆"]="vegetable"
category["西红柿"]="vegetable"


# In[30]:

category["香蕉"]


# In[31]:

doc_a = "Brocolli is good to eat. My brother likes to eat good brocolli, but not my mother."
doc_b = "My mother spends a lot of time driving my brother around to baseball practice."
doc_c = "Some health experts suggest that driving may cause increased tension and blood pressure."
doc_d = "I often feel pressure to perform well at school, but my mother never seems to drive my brother to do better."
doc_e = "Health professionals say that brocolli is good for your health."

# compile sample documents into a list
doc_set = [doc_a, doc_b, doc_c, doc_d, doc_e]


# In[32]:

print (doc_set)


# ## 5-Text Reading

# In[4]:

import os
import re
import pandas as pd
import numpy as np
import jieba.posseg as pseg


# In[8]:

report= pd.read_csv('samgov.csv',encoding="gbk")
report.head()


# In[17]:

print (report['content'][0])


# In[13]:

list(pseg.cut('尊敬的苏书记您好！赣县王母渡镇新兴村委员会（书记、村长、会计）三个职位在近5年内从未公开竟选，三人内定，并没有经过正规程序来进行。近5年内村财务从未公开过。村民批地建房由村书记一人收钱不开收据不开发票口头承诺要定可以办好，收费标准并无文件规定，漫口要价村民不知情，但近5年内的批地申请表并未办下来，一直积压在镇国土资源管理所。恳请苏书记督促赣县检察机关、县纪检部门到镇和村民中调查，因镇政府和镇国土资源所和村委员会的三个干部有着千丝万缕的联系，所以镇相关并未有过调查。赣县王母渡镇新兴村委员会村长去年说了一句话：我们光靠共产党的工资那里够呀，吃饭都不够，所以不搞点项目或从国土审批中一次性从村民手中收取一个上限的费用那才不吃亏，也确实村委会工作人员一次随口开价就从未有过批地费用补差或退回，难不成就每次都那么准刚刚好就够了。'))


# In[14]:

len(report) #length of the string 


# In[18]:

report2 = pd.read_excel('/Users/liding/E/Bdata/Course/6TextasData/sample2.xlsx')
report2.head()


# In[19]:

len(report2) #length of the string 


# <br><br><br><center><font color=#13577F size=8 face='arial black'>大数据社会科学的时代来临了!</font></center><br><br>

# <br><br><br><center><font color=#505050 size=4 face='arial black'>邮箱：maxmeng@tsinghua.edu.cn</font></center><br><br>

# <font color=#505050 size=4 face='arial black'>Copyright@Tsinghua Research Center on Data Science and Governance</font>

# In[ ]:



