
# coding: utf-8

# In[54]:

import pandas as pd
import numpy as np
import jieba.posseg as pseg
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import GaussianNB
from sklearn.decomposition import NMF, LatentDirichletAllocation
import json
import jieba
import matplotlib.pyplot as plt
get_ipython().magic('matplotlib inline')


# In[55]:

# words cut
# 如果要安装新的模块 使用 anconda prompt 可以安装


# In[56]:

seg_list = jieba.cut("我来到北京清华大学", cut_all=True)
print("Full Mode:", "/ ".join(seg_list) ) # 全模式

seg_list = jieba.cut("我来到北京清华大学", cut_all=False)
print ("Default Mode:", "/ ".join(seg_list))  # 精确模式

seg_list = jieba.cut("他来到了网易杭研大厦")  # 默认是精确模式
print (", ".join(seg_list))

seg_list = jieba.cut_for_search("小明硕士毕业于中国科学院计算所，后在日本京都大学深造")  # 搜索引擎模式
print (", ".join(seg_list))


# In[57]:

words = pseg.cut("小明硕士毕业于中国科学院计算所，后在日本京都大学深造")  # 搜索引擎模式
for w in words:
    print (w.word,w.flag)


# In[60]:

# stopwords = [ line.rstrip().decode('ANSI').encode('utf-8') for line in open('/Users/liding/E/Bdata/Course/6TextasData/TextasdatachineseStopWords.txt') ]
with open('/Users/liding/E/Bdata/Course/6TextasData/chineseStopWords.txt',encoding="gbk") as f:
    stopwords = f.read().splitlines()
    


# In[62]:

report= pd.read_csv('samgov.csv',encoding="gbk")
report.head()


# In[63]:

list(pseg.cut('中共中央总书记'))


# In[65]:

txt_list = []
cixing=["x","zg","uj","ul","e","d","uz","y","eng","m"]
for i, txt in enumerate(report['content']):
    result = ''
    try:
        for w in pseg.cut(txt):
                if not str(w.flag)  in cixing:
                    seg=str(w.word)
#                    if seg not in stopwords:
                    result+=str(seg)+" "
        print (i)
    finally:
        pass
    txt_list.append(result)


# In[66]:

print(txt_list)


# In[67]:

vectorizer = CountVectorizer(min_df=1)
dtm = vectorizer.fit_transform(txt_list)
vocab = vectorizer.get_feature_names()


# In[68]:

lda = LatentDirichletAllocation(n_topics=20, max_iter=5,)
l = lda.fit(dtm)
data = pd.DataFrame(l.components_)
def print_top_words(model, feature_names, n_top_words):
    for topic_idx, topic in enumerate(model.components_):
        print("Topic #%d:" % topic_idx)
        print(" ".join([feature_names[i]
                        for i in topic.argsort()[:-n_top_words - 1:-1]]))
    print()
print_top_words(l, vocab, 10)


# In[69]:

num_topics = 20
num_top_words = 20


# In[70]:

clf = NMF(n_components=num_topics, random_state=1)
doctopic = clf.fit_transform(dtm)
topic_words = []
for topic in clf.components_:
    word_idx = np.argsort(topic)[::-1][0:num_top_words]
    topic_words.append([vocab[i] for i in word_idx])
words = pd.DataFrame(topic_words)
words.to_csv('topicterm.csv', encoding='gb18030')
doctopic = doctopic / np.sum(doctopic, axis=1, keepdims=True)
doctopic = pd.DataFrame(doctopic)
report = pd.concat([report, doctopic], axis=1)
report.to_csv('topics.csv')


# In[71]:

N, K = doctopic.shape
ind = np.arange(N) 
width = 0.5


# In[72]:

plt.hist(doctopic[11])


# In[73]:

plt.scatter(doctopic[4], doctopic[7])


# In[74]:

np.mean(doctopic)


# In[79]:

topic = ('0', '1', '2', '3', '4','5', '6', '7', '8', '9')
y_pos = np.arange(len(topic))
percent = (0.327, 0.025, 0.063, 0.052, 0.086,0.056, 0.052, 0.027, 0.119, 0.192)
error = np.random.rand(len(topic))

plt.bar(y_pos, percent, align='center', color='Y')
plt.xticks(y_pos, topic)
plt.title('Frequncey of Topics')

plt.show()


# In[80]:

plt.boxplot(doctopic[11])


# In[ ]:



