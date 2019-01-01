# -*- coding: utf-8 -*-
# http://www.cnblogs.com/jasonfreak/p/5441512.html
from numpy import array
from numpy.random import normal, randint
#使用List来创造一组数据
data = [1, 2, 3]
#使用ndarray来创造一组数据
data = array([1, 2, 3])
#创造一组服从正态分布的定量数据
data1 = normal(0, 10, size=10)
#创造一组服从均匀分布的定性数据
data2 = randint(0, 10, size=10)

from numpy import mean, median

#计算均值
mean(data)
#计算中位数
median(data)

from scipy.stats import mode

#计算众数
mode(data)

from numpy import mean, ptp, var, std
 
#极差
ptp(data)
#方差
var(data)
#标准差
std(data)
#变异系数
mean(data) / std(data)

from numpy import mean, std

#计算第一个值的z-分数
(data[0]-mean(data)) / std(data)

from numpy import array, cov, corrcoef

data = array([data1, data2])

#计算两组数的协方差
#参数bias=1表示结果需要除以N，否则只计算了分子部分
#返回结果为矩阵，第i行第j列的数据表示第i组数与第j组数的协方差。对角线为方差
cov(data, bias=1)

#计算两组数的相关系数
#返回结果为矩阵，第i行第j列的数据表示第i组数与第j组数的相关系数。对角线为1
corrcoef(data)

from numpy import array
from numpy.random import normal

def genData():
    heights = []
    weights = []
    grades = []
    N = 10000

    for i in range(N):
        while True:
            #身高服从均值172，标准差为6的正态分布
            height = normal(172, 6)
            if 0 < height: break
        while True:
            #体重由身高作为自变量的线性回归模型产生，误差服从标准正态分布
            weight = (height - 80) * 0.7 + normal(0, 1)
            if 0 < weight: break
        while True:
            #分数服从均值为70，标准差为15的正态分布
            score = normal(70, 15)
            if 0 <= score and score <= 100:
                grade = 'E' if score < 60 else ('D' if score < 70 else ('C' if score < 80 else ('B' if score < 90 else 'A')))
                break
        heights.append(height)
        weights.append(weight)
        grades.append(grade)
    return array(heights), array(weights), array(grades)

heights, weights, grades = genData()

from matplotlib import pyplot

#绘制柱状图
def drawBar(grades):
    xticks = ['A', 'B', 'C', 'D', 'E']
    gradeGroup = {}
    #对每一类成绩进行频数统计
    for grade in grades:
        gradeGroup[grade] = gradeGroup.get(grade, 0) + 1
    #创建柱状图
    #第一个参数为柱的横坐标
    #第二个参数为柱的高度
    #参数align为柱的对齐方式，以第一个参数为参考标准
    pyplot.bar(range(5), [gradeGroup.get(xtick, 0) for xtick in xticks], align='center')

    #设置柱的文字说明
    #第一个参数为文字说明的横坐标
    #第二个参数为文字说明的内容
    pyplot.xticks(range(5), xticks)

    #设置横坐标的文字说明
    pyplot.xlabel('Grade')
    #设置纵坐标的文字说明
    pyplot.ylabel('Frequency')
    #设置标题
    pyplot.title('Grades Of Male Students')
    #绘图
    pyplot.show()

drawBar(grades)

from matplotlib import pyplot

#绘制饼形图
def drawPie(grades):
    labels = ['A', 'B', 'C', 'D', 'E']
    gradeGroup = {}
    for grade in grades:
        gradeGroup[grade] = gradeGroup.get(grade, 0) + 1
    #创建饼形图
    #第一个参数为扇形的面积
    #labels参数为扇形的说明文字
    #autopct参数为扇形占比的显示格式
    pyplot.pie([gradeGroup.get(label, 0) for label in labels], labels=labels, autopct='%1.1f%%')
    pyplot.title('Grades Of Male Students')
    pyplot.show()

drawPie(grades)

from matplotlib import pyplot
 
#绘制直方图
def drawHist(heights):
    #创建直方图
    #第一个参数为待绘制的定量数据，不同于定性数据，这里并没有事先进行频数统计
    #第二个参数为划分的区间个数
    pyplot.hist(heights, 100)
    pyplot.xlabel('Heights')
    pyplot.ylabel('Frequency')
    pyplot.title('Heights Of Male Students')
    pyplot.show()

drawHist(heights)

from matplotlib import pyplot

#绘制累积曲线
def drawCumulativeHist(heights):
    #创建累积曲线
    #第一个参数为待绘制的定量数据
    #第二个参数为划分的区间个数
    #normed参数为是否无量纲化
    #histtype参数为'step'，绘制阶梯状的曲线
    #cumulative参数为是否累积
    pyplot.hist(heights, 20, normed=True, histtype='step', cumulative=True)
    pyplot.xlabel('Heights')
    pyplot.ylabel('Frequency')
    pyplot.title('Heights Of Male Students')
    pyplot.show()

drawCumulativeHist(heights)

from matplotlib import pyplot

#绘制散点图
def drawScatter(heights, weights):
    #创建散点图
    #第一个参数为点的横坐标
    #第二个参数为点的纵坐标
    pyplot.scatter(heights, weights)
    pyplot.xlabel('Heights')
    pyplot.ylabel('Weights')
    pyplot.title('Heights & Weights Of Male Students')
    pyplot.show()

drawScatter(heights, weights)

from matplotlib import pyplot
 
 #绘制箱形图
def drawBox(heights):
    #创建箱形图
    #第一个参数为待绘制的定量数据
    #第二个参数为数据的文字说明
    pyplot.boxplot([heights], labels=['Heights'])
    pyplot.title('Heights Of Male Students')
    pyplot.show()

drawBox(heights)

#直方图
import matplotlib.pyplot as plt
plt.hist(heights)
plt.show()



# https://dzone.com/refcardz/data-mining-discovering-and
# http://blog.csdn.net/sbtgmz/article/details/53577141 做了一些修正和说明
import urllib2
url = 'http://aima.cs.berkeley.edu/data/iris.csv'
u = urllib2.urlopen(url)
localFile = open('iris.csv', 'w')
localFile.write(u.read())
localFile.close()

from numpy import genfromtxt, zeros
# read the first 4 columns
data = genfromtxt('iris.csv',delimiter=',',usecols=(0,1,2,3)) 
# read the fifth column
target = genfromtxt('iris.csv',delimiter=',',usecols=(4),dtype=str)

# 另一种读法
import pandas as pd
url = 'http://aima.cs.berkeley.edu/data/iris.csv'
data = pd.read_csv(url,usecols=(0,1,2,3))


#查看属性
print (data.shape)
#(150, 4)
print (target.shape)
#(150,)

print (set(target)) # build a collection of unique elements
set(['setosa', 'versicolor', 'virginica'])

#散点图
from pylab import plot, show
plot(data[target=='setosa',0],data[target=='setosa',2],'bo')
plot(data[target=='versicolor',0],data[target=='versicolor',2],'ro')
plot(data[target=='virginica',0],data[target=='virginica',2],'go')
show()

#4个叠加图
from pylab import figure, subplot, hist, xlim, show
xmin = min(data[:,0])
xmax = max(data[:,0])
figure()
subplot(411) # distribution of the setosa class (1st, on the top)
hist(data[target=='setosa',0],color='b',alpha=.7)
xlim(xmin,xmax)
subplot(412) # distribution of the versicolor class (2nd)
hist(data[target=='versicolor',0],color='r',alpha=.7)
xlim(xmin,xmax)
subplot(413) # distribution of the virginica class (3rd)
hist(data[target=='virginica',0],color='g',alpha=.7)
xlim(xmin,xmax)
subplot(414) # global histogram (4th, on the bottom)
hist(data[:,0],color='y',alpha=.7)
xlim(xmin,xmax)
show()

#分类
#编码为数字
t = zeros(len(target))
t[target == 'setosa'] = 1
t[target == 'versicolor'] = 2
t[target == 'virginica'] = 3

#分类
from sklearn.naive_bayes import GaussianNB
classifier = GaussianNB()
classifier.fit(data,t) # training on the iris dataset

#查看结果
print classifier.predict(data[0])
print t[0]

#效果评估
from sklearn import cross_validation
train, test, t_train, t_test = cross_validation.train_test_split(data, t, …
test_size=0.4, random_state=0)

classifier.fit(train,t_train) # train
print classifier.score(test,t_test) # test

#结果对比
from sklearn.metrics import confusion_matrix
print confusion_matrix(classifier.predict(test),t_test)

#分类效果
from sklearn.metrics import classification_report
print classification_report(classifier.predict(test), t_test, target_names=['setosa', 'versicolor', 'virginica'])

#Precision: the proportion of the predicted positive cases that were correct
#Recall (or also true positive rate): the proportion of positive cases that were correctly identified
#F1-Score: the harmonic mean of precision and recall

#平均预测效果
from sklearn.cross_validation import cross_val_score
# cross validation with 6 iterations 
scores = cross_val_score(classifier, data, t, cv=6)
print scores
from numpy import mean
print mean(scores)

#聚类分析
from sklearn.cluster import KMeans 
kmeans = KMeans(k=3, init='random') # initialization
kmeans.fit(data) # actual execution

c = kmeans.predict(data)
from sklearn.metrics import completeness_score, homogeneity_score
print completeness_score(t,c)

print homogeneity_score(t,c)

#实际类型，聚类类型
figure()
subplot(211) # top figure with the real classes
plot(data[t==1,0],data[t==1,2],'bo')
plot(data[t==2,0],data[t==2,2],'ro')
plot(data[t==3,0],data[t==3,2],'go')
subplot(212) # bottom figure with classes assigned automatically
plot(data[c==1,0],data[tt==1,2],'bo',alpha=.7)
plot(data[c==2,0],data[tt==2,2],'go',alpha=.7)
plot(data[c==0,0],data[tt==0,2],'mo',alpha=.7)
show()

#回归分析
from numpy.random import rand
x = rand(40,1) # explanatory variable
y = x*x*x+rand(40,1)/5 # depentend variable

from sklearn.linear_model import LinearRegression
linreg = LinearRegression()
linreg.fit(x,y)
#作图
from numpy import linspace, matrix
xx = linspace(0,1,40)
plot(x,y,'o',xx,linreg.predict(matrix(xx).T),'--r')
show()
# 均离差方差
from sklearn.metrics import mean_squared_error
print mean_squared_error(linreg.predict(x),y)

#相关分析
from numpy import corrcoef
corr = corrcoef(data.T) # .T gives the transpose
print corr

#相关矩阵-用颜色代表相关程度
from pylab import pcolor, colorbar, xticks, yticks
from numpy import arrange
pcolor(corr)
colorbar() # add
# arranging the names of the variables on the axis
xticks(arange(0.5,4.5),['sepal length',  'sepal width', 'petal length', 'petal width'],rotation=-20)
yticks(arange(0.5,4.5),['sepal length',  'sepal width', 'petal length', 'petal width'],rotation=-20)
show()

#Dimensionality Reduction，因子分析
from sklearn.decomposition import PCA
pca = PCA(n_components=2)
pcad = pca.fit_transform(data)

plot(pcad[target=='setosa',0],pcad[target=='setosa',1],'bo')
plot(pcad[target=='versicolor',0],pcad[target=='versicolor',1],'ro')
plot(pcad[target=='virginica',0],pcad[target=='virginica',1],'go')
show()

# 方差解释量
print pca.explained_variance_ratio_
# 未解释方差
print 1-sum(pca.explained_variance_ratio_)

#重生原数据，与真实原数据进行比较
data_inv = pca.inverse_transform(pcad)
print abs(sum(sum(data - data_inv)))

#提取的因子数量不同，还原的信息量不同
for i in range(1,5):
    pca = PCA(n_components=i)
    pca.fit(data)
    print sum(pca.explained_variance_ratio_) * 100,'%'

#Networks Mining
#https://gephi.org/datasets/lesmiserables.gml.zip
#     ------ 网络挖掘 ------  
# 通常我们分析的数据是以网络结构存储的，我们可以使用点和边描述之间的关系  
# 本章中我们将会介绍分析此类数据的基本步骤，称为图论，一个帮助我们创造、处理和研究网络的类库  
# 尤其我们将会介绍如何使用特定方法建立有意义的数据可视化，以及如何建立一组关联稠密的点  
# 使用图论可以让我们很容易的导入用于描述数据结构的最常用结构  
import networkx as nx  
G = nx.read_gml("/Users/liding/E/Bdata/ptemp/liding/lesmiserables.gml")      # networkx 必须要下载 1.9.1 版本才行  
# 在上述代码我们导入了《悲惨世界》同时出现的单词组成的网络，可以通过https://gephi.org/datasets/lesmiserables.gml.zip免费  
# 下载，数据以GML格式存储。我们还可以使用下面的命令导入并可视化网络：  
nx.draw(G, node_size=0, edge_color="b", alpha=.2, font_size=7)


import igraph.test 

#链接度
deg = nx.degree(G)
from numpy import percentile, mean, median
print min(deg.values())
print percentile(deg.values(),25) # computes the 1st quartile
print median(deg.values())
print percentile(deg.values(),75) # computes the 3rd quartile
print max(deg.values())10 

#挑选连接度大于10的案例
Gt = G.copy()
dn = nx.degree(Gt)
for n in Gt.nodes():
 if dn[n] <= 10:
  Gt.remove_node(n)
nx.draw(Gt,node_size=0,edge_color='b',alpha=.2,font_size=12)

#小集团分析
from networkx import find_cliques
cliques = list(find_cliques(G))
print max(cliques, key=lambda l: len(l))


