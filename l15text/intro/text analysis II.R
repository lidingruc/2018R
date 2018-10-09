# 文本分析：主题分析和聚类分析


# 文本分析主题模型

# 设置文档存储位置
setwd("/Users/liding/E/Bdata/liding17/2018R/l15text/intro/data")

library(rJava);  
library(Rwordseg); 
library(tm); 

# 安装中文TM包
#install.packages("C:\\SogouDownload\\tmcn_0.1-4.tar", repos=NULL, type="source") 
library(tmcn)
library(tm)
library(Rwordseg)
lecture<-read.csv("samgov1.csv",encoding="utf-8")
names(lecture)
nchar(lecture)
# == 文本预处理  
res=lecture[lecture!=" "]; 
ls()
fix(res)

#剔除URL  
res=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",res);   
res=gsub(pattern="[n|t]","",res);   

#剔除特殊词  
res=gsub(pattern="[我|你|的|了|一下|一个|没有|这样|现在|原告|被告|北京|法院|简称]","",res);

#剔除数字 
res=gsub(pattern="/^[^0-9]*$/","",res); 

# 安装新词典
# 搜狗 词库 https://pinyin.sogou.com/dict/cate/index/361

#installDict("C:\\SogouDownload\\diming.scel","diming")
#installDict("C:\\SogouDownload\\xinli.scel","xinli")
#installDict("C:\\SogouDownload\\zzx.scel","zzx")
#listDict()
# uninstallDict()

d.vec <- segmentCN("samgov1.csv", returnType = "tm")
# read file after word segment
samgov.segment <- read.table("samgov1.segment.csv", header = TRUE, fill = TRUE, stringsAsFactors = F, 
 sep = ",")
head(samgov.segment)
fix(samgov.segment)

# 创建DTM文件(document term matrix)
d.corpus <- Corpus(VectorSource(samgov.segment$content))
inspect(d.corpus[1:10])
d.corpus <- tm_map(d.corpus, removeWords, stopwordsCN())
ctrl <- list(removePunctuation = TRUE, removeNumbers= TRUE, wordLengths = c(1, Inf), stopwords = stopwordsCN(), wordLengths = c(2, Inf))
d.dtm <- DocumentTermMatrix(d.corpus, control = ctrl)
inspect(d.dtm[1:10, 110:112])
d.dtm

# 词频分析
findFreqTerms(d.dtm,100)
findFreqTerms(d.dtm,50)

# 词频共显关系
findAssocs(d.dtm, "社会", 0.5)
findAssocs(d.dtm, "征用", 0.5)

# 删除稀疏矩阵
d.dtm.sub <- removeSparseTerms(d.dtm, 0.99)
dim(d.dtm.sub)
findAssocs(d.dtm.sub, "农民", 0.5)
dim(d.dtm)

# 聚类分析
library(proxy) # proxy中的dist函数可以计算文档间的余弦相似度，作为聚类分析的基础
d.dist <- proxy:: dist(as.matrix(d.dtm.sub),method='cosine')
heatmap(as.matrix(d.dist),labRow=FALSE, labCol=FALSE)
d.clust <- hclust(d.dist) #聚类分析
result<-cutree(d.clust,k=5)
summary(result)
result
plot(d.clust)

# 主题分析
library(topicmodels)
ctm<-CTM(d.dtm,k=10, control=list(seed=111))
Terms <- terms(ctm, 10)
Terms[,1:10]

ctm<-CTM(d.dtm,k=5, control=list(seed=111))
Terms <- terms(ctm, 10)
Terms[,1:5]


# 相似性
library(stringr)
library(text2vec)
data("movie_review")
# select 500 rows for faster running times
movie_review = movie_review[1:500, ]
prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}
movie_review$review_clean = prep_fun(movie_review$review)
cosine 

#定义两个文档：
doc_set_1 = movie_review[1:300, ]
it1 = itoken(doc_set_1$review_clean, progressbar = FALSE)

# specially take different number of docs in second set
doc_set_2 = movie_review[301:500, ]
it2 = itoken(doc_set_2$review_clean, progressbar = FALSE)

#1、Jaccard距离
vectorizer = hash_vectorizer(2 ^ 18, c(1L, 2L))
dtm1 = create_dtm(it1, vectorizer)
dtm2 = create_dtm(it2, vectorizer)
d1_d2_jac_sim = sim2(dtm1, dtm2, method = "jaccard", norm = "none")
#生成了一个300*200的相似性矩阵。
dtm1_2 = dtm1[1:200, ]
dtm2_2 = dtm2[1:200, ]
d1_d2_jac_psim = psim2(dtm1_2, dtm2_2, method = "jaccard", norm = "none")
str(d1_d2_jac_psim)
#生成了一个200个数值的相似性系数。

#2、cosine距离
d1_d2_cos_sim = sim2(dtm1, dtm2, method = "cosine", norm = "l2")

#3、Euclidean 距离
x = dtm_tfidf_lsa[1:300, ]
y = dtm_tfidf_lsa[1:200, ]
m1 = dist2(x, y, method = "euclidean")

#4、RWMD距离
data("movie_review")
tokens = movie_review$review %>%
  tolower %>%
  word_tokenizer
v = create_vocabulary(itoken(tokens)) %>%
  prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5)
corpus = create_corpus(itoken(tokens), vocab_vectorizer(v, skip_grams_window = 5))
dtm = get_dtm(corpus)
tcm = get_tcm(corpus)
glove_model = GloVe$new(word_vectors_size = 50, vocabulary = v, x_max = 10)
wv = glove_model$fit(tcm, n_iter = 10)
rwmd_model = RWMD(wv)
rwmd_dist = dist2(dtm[1:10, ], dtm[1:100, ], method = rwmd_model, norm = 'none')
