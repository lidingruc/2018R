Demo 5 sentiment analysis with supervised machine learning
========================================================



```r
##########################################
"Sentiment analysis with machine learning"
```

```
## [1] "Sentiment analysis with machine learning"
```

```r
##########################################

# Text analysis in R has been well recognized (see the R views on natural 
# language processing). Part of the success belongs to the tm package: A 
# framework for text mining applications within R. It did a good job for text 
# cleaning (stemming, delete the stopwords, etc) and transforming texts to 
# document-term matrix (dtm).  

# As you know the most important part of text analysis is to get the feature
# vectors for each document. The word feature is the most important one. Of
# course, you can also extend the unigram word features to bigram and trigram,
# and so on to n-grams. However, here for our simple case, we stick to the
# unigram word features. Note, it’s easy to use ngrams in R. In the past, the
# package of Rweka supplies functions to do it. Now, you can set the ngramLength
# in the function of create_matrix using RTextTools.

# The first step is to read data:

library(RTextTools)
```

```
## Loading required package: SparseM
## 
## Attaching package: 'SparseM'
## 
## 下列对象被屏蔽了from 'package:base':
## 
##     backsolve
## 
## KernSmooth 2.23 loaded
## Copyright M. P. Wand 1997-2009
```

```r
library(e1071)



pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive')
)


neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative')
)


test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

tweets = rbind(pos_tweets, neg_tweets, test_tweets)


# ----------------------naive bayes-----------------------#


# build dtm
matrix= create_matrix(tweets[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE,  # we can also removeSparseTerms
                      stemWords=FALSE) 


# train the model

# Now, we can train the naive Bayes model with the training set. Note that,
# e1071 asks the response variable to be numeric or factor. Thus, we convert
# characters to factors here. This is a trick.

mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )

# test the validity
predicted = predict(classifier, mat[11:15,]); predicted
```

```
## [1] positive positive negative negative positive
## Levels: negative positive
```

```r
table(tweets[11:15, 2], predicted)
```

```
##           predicted
##            negative positive
##   negative        2        1
##   positive        0        2
```

```r
recall_accuracy(tweets[11:15, 2], predicted)
```

```
## [1] 0.8
```

```r
# > 0.8

#-----------------the other machine learning methods------------------#

# How about the other machine learning methods? As I mentioned, we can do it
# using RTextTools.

# First, to specify our data:

# specify response variable, training set, testing set.
container = create_container(matrix, as.numeric(as.factor(tweets[,2])),
                               trainSize=1:10, testSize=11:15,virgin=FALSE)

# Second, to train the model with multiple machine learning algorithms:
models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))
# Now, we can classify the testing set using the trained models.
results = classify_models(container, models)
# How about the accuracy?
# accuracy table
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
```

```
##    
##     1 2
##   1 3 0
##   2 1 1
```

```r
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])
```

```
##    
##     1 2
##   1 1 2
##   2 0 2
```

```r

# recall accuracy
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
```

```
## [1] 0.8
```

```r
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])
```

```
## [1] 0.6
```

```r
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"])
```

```
## [1] 0.6
```

```r
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"BAGGING_LABEL"])
```

```
## [1] 0.6
```

```r
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])
```

```
## [1] 0.2
```

```r
# To summarize the results (especially the validity) in a formal way:
# model summary
analytics = create_analytics(container, results)
summary(analytics)
```

```
## ENSEMBLE SUMMARY
## 
##        n-ENSEMBLE COVERAGE n-ENSEMBLE RECALL
## n >= 1                 1.0              0.60
## n >= 2                 1.0              0.60
## n >= 3                 1.0              0.60
## n >= 4                 0.6              0.67
## 
## 
## ALGORITHM PERFORMANCE
## 
##        SVM_PRECISION           SVM_RECALL           SVM_FSCORE 
##                0.165                0.165                0.165 
##    BAGGING_PRECISION       BAGGING_RECALL       BAGGING_FSCORE 
##                0.300                0.500                0.375 
##    FORESTS_PRECISION       FORESTS_RECALL       FORESTS_FSCORE 
##                0.875                0.750                0.765 
##       TREE_PRECISION          TREE_RECALL          TREE_FSCORE 
##                0.300                0.500                0.375 
## MAXENTROPY_PRECISION    MAXENTROPY_RECALL    MAXENTROPY_FSCORE 
##                0.750                0.665                0.585
```

```r
head(analytics@document_summary)
```

```
##   MAXENTROPY_LABEL MAXENTROPY_PROB SVM_LABEL SVM_PROB FORESTS_LABEL
## 1                2          0.6444         1   0.5655             1
## 2                2          0.9922         1   0.8446             2
## 3                1          1.0000         2   0.9808             1
## 4                2          0.7347         1   0.6059             1
## 5                2          0.5000         2   0.5314             1
##   FORESTS_PROB BAGGING_LABEL BAGGING_PROB TREE_LABEL TREE_PROB MANUAL_CODE
## 1        0.500             1         0.64          1       0.5           2
## 2        0.595             1         0.68          1       0.5           2
## 3        0.775             1         0.92          1       0.5           1
## 4        0.610             1         0.56          1       0.5           1
## 5        0.570             1         0.68          1       0.5           1
##   CONSENSUS_CODE CONSENSUS_AGREE CONSENSUS_INCORRECT PROBABILITY_CODE
## 1              1               4                   1                2
## 2              1               3                   1                2
## 3              1               4                   0                1
## 4              1               4                   0                2
## 5              1               3                   0                1
##   PROBABILITY_INCORRECT
## 1                     0
## 2                     0
## 3                     0
## 4                     1
## 5                     0
```

```r
analytics@ensemble_summary
```

```
##        n-ENSEMBLE COVERAGE n-ENSEMBLE RECALL
## n >= 1                 1.0              0.60
## n >= 2                 1.0              0.60
## n >= 3                 1.0              0.60
## n >= 4                 0.6              0.67
```

```r
# To cross validate the results:
N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
```

```
## Fold 1 Out of Sample Accuracy = 0.3333
## Fold 2 Out of Sample Accuracy = 0.3333
## Fold 3 Out of Sample Accuracy = 0.25
## Fold 4 Out of Sample Accuracy = 0.6
```

```
## [[1]]
## [1] 0.3333 0.3333 0.2500 0.6000
## 
## $meanAccuracy
## [1] 0.3792
```

```r
cross_validate(container,N,"SVM")
```

```
## Fold 1 Out of Sample Accuracy = 0.3333
## Fold 2 Out of Sample Accuracy = 0.6
## Fold 3 Out of Sample Accuracy = 1
## Fold 4 Out of Sample Accuracy = 0.6667
```

```
## [[1]]
## [1] 0.3333 0.6000 1.0000 0.6667
## 
## $meanAccuracy
## [1] 0.65
```

```r
cross_validate(container,N,"RF")
```

```
## Fold 1 Out of Sample Accuracy = 0.8
## Fold 2 Out of Sample Accuracy = 0.3333
## Fold 3 Out of Sample Accuracy = 0.4
## Fold 4 Out of Sample Accuracy = 0.5
```

```
## [[1]]
## [1] 0.8000 0.3333 0.4000 0.5000
## 
## $meanAccuracy
## [1] 0.5083
```

```r

######################
# excercise
######################
# download data

# load data
setwd("E:/Twitter-Sentimental-Analysis-master/")
happy = readLines("./happy.txt")
sad = readLines("./sad.txt")
happy_test = readLines("./happy_test.txt")
sad_test = readLines("./sad_test.txt")

tweet = c(happy, sad)
tweet_test= c(happy_test, sad_test)
tweet_all = c(tweet, tweet_test)
sentiment = c(rep("happy", length(happy) ), 
              rep("sad", length(sad)))
sentiment_test = c(rep("happy", length(happy_test) ), 
                   rep("sad", length(sad_test)))
sentiment_all = as.factor(c(sentiment, sentiment_test))

#---------------------naive bayes------------------------#

mat= create_matrix(tweet_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

mat = as.matrix(mat)

classifier = naiveBayes(mat[1:160,], as.factor(sentiment_all[1:160]))
predicted = predict(classifier, mat[161:180,]); predicted
```

```
##  [1] sad   happy sad   happy happy sad   happy sad   happy happy sad  
## [12] sad   sad   sad   sad   sad   sad   sad   happy happy
## Levels: happy sad
```

```r

table(sentiment_test, predicted)
```

```
##               predicted
## sentiment_test happy sad
##          happy     6   4
##          sad       2   8
```

```r
recall_accuracy(sentiment_test, predicted)
```

```
## [1] 0.7
```

```r
#---------------------the other methods------------------------#


# the other methods
mat= create_matrix(tweet_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

container = create_container(mat, as.numeric(sentiment_all),
                             trainSize=1:160, testSize=161:180,virgin=FALSE) # can removeSparseTerms

models = train_models(container, algorithms=c("MAXENT",
                                              "SVM",
                                              "SLDA","BAGGING", 
                                              "RF", 
                                              "TREE" 
))

# test the model
results = classify_models(container, models)

table(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])
```

```
##    
##      1  2
##   1 10  0
##   2  1  9
```

```r


recall_accuracy(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])
```

```
## [1] 0.95
```

```r



# Here we also want to get the formal test results, including: 
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND
# ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM analytics@label_summary:
# SUMMARY OF LABEL (e.g. TOPIC) ACCURACY analytics@document_summary: RAW SUMMARY
# OF ALL DATA AND SCORING analytics@ensemble_summary: SUMMARY OF ENSEMBLE
# PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics() Now
# let’s see the results:

# formal results
analytics = create_analytics(container, results)
summary(analytics)
```

```
## ENSEMBLE SUMMARY
## 
##        n-ENSEMBLE COVERAGE n-ENSEMBLE RECALL
## n >= 1                1.00              0.95
## n >= 2                1.00              0.95
## n >= 3                1.00              0.95
## n >= 4                1.00              0.95
## n >= 5                1.00              0.95
## n >= 6                0.95              1.00
## 
## 
## ALGORITHM PERFORMANCE
## 
##        SVM_PRECISION           SVM_RECALL           SVM_FSCORE 
##                0.955                0.950                0.950 
##       SLDA_PRECISION          SLDA_RECALL          SLDA_FSCORE 
##                0.955                0.950                0.950 
##    BAGGING_PRECISION       BAGGING_RECALL       BAGGING_FSCORE 
##                0.955                0.950                0.950 
##    FORESTS_PRECISION       FORESTS_RECALL       FORESTS_FSCORE 
##                0.955                0.950                0.950 
##       TREE_PRECISION          TREE_RECALL          TREE_FSCORE 
##                1.000                1.000                1.000 
## MAXENTROPY_PRECISION    MAXENTROPY_RECALL    MAXENTROPY_FSCORE 
##                0.955                0.950                0.950
```

```r

head(analytics@algorithm_summary)
```

```
##   SVM_PRECISION SVM_RECALL SVM_FSCORE SLDA_PRECISION SLDA_RECALL
## 1          0.91        1.0       0.95           0.91         1.0
## 2          1.00        0.9       0.95           1.00         0.9
##   SLDA_FSCORE BAGGING_PRECISION BAGGING_RECALL BAGGING_FSCORE
## 1        0.95              0.91            1.0           0.95
## 2        0.95              1.00            0.9           0.95
##   FORESTS_PRECISION FORESTS_RECALL FORESTS_FSCORE TREE_PRECISION
## 1              0.91            1.0           0.95              1
## 2              1.00            0.9           0.95              1
##   TREE_RECALL TREE_FSCORE MAXENTROPY_PRECISION MAXENTROPY_RECALL
## 1           1           1                 0.91               1.0
## 2           1           1                 1.00               0.9
##   MAXENTROPY_FSCORE
## 1              0.95
## 2              0.95
```

```r
head(analytics@label_summary)
```

```
##   NUM_MANUALLY_CODED NUM_CONSENSUS_CODED NUM_PROBABILITY_CODED
## 1                 10                  11                    11
## 2                 10                   9                     9
##   PCT_CONSENSUS_CODED PCT_PROBABILITY_CODED PCT_CORRECTLY_CODED_CONSENSUS
## 1                 110                   110                           100
## 2                  90                    90                            90
##   PCT_CORRECTLY_CODED_PROBABILITY
## 1                             100
## 2                              90
```

```r
head(analytics@document_summary)
```

```
##   MAXENTROPY_LABEL MAXENTROPY_PROB SVM_LABEL SVM_PROB SLDA_LABEL SLDA_PROB
## 1                1               1         1   0.9998          1         1
## 2                1               1         1   0.9998          1         1
## 3                1               1         1   0.9870          1         1
## 4                1               1         1   0.9873          1         1
## 5                1               1         1   0.9923          1         1
## 6                1               1         1   0.9723          1         1
##   BAGGING_LABEL BAGGING_PROB FORESTS_LABEL FORESTS_PROB TREE_LABEL
## 1             1            1             1        0.930          1
## 2             1            1             1        0.870          1
## 3             1            1             1        0.905          1
## 4             1            1             1        0.955          1
## 5             1            1             1        0.955          1
## 6             1            1             1        0.750          1
##   TREE_PROB MANUAL_CODE CONSENSUS_CODE CONSENSUS_AGREE CONSENSUS_INCORRECT
## 1         1           1              1               6                   0
## 2         1           1              1               6                   0
## 3         1           1              1               6                   0
## 4         1           1              1               6                   0
## 5         1           1              1               6                   0
## 6         1           1              1               6                   0
##   PROBABILITY_CODE PROBABILITY_INCORRECT
## 1                1                     0
## 2                1                     0
## 3                1                     0
## 4                1                     0
## 5                1                     0
## 6                1                     0
```

```r
analytics@ensemble_summary # Ensemble Agreement
```

```
##        n-ENSEMBLE COVERAGE n-ENSEMBLE RECALL
## n >= 1                1.00              0.95
## n >= 2                1.00              0.95
## n >= 3                1.00              0.95
## n >= 4                1.00              0.95
## n >= 5                1.00              0.95
## n >= 6                0.95              1.00
```

```r


# Cross Validation
N=3
cross_SVM = cross_validate(container,N,"SVM")
```

```
## Fold 1 Out of Sample Accuracy = 0.9595
## Fold 2 Out of Sample Accuracy = 0.9615
## Fold 3 Out of Sample Accuracy = 0.9259
```

```r
cross_GLMNET = cross_validate(container,N,"GLMNET")
```

```
## Fold 1 Out of Sample Accuracy = 1.154
## Fold 2 Out of Sample Accuracy = 0.7
## Fold 3 Out of Sample Accuracy = 1
```

```r
cross_MAXENT = cross_validate(container,N,"MAXENT")

```


