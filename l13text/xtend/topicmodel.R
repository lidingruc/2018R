#!/usr/bin/Rscript
# https://github.com/nickbadams/D-Lab_TextAnalysisWorkingGroup/blob/7f90990342aff714179f88af0662f8850b18bc4a/tutorial1.R
library(topicmodels)
library(tm)
#### instead of using setwd(), start the script by changing to the correct directory first
## setwd("/home/My Documents/D-Lab/textanalysis_workshop/R/")
newspaper_text <-read.csv("ceos_newyorktimes_data.csv", header=TRUE) #read in CSV file
m <- list(Content="TEXT")
myReader <- readTabular(mapping=m)
(a <- Corpus(DataframeSource(newspaper_text), readerControl = list(reader = myReader))) #create corpus containing the text column, on line per document

######Alternative way to create corpus if you have separate documents. Put them all in one folder, then point to that directory:
## a <- Corpus(DirSource("/home/My Documents/D-Lab/textanalysis_workshop/R/ceos_newyorktimes_files"))
########

summary(a) #should tell you how many documents are in your corpus
a[[1]] #check the content of the first document in the corpus
a <- tm_map(a, tolower) # convert all text to lower case
a <- tm_map(a, removePunctuation) 
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removeWords, stopwords("english"))

library(SnowballC) # needed for stemming function
a <- tm_map(a, stemDocument, language = "english") # converts terms to tokens
a.dtm <- TermDocumentMatrix(a, control=list(minDocFreq = 2)) #convert to term document matrix, words have to be in at least minDocFreq to appear, I set it to 2, but you can change this.
inspect(a.dtm[1:10,1:10]) # have a quick look at the term document matrix
findFreqTerms(a.dtm, lowfreq=100) # have a look at common words
nrow(a.dtm) #number of unique words in corpus
#findAssocs(a.dtm, 'women', 0.30) # find associated words and strength of the common words.
a.dtm.sp <- removeSparseTerms(a.dtm, sparse=0.987) #remove sparse terms, maybe not necessary, sometimes is. Play around with this if you're not getting good output.
a.dtm.sp.df <- as.data.frame(inspect(a.dtm.sp)) # convert document term matrix to data frame
nrow(a.dtm.sp.df) # check to see how many words we're left

#####Alternative: use tf-idf scores to choose which words to keep

require(slam)
a.dtm.sp.t <- t(a.dtm.sp) # transpose document term matrix
summary(col_sums(a.dtm.sp.t)) # check median...
term_tfidf <- tapply(a.dtm.sp.t$v/row_sums(a.dtm.sp.t)[a.dtm.sp.t$i], a.dtm.sp.t$j,mean) * log2(nDocs(a.dtm.sp.t)/col_sums(a.dtm.sp.t>0)) # calculate tf-idf values
summary(term_tfidf) # check median... note value for next line... 
a.dtm.sp.t.tdif <- a.dtm.sp.t[,term_tfidf>=0.014] # keep only those terms that are slightly less frequent that the median
a.dtm.sp.t.tdif <- a.dtm.sp.t[row_sums(a.dtm.sp.t) > 0, ]
summary(col_sums(a.dtm.sp.t.tdif)) # have a look
ncol(a.dtm.sp.t.tdif) #number of terms you're left with

######

require(topicmodels)

a.dtm.t <- t(a.dtm) #try with all of the words, not removing sparse terms

#Now we produce the model

lda10 <- LDA(a.dtm.sp.t.tdif,10) # generate a LDA model with 10 topics
## lda5 <- LDA(a.dtm.sp.t.tdif,5) # generate a LDA model with 10 topics
## lda50 <- LDA(a.dtm.sp.t.tdif,50) # generate a LDA model with 10 topics
get_terms(lda10, 10) # get keywords for each topic, just for a quick look
get_topics(lda10, 5) # gets topic numbers per document
lda_topics<-get_topics(lda10, 5) #creat object with top 5 topics per document
beta <- lda10@beta # create object containing parameters of the word distribution for each topic
gamma <- lda10@gamma # create object containing posterior topic distribution for each document
terms <- lda10@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(beta) <- terms # puts the terms (or words) as the column names for the topic weights.
id <- t(apply(beta, 1, order)) # order the beta values
beta_ranked <- lapply(1:nrow(id), function(i) beta[i,id[i,]])  # gives table of words per topic with words ranked in order of
beta_ranked
gamma
terms
#######################
#Put output into various csv files

top50 <- lapply(beta_ranked, function (el1) {el1[3484:3534]}) #change based on the number of rows you have

top50t <- t(top50)

top50t
lda_topics<-get_topics(lda10, 5) 

lda_topics <- t(lda_topics)
write.csv(lda_topics, file = "lda10_topics.csv")

lda_terms <- get_terms(lda10, 50)
write.csv(lda_terms, file = "lda10_terms.csv") 

###################
#Output terms with weights, by topic (top 50 terms)
term_list <- lapply(beta_ranked,function(x) exp(x[3500:3534]))
term_list <- lapply(term_list,function(x) cbind(names(x),x))
output <- c()
for (i in 1:length(term_list)){
  output <- cbind(output,term_list[[i]])
}
write.csv(output, file = "lda10_terms_weight.csv", row.names=F, col.names=F)