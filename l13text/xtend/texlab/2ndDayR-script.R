
1+4

"hello world"


#articles categorized by gender of CEO of company mentioned

install.packages("tm")
library(tm)

setwd("~/D-Lab--Text-Analysis-Workshop")

a <- Corpus(DirSource("diff_nyt")) #the directory should have two text files in it, the files you want to compare
summary(a)
head(a)
inspect(a)

#as.character(a)

?inspect

#for use in different version of R
#a <- tm_map(a, tolower) # convert all text to lower case
#a <- tm_map(a, removePunctuation) 
#a <- tm_map(a, removeNumbers)
#a <- tm_map(a, removeWords, stopwords("english"))

a <- tm_map(a, content_transformer(tolower)) # convert all text to lower case
a <- tm_map(a, content_transformer(removePunctuation)) 
a <- tm_map(a, content_transformer(removeNumbers))
a <- tm_map(a, content_transformer(removeWords), stopwords("english"))
?tm_map

##############
#confusion, here, re: term-document vs. document term

install.packages("SnowballC")
library(SnowballC) # needed for stemming function
a <- tm_map(a, stemDocument, language = "english") # converts words to stemmed terms
a.tdm <- TermDocumentMatrix(a) #puts tokens into a term document matrix
?TermDocumentMatrix
inspect(a.tdm[1:7,1:2]) # have a quick look at the term document matrix -- fiddle with those numbers, too to see what happens
findFreqTerms(a.tdm, lowfreq=150) # have a look at common words -- fiddle with those numbers, too
nrow(a.tdm) #gives you the number of unique words
ncol(a.tdm)
a.tdm.sp <- removeSparseTerms(a.tdm, sparse=0.4) #remove sparse terms, the sparse number should be higher with a large number of documents, smaller with small number of documents, always less than 1
?removeSparseTerms
nrow(a.tdm.sp) #compare the number of unique words after removing sparse terms
a.tdm.sp.df <- as.data.frame(inspect(a.tdm.sp)) # convert document term matrix to data frame
View(a.tdm.sp.df)
nrow(a.tdm.sp.df) # check to see how many words are left
a.dtm.sp.df <- t(a.tdm.sp.df) #transpose matrix
ncol(a.dtm.sp.df)

View(a.dtm.sp.df)
###########
rowTotals <- apply(a.dtm.sp.df, 1, sum) #creates a column with row totals, total number of words per document -- the '1' indicates that the new addition should 'sum' the rows, not hte columns
?apply
head(rowTotals)
View(rowTotals)
prop <- a.dtm.sp.df/rowTotals #change frequencies to proportions
View(prop)
prop <- t(prop) #do we remember what the function 't' does?
View(prop)

#nick, don't call data frames "data". that is a poor practice.
data <- as.data.frame(prop)
View(data)  

View(data)
data$diff <- (data$men_nyt.txt - data$women_nyt.txt) #creates new column (diff) which is the difference between columns 1 and 2, need to change the names to fit your file names

sorted <- data[with(data,order(data$diff)),] #sorts data by column diff
row.names(sorted)[1:40] #prints first row names, the top 40 words define what makes articles on men CEOs most distinctive
row.names(sorted)[2743:2782] #prints last row names, change the numbers to be the last 40 rows, the top 40 words definine your first file

#red <- sapply(sorted, function(x) `*`(x[1:40], 1000))
first_rows <- cbind(row.names(sorted)[1:40], sorted[1:40,3]) #prints last row names
last_rows <- cbind(row.names(sorted)[2741:2782], sorted[2741:2782,3]) #prints last row names, change the numbers to fit your matrix

#write out for publication
write.csv(first_rows, file = "diffprop_men_terms.csv") #writes csv file with the first rows and weights
write.csv(last_rows, file = "diffprop_women_terms.csv") #ditto with the last rows





