# model answer with credit to SA Reade, class of 2014.

# install.packages("party")

rm(list = ls())

library(party)

# Load seeds dataset

seeds <- read.csv2(file="seeds_dataset.txt", sep=";", dec=".", header=TRUE)
class(seeds$variety)
  
seeds$variety <- factor(seeds$variety,labels=c("Kama", "Rosa" , "Canadian") )
class(seeds$variety)

target <- variety ~ .
cdt <- ctree(target, seeds)
plot(cdt,type="simple")

# use the tree to make predictions on the data
predict(cdt)

# calculate sum of correct matching variety
correct <- sum(seeds$variety==predict(cdt))

# calculate success rate of predicted variety
success <- correct/nrow(seeds)*100
print(paste("Party success rate on full data set:", round(success,2), "%"))

# in tabular form
table(predict(cdt), seeds$variety)


# now lets create a routine for splitting the data into train and test sets
seedtrain <- function(trainsize)
{  
  # print("===================================================")
  train <- c(sample(70,trainsize), 70 + sample(70,trainsize), 140 + sample(70,trainsize))
  seeds_train <- seeds[train,]
  seeds_test <- seeds[-train,]
  target <- variety ~ .
  cdt <- ctree(target, seeds_train)
  
  seeds_train$predicted <-predict(cdt,newdata=seeds_test)
  correct <- sum(seeds_train$variety==predict(cdt,newdata=seeds_test))
  success <- 100 * correct/nrow(seeds_train)
  return(success)
}


seedtrain(35)




################Seeds Training################

# Split dataset into training and testing subsets.
trainsplit <- 35

# Run training 1000 times
successRates <- sapply(rep(trainsplit,1000), seedtrain)

# Decision tree successes
summary(successRates)

# Decision tree success average
print(paste("Party split average success rate:", round(mean(successRates),2), "%"))

# plot the distribution of successes
hist(successRates)
