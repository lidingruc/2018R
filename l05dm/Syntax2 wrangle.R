# -------------------------------------------
# Data Wrangling in R
# Last Updated: November 2016
# NYU Data Services: data.services@nyu.edu
# http://guides.nyu.edu/r
# -------------------------------------------

# -------------------------------------------
# -- I. Brief Overview of Data Types  --
# -------------------------------------------

# Remove every objects in your working environment 
rm(list = ls())
# remove.packages()

#  Vectors 
v1 <- rnorm(100, 75, 15)
v2 <- as.factor(rep(c("A", "B", "C", "D"), times = 25))
v3 <- rnorm(100, 1, .5) 

# Vector Indices
v2[1:10]
v2[c(4, 8, 12)]

# Data Frames 
mydata <- data.frame(v1, v2, v3)
mydata$v1
mydata[1:10, c("v1", "v2")]

# Data Type Conversion
# as.numeric() 
# as.character()
# as.vector() 
# as.matrix() 
# as.data.frame()
# as.factor()


# -------------------------------------------
# -- II. Data Management  --
# -------------------------------------------

# A. Working with Strings
dna_ex <- "ACAAAGATGCCATTGTCCCCCGGCCTCCTGCTGCTGCTGCTCTCCGGGGCCACGGCCACCGCTGCCCTGCCCCTGGAGGGTGGCCCCACCGGCCGAGACAGCGAGCATATGCAGGAAGCGGCAGGAATAAGGAAAAGCAGCCTCCTGACTTTCCTCGCTTGGTGGTTTGAGTGGACCTCCCAGGCCAGTGCCGGGCCCCTCATAGGAGAGGAAGCTCGGGAGGTGGCCAGGCGGCAGGAAGGCGCACCCCCCCAGCAATCCGCGCGCCGGGACAGAATGCCCTGCAGGAACTTCTTCTGGAAGACCTTCTCCTCCTGCAAATAAAACCTCACCCATGAATGCTCACGCAAGTTTAATTACAGACCTGAAGGTCC"

# Counting Characters
length(dna_ex)
nchar(dna_ex)

# Splitting Strings
sp_dna <- strsplit(dna_ex, "") 
class(sp_dna)
table(sp_dna)

strsplit(dna_ex, "A")

substr(dna_ex, 20, 30)

# Matching Strings
grep('AA', dna_ex, value = TRUE)
regexpr('AA', dna_ex)
gregexpr('AA', dna_ex)

install.packages('stringr')
library(stringr)

str_locate_all(dna_ex, 'AA') # stringr package, str_extract_all


install.packages('stringi')
library(stringi)

stri_count_fixed(dna_ex, 'AA')

fruits <- c('Apple', 'Banana', 'Orange', 'Grape', 'Pineapple', 'Kiwi', 'Peach', 'Mango', 'Strawberry', 'Guava', 'Cherry')
grep('a', fruits, value = TRUE, ignore.case = TRUE)
positions_a <- gregexpr(pattern = "a", text = fruits, ignore.case = TRUE)
num_a <- sapply(positions_a, function(x) ifelse(x[1] > 0, length(x), 0))

# Paste
paste("X", 1:5, sep = ".")
paste("X", 1:5, sep = ".", collapse = "")
paste0("X", 1:5, sep = ".")# paste0("X", 1:5) is same as paste("X", 1:5, sep = "")

stri_paste(rep(c("A","C","G","T"),each=4),c("A","C","G","T"), collapse = "")


# Other manipulation
string1 <- 'NYU Data Services'
tolower(string1) 
toupper(string1)

string2 <- toString(c(1,3,4))
string2

# -----------------------------------------------------------------------------
# -- Exercise with Strings
# --- a. Create this string 'A&1B&2C&3' using a paste function
# --- b. Create this string 'A&1&4B&2&5C&3&6' using a paste function
# --- c. Find the length of the string created in part b
# --- d. Count the number of '&' in the string created in part b
# -----------------------------------------------------------------------------  


# B. Working with Dates

dates <- c('11/28/2011', '12/07/2012', '08/01/2013', '02/09/2015')
class(dates)
real_dates <- as.Date(dates, format = '%m/%d/%Y')
class(real_dates)

other_format <- format(real_dates, '%A %B %d, %Y')
class(other_format)


today <- Sys.Date()

dif <- today - real_dates
class(dif)

# -----------------------------------------------------------------------------
# -- Exercise with Dates
# a. Take the following dates (November 11, 2011) and turn them into dates in R
# b. Display all of the dates in the format (month.day.year')
# -----------------------------------------------------------------------------
date_ex1 <- 'Friday November 11, 2011'
date_ex2 <- 'Fri Nov 11 11 '
date_ex3 <- '11-11-11' # (day-month-year)


# UScereal is the dataset inside the MASS package
library(MASS)
data(UScereal)
head(UScereal)
      
# C. Selecting Cases 
which(UScereal$mfr == 'K')  # reports indicies       
which(UScereal$calories > 250)

# Use indices to select rows w/ selected columns                                              
UScereal[which(UScereal$mfr == 'K'), c('mfr', 'calories')]

# subset function 
subset(UScereal, calories > 250, c('mfr', 'calories'))

# with dplyr, using the %>% operator
# select rows
install.packages('dplyr')
library(dplyr)
UScereal %>% filter(calories > 250)

# select columns 
UScereal %>% select(mfr, calories)

# select rows and columns 
UScereal %>% filter(calories > 250) %>% select(mfr, calories)

# D. Sorting Data 
sort(UScereal$calories)  
sort(UScereal)  # sort() only works for vectors

order(UScereal$calories)  
UScereal[order(UScereal$calories), c('mfr', 'calories')] 

UScereal[order(UScereal$mfr, -UScereal$calories), ]

# with dplyr
library(dplyr)
UScereal %>% arrange(mfr, desc(calories)) 


# -----------------------------------------------------------------------------
# -- Exercise 1 
# --- Create a subset of mydata, which contains the 25 highest v1 scores
# -----------------------------------------------------------------------------  



# E. Reshape 
# Make a Panel Dataset
health <- data.frame(id = rep(1:10, each = 4, len = 40),
                     trial = rep(c(1:4), 10), 
                     score = rnorm(40, 3, .75))
health[1:5, ]

# Reshape : Long --> Wide
health_wide <- reshape(health, v.names = "score", idvar = "id", 
                       timevar = "trial", direction = "wide") 
health_wide[1:5, ]
head(reshape(health_wide)) # to go back to long format

# Reshape : Wide --> Long
health_long <- reshape(health_wide, idvar = "id", direction = "long")         
health_long[1:5, ] 

# Using the tidyr package
# install.packages('tidyr')
library(tidyr)
spread(health, key = trial, value = score) # key is the identifier
gather(health_wide, key = trial, value = score, score.1:score.4) # can also reference by column number(2:5)


# -----------------------------------------------------------------------------
# -- Exercise 2 
# --- Determine if the dataset below (exercise2) is long or wide, and reshape 
# --- the dataset using one of the methods above
# -----------------------------------------------------------------------------
# setwd to the Data Management folder
setwd("/Users/liding/E/Bdata/Course/2R/nyu/") 
exercise2 <- read.csv("Exercise 2.csv")



# F. Merge Datasets 
# Create two data with common variables
data1 <- data.frame(id = rep(1:5, 3), year = rep(2000:2002, each = 5), 
                     group = sample(c("A", "B", "C"), 15, replace = TRUE))

data2 <- data.frame(id = rep(1:5, each = 4), year = rep(2000:2003, 5),
                    score = rnorm(20, 50, 15)) 

data1
data2


# Merge them by id & year, 1:1 merge
data_merge <- merge(data1, data2, by = c("id", "year")) 
data_merge

# Extra rows from both datasets are added.
data_merge <- merge(data1, data2, by = c("id", "year"), all = TRUE) 
data_merge

# left-merge: x.all=TRUE. right-merge:y.all=TRUE

browseURL("http://guides.nyu.edu/quant/merge")

# dplyr package
library(dplyr)
inner_merge <- data1 %>% inner_join(data2, by = c("id", "year"))
outer_merge <- data1 %>% full_join(data2, by = c("id", "year"))

# -----------------------------------------------------------------------------
# -- Exercise 3 
# --- Merge the reshaped dataset from Ex.2, and the exercise_3 dataset below
# -----------------------------------------------------------------------------
exercise_3 <- read.csv("Exercise 3.csv")



# G. Apply function and Aggregate Statistics 
# Apply mean() across numeric nutrition variables in the UScereal dataset     
apply(UScereal[, c(2:8, 9)], MARGIN = 1, FUN = mean)  
apply(UScereal[, c(2:8, 9)], MARGIN = 2, FUN = mean)

# Apply sd() across numeric nutrition variables in the UScereal dataset      
lapply(UScereal[, c(2:8, 9)], sd)
sapply(UScereal[, c(2:8, 9)], sd)

# tapply() instead of by factors : return an array.
tapply(UScereal$calories, UScereal$mfr, summary)

# Summary statistics of 'cty' grouped by 'trans'            
by(UScereal$calories, UScereal$mfr, summary)
by(UScereal$calories, list(UScereal$mfr, UScereal$shelf), summary) 

# dplyr 
library(dplyr)
UScereal %>% group_by(mfr) %>% summarize(avg.cal = mean(calories))

UScereal %>% group_by(mfr) %>% summarize(avg.cal = mean(calories), count = n())

UScereal %>% group_by(mfr) %>% mutate(avg.cal = mean(calories), count = n())

# -----------------------------------------------------------------------------
# -- Exercise 4 
# --- Using one of the methods above, find the average Before & After Score 
# --- for each gender and then each state
# -----------------------------------------------------------------------------



# -------------------------------------------
# -- III. Functions  --
# -------------------------------------------

#  <function.name> <- function(arg1, arg2, ...) {
#                     function_body
#                     return(any_value_to_return)
#                   }

# Create your own functions
addTwoNums <- function(a, b) {
  tmp <- a + b
  return(tmp)
  # Alternatively either of the below would substitue for the above
  # return(a + b)
  # a + b
}

addTwoNums(2, 1)
addTwoNums(5)  # Does it work? 
  
addTwoNums <- function(a, b = 2) {
  return(a + b)
}

addTwoNums(5)
addTwoNums(a = c(4, 10, 0))  # Multiple arguments at the same time
addTwoNums(3, 4) # How does it work?

# Multiple results to report 
myOperations <- function(a, b) {
  add <- a + b
  subtract <- a - b
  multiply <- a * b
  divide <- a / b
  mylist <- list(add, subtract, multiply, divide)
  return(mylist)
}

myOperations(5, 10)


# -----------------------------------------------------------------------------
# -- Exercise 5 
# --- Create a function which takes the difference (After-Before) of the
# --- merged data set (ex. 4) and reports the mean difference
# -----------------------------------------------------------------------------


 
# A more complicated example
# Create your own t-test function
my_ttest <- function(x, mu = 0, test = "two-sided", alpha = 0.05) {
  n <- length(x)
  df <- n - 1
  std <- sqrt(var(x))
  t <- sqrt(n) * (mean(x) - mu) / std  
  tail_area <- switch(test, "two-sided" = 2 * (1 - pt(abs(t), n - 1)),
                      lower = pt(abs(t), df), upper = 1 - pt(abs(t), df))
  list(t.statistics = t, degree.freedom = df, p.value = tail_area)
}

my_ttest(v1)

switch(1, c("one", "two"),c("three", "four"))
switch(2, c("one", "two"),c("three", "four"))

# -------------------------------------------
# -- IV. If/Else Statements  --
# -------------------------------------------

# If Statement  - Typically used inside Functions & Loops
x <- 10
if (x > 10) {
  print ("Greater than 10")
} else if (x == 10) {
  print ("Equal to 10")
} else if (x < 10 && x >= 0) {
  print ("Between 0 and 10")
} else {
  print ("Less than 0")
}

# If-Else Statement 
# ifelse(test, action if yes, action if no)
x <- seq(1:4)
ifelse(x < 4, "less than 4", "more than equal to 4")


# -----------------------
# -- V. For Loop  --
# -----------------------

for (i in c(1, 2, 4, 5))  {
  out <- i + 1
  print(out)
}

univ <- c("Yale", "Harvard", "Pittsburg", "NYU", "Rutgers")
for (i in univ) {
  print(i)
}


# -----------------------------------------------------------------------------
# -- Exercise 6 
# --- Create a for loop which prints out the participants who improved their 
# --- score by at least 5 points
# -----------------------------------------------------------------------------



# A more complicated example
# Merging multiple datasets using for loop & if-else statement
# setwd to the data folder inside the Data Management folder
setwd("/Users/katieanderson/Desktop/Data Management in R/data") 

for(data_file in list.files()[grep(".csv$", list.files())]) {
  temp <- read.csv(file = data_file)  # Open file
	temp$X <- NULL # Get rid of the variable named "X"
	temp$avg_var1_country_year <- ave(temp$var1, temp$country, FUN = mean)  # Compute the average of var1 by country for this year
	# Add rows to data set if it exists, if not, create the dataset to get started
	if(exists("merged_data")) {
	  merged_data <- rbind(merged_data, temp)
  } else {
	  merged_data <- temp # first dataset
  }
}

# Merge the multiple datasets using apply() functions this time
# Step1: Create a function to open and clean a data set
clean_dat <- function(filename) {
  dat <- read.csv(file = filename)
  dat$X <- NULL # get rid of the variable named "X"
  dat$avg_var1_country_year <- ave(dat$var1, dat$country, FUN = mean)  # Compute the average of var1 by country for this year
  return(dat)
}
    
# Step2: Create a list containing all clean datasets
dat_list <- lapply(list.files()[grep(".csv$", list.files())], clean_dat)  

# Step3: Merge all datasets
merged_data_2 <- do.call(rbind, dat_list)

# Both approaches returned the same results                      
all.equal(merged_data, merged_data_2)


# -----------------------
# -- VI. While Loop  --
# ----------------------- 

i <- 1
while (i <= 5) {
  i <- i + 2
  print(i)
}

univ <- c("Yale", "Harvard", "Pittsburg", "NYU", "Rutgers")
i <- 1
while(univ[i] != "Rutgers") {
  print(univ[i])
  i <- i + 1
}

# Create a fuction with a while loop 
univ_while <- function(x) {
  i <- 1
  while(x[i] != "NYU") {
    print(x[i])
    i <- i + 1
  }
}

univ_while(univ)

# Create a function with while loop
f6 <- function(x) {
  i <- 0
  while (i < x) {
    i <- i + 2
    y <- i * 2
    print(y)
  }
  return(y * 2)
}

# Graph a function 
f <- function(x) (x ^ 3 - 13)
  plot(-5:6, f(-5:6), type = "l", ylab = "f(x) = x ^ 3 - 13", xlab = "")

# Bisection Method for f(x) = x ^ 3 - 13
a = -5
b = 6
c = (a + b) / 2
i = 1
while (abs(f(c) - 0) > 10 ^ -5) {
  if (sign(f(c)) == sign(f(a))) {
    a <- c
  } else {
    b <- c
  }
  c <- (a + b) / 2
  print(c)
  i <- i + 1
}


# -----------------------
# -- VII. Repeat Loop  --
# -----------------------

i <- 2
repeat {
  print(i)
  i <- i + 2
  if (i > 6) break
}

univ <- c("Yale", "Harvard", "Pittsburg", "NYU", "Rutgers")   
f_repeat <- function(x) {
  i <- 1
  repeat { 
  print(x[i])
  i <- i + 1
  if (x[i] == "NYU") break
  }
}

f_repeat(univ)


# -----------------------
# -- Evaluation  --
# -----------------------
# Please help us improve this tutorial and others by taking the survey below:
# Just run the line below to open the evaluation in your browser, or copy this link http://bit.ly/ManagementR
browseURL("http://bit.ly/ManagementR")




# -----------------------------------------------------------------------------
# -- Exercise Solutions  --
# Of course there is always more than 1 solution, below are just examples
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# -- Exercise with Strings
# --- a. Create this string 'A&1B&2C&3' using a paste function
# --- b. Create this string 'A&1&4B&2&5C&3&6' using a paste function
# --- c. Find the length of the string created in part b
# --- d. Count the number of '&' in the string created in part b
# -----------------------------------------------------------------------------  
paste(c('A', 'B', 'C'), 1:3, sep = '&', collapse = '')
mystring <- paste(c('A', 'B', 'C'), 1:3, 4:6, sep = '&', collapse = '')
nchar(mystring)
stri_count_fixed(mystring, '&')

# -----------------------------------------------------------------------------
# -- Exercise with Dates
# a. Take the following dates (November 11, 2011) and turn them into dates in R
# b. Display all of the dates in the format (month.day.year')
# -----------------------------------------------------------------------------
date_ex1 <- 'Friday November 11, 2011'
date_ex2 <- 'Fri Nov 11 11 '
date_ex3 <- '11-11-11' # (day-month-year)

a1 <- as.Date(date_ex1, '%A %B %d, %Y')
a2 <- as.Date(date_ex2, '%a %b %d %y')
a3 <- as.Date(date_ex3, '%d-%m-%y')

format(c(a1, a2, a3), '%m.%d.%y')

# -----------------------------------------------------------------------------
# -- Exercise 1 
# --- Create a subset of mydata, which contains the 25 highest v1 scores
# -----------------------------------------------------------------------------  
sorted <- mydata[order(-mydata$v1), ][1:25, ]

fullsort<-mydata %>% arrange(desc(v1))
fullsort[1:25, ]
# -----------------------------------------------------------------------------
# -- Exercise 2 
# --- Determine if the dataset below (exercise_2) is long or wide, and reshape 
# --- the dataset using one of the methods above
# -----------------------------------------------------------------------------
exercise_2 <- read.csv("Exercise2.csv")

spread(exercise_2, key = Treatment, value = Result)

reshape(exercise_2, v.names = "Result", idvar = "Participant",
        timevar = "Treatment", direction = "wide")

# -----------------------------------------------------------------------------
# -- Exercise 3 
# --- Merge the reshaped dataset from Ex.2, and the exercise_3 dataset below
# -----------------------------------------------------------------------------
exercise_3 <- read.csv("Exercise 3.csv")
wide <- dcast(exercise_2, Participant ~ Treatment, value.var = "Result")
merged <- merge(exercise_3, wide, by = "Participant")


# -----------------------------------------------------------------------------
# -- Exercise 4 
# --- Using one of the methods above, find the average Before & After Score 
# --- for each Gender and then each State
# ----------------------------------------------------------------------------- 
by(merged$Before, merged$Sex, mean)
by(merged$After, merged$Sex, mean)
# OR
aggregate(merged[, c("Before", "After")], by = list(merged$State), FUN = mean)
# OR
apply(merged[which(merged$Sex == "Male"), c("Before", "After")], 2, mean)


# -----------------------------------------------------------------------------
# -- Exercise 5 
# --- Create a function which takes the difference (After-Before) of the
# --- merged data set (ex. 4) and reports the mean difference
# -----------------------------------------------------------------------------
fn_mean_dif <- function(low_bound, up_bound) {
  diff <- up_bound - low_bound
  return(mean(diff))
}    

fn.mean.dif(merged$Before, merged$After)


# -----------------------------------------------------------------------------
# -- Exercise 6 
# --- Create a for loop which prints out the participants who improved their 
# --- score by at least 5 points
# -----------------------------------------------------------------------------
for(i in 1:nrow(merged)) {
  if ((merged$After[i] - merged$Before[i]) >= 5) {
    print(merged[i, ])
  }
}

