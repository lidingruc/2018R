##########################################################
# Examples of random sampling from data
# Copy and paste into the R console window to see how the
# commands work. 

# ( Note: "#" starts a comment. anything to the right of a "#" is
# not executed in R).


###################### Sample from a population marked with 1's and 0's

x=c(rep(0,5000),rep(1,5000))   #create a population with 5000 0's and 5000 1's.
sample(x,10)                   #takes a random sample of size 10 from the population.
mean(sample(x,10))             #gives the mean of a random sample of size 10
y=c(mean(sample(x,10)))        #stores the mean of a random sample of size 10
y=c(y,mean(sample(x,10)))      #adds the mean of a random sample of size 10 to the previous stored data


#the following short program will add the mean of 1000 random samples of size ten to the previous store data

for(i in 1:1000){              #this line tells the program to run 1000 times
y=c(y,mean(sample(x,10)))      #adds the mean of a random sample of size 10 to the previous stored data
}

hist(y)                        #makes a histogram of the stored means of the random samples. 



######################## sample from a simulated Normal population

z=rnorm(200,50,10)             #create population of 200 values from the Normal distribution
                               #with mean=50 and standard deviation=10
mean(z)                        #display the mean of the "population"

zsamp=sample(z,16)             #takes a sample of 16 of the values
zsamp                          #display the sample
mean(zsamp)                    #mean of the sample

zmeans=NULL                    #intialize vector to store results from repeated sampling
for(i in 1:100){               #run the sampling 100 times and save the results
zmeans=c(zmeans,mean(sample(z,16)))
}
mean(zmeans)
sd(zmeans)
x11()
hist(zmeans)

# Notice that the sd for the sample means is about 2.5 = 10/4
# This illustrates the square root rule, that increasing the sample size
# by a factor of n decreases the sd of the sample mean by a factor of sqrt(n)



####################### sample from a list based on id numbers

# create some fake data
testdata = 
data.frame(id=seq(12), 
           vals=c(83,22,87,55,60,97,81,79,100,83,94,43))
testdata

attach(testdata)

# sample 3 observations based on the id numbers

idsamp=sample(id,3)
idsamp

#corresponding values

vals[idsamp]

# display sampled data

testdata[idsamp,]



########################### enter the data into a vector

data = c(250,300,410,330,175)

data

# can use built in functions
mean(data)
sd(data)
median(data)
IQR(data)



# sample without replacement from the data vector 
# and compute the sample mean


rdat = sample(data,2)
rdat
mean(rdat)




################################### sampling id numbers versus sampling from data

idnum = c(1,2,3,4,5)

value = c(300,250,450,200,150)

sample(idnum,3)

ids = sample(idnum,3)
ids


value[1]
value[ids]   # the values for the sample of id numbers



# 0-1 data (binomial sampling model)

bindat = c(0,0,0,1,1)   # like a box with two "winning" tickets

sample(bindat,10,replace=TRUE)     # sampling with replacement

sum(sample(bindat,10,replace=TRUE))    # sum of the random sample

mean(sample(bindat,10,replace=TRUE))   # sample proportion (why?)



# sum is a Binomial random variable   B(10, 0.4)   
# theoretical mean (mu) is 10*0.4=4
# theoretical sd = sqrt(10*0.4*0.6)
