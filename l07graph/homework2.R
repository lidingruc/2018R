###################################################
# Exercise 2a. 
# How can we improve this graph? From what we have learned about R basic graph command, implement at least 3 improvements to this graph.
library(ggplot2)
data(mpg)
head(mpg)
plot(cty~hwy, data = mpg)

# Exercise 2b.
# How can we improve this graph? From what we have learned about R basic graph command, implement at least 3 improvements to this graph.
library(nycflights13)
data(weather)
head(weather)
boxplot(temp ~ month, data = weather)


###################################################
# Exercise 2c. Use ggplot2 to improve 2a图

# Exercise 2d. Use ggplot2 to improve 2b图



