# -------------------------------------------
# Creating Graphics with R
# Last Updated: September 18, 2015
# NYU Data Services: data.services@nyu.edu
#http://guides.nyu.edu/r
# -------------------------------------------


# -----------------------
# - I. Base Graphics -
# -----------------------

library(MASS)
data(UScereal)
head(UScereal)


# A. Choosing the Right Graph for your Data

# One Continuous Variable

# Q: What is the Distribution of Variable X?
# Q: Is my Variable X normally distributed? Bimodal? Skewed?
# Q: Are there any outliers in my variable?

# Histogram
hist(UScereal$calories, breaks = 15)

# Boxplot
boxplot(UScereal$calories, horizontal = TRUE)



# One Categorical (Discrete) Variable

# Q: Is the distribution across categories of Variable X evenly distributed?

barplot(table(UScereal$shelf))



# Two Continuous Variables

# Q: Is there a relationship between Variable X and Variable Y?
# Q: If there is a relationship, is it linear? Quadratic? 

plot(x = UScereal$sugars, y = UScereal$calories)

plot(calories ~ sugars, data = UScereal) # formula notation

plot(UScereal[, c(2:8, 10)]) # scatterplot matrix



# One Continuous Variable and One Categorical Variable

# Q: Is the distribution of Variable X, different across categories of Variable Y?

boxplot(sugars ~ shelf, data = UScereal, horizontal = TRUE)



# Two Continuous Variables and One Categorical Variable

# Q: Is the relationship between Variable X and Y different across categories of Variable Z?

plot(calories ~ sugars, data = UScereal, col = shelf)
legend('topright', inset = .05, legend = c(3,2,1),
       fill = c('green', 'red', 'black'))



# B. Graphing Elements

# Titles

# Tip: We want titles to meaningful, non-repetitive and include units when applicable

# Use the ylab, xlab and main arguments within the plot command
plot(calories ~ sugars, data = UScereal, ylab = 'Calories',
     xlab = 'Sugars (grams)', main = 'Nutrition of a Single Cup of Cereal')


# Use the title function after the plot has been generated
# If using the title function, the ann argument inside plot must be set to FALSE
plot(calories ~ sugars, data = UScereal, ann = FALSE)
title(main = 'Nutrition of a Single Cup of Cereal', ylab = 'Calories',
      xlab = 'Sugars (grams)') # add afterwards



# Legend

# Use the legend function after the plot has been generated
plot(calories ~ sugars, data = UScereal, col = shelf)
legend('topright', inset = .05, legend = c(3,2,1),
       fill = c('green', 'red', 'black'))



# Point Shape and Color 

# Tip: Changing color or shape of points can be used to represent the same dimension

# The pch argument changes the shape of the points
plot(calories ~ sugars, data = UScereal, pch = 15)


# Set a color to a factor variable, and R will use default colors
plot(calories ~ sugars, data = UScereal, pch = 19, col = shelf, bg = shelf)
legend('topright', inset = .05, legend = c(3,2,1),
       fill = c('green', 'red', 'black'))


# Use a palette of defined colors
palette(c('#e5f5f9', '#99d8c9', '#2ca25f'))
plot(calories ~ sugars, data = UScereal, pch = 19, col = shelf, bg = shelf)
legend('topright', inset = .05, legend = c(3,2,1),
       fill = c('#e5f5f9', '#99d8c9', '#2ca25f'))



# Label all points

# Label points with the text function
plot(calories ~ sugars, data = UScereal, pch = 15)
text(UScereal$sugars, UScereal$calories, row.names(UScereal),
     col = "red", pos = 1, cex = .5)

# The pos argument in the text funciton changes the position of the labels
plot(calories ~ sugars, data = UScereal, pch = 15)
text(UScereal$sugars, UScereal$calories, UScereal$mfr, col = "blue", pos = 2)



# Identify Outliers

# 1. Create a subset of datas which are the outliers, and then use that subset to label
plot(calories ~ sugars, data = UScereal, pch = 19)
outliers <- UScereal[which(UScereal$calories > 300),]
text(outliers$sugars, outliers$calories - 15, labels = row.names(outliers))


# 2. Interactively choose outliers with the identify function
plot(calories ~ sugars, data = UScereal, pch = 19)
identify(UScereal$carbo, UScereal$calories, n = 2, labels = row.names(UScereal))     


# 3. Change the axis limits to remove outliers from view
plot(calories ~ sugars, data = UScereal, pch = 19, ylim = c(0, 325))



# Size (text size, point size, label size etc..)

# Use the cex argument family to change the size of any attribute
plot(calories ~ sugars, data = UScereal, pch = 19, ann = FALSE, cex = 1.5)
outliers <- UScereal[which(UScereal$calories > 300),]
text(outliers$sugars, outliers$calories - 15,
     labels = row.names(outliers), cex = .75)
title(main = 'Nutrition of a Single Cup of Cereal', ylab = 'Calories',
      xlab = 'Sugars (grams)', cex.main = 2, cex.lab = 1.5)



# Combine Graphs into the Same Window
par(mfrow = c(2, 2))

boxplot(calories ~ shelf, data = UScereal)
hist(UScereal$calories, breaks = 15)
boxplot(sugars ~ shelf, data = UScereal)
hist(UScereal$sugars, breaks = 15)

par(mfrow = c(1, 1)) # reset the matrix





# Exercise 1a. 
# How can we improve this graph? From what we have learned above, implement at least 3 improvements to this graph.
# A little more information about this dataset: The heart and body weights of samples of male and 
# female cats used for digitalis experiments. The cats were all adult, over 2 kg body weight.
data(cats)
head(cats)
plot(Hwt~Bwt, data = cats)


# Exercise 1b.
# How can we improve this graph? From what we have learned above, implement at least 3 improvements to this graph.
# A little more information about this dataset: The heart and body weights of samples of male and 
# Fisher's famours dataset measures the sepal and petal length and width for 3 species of Iris
data(iris)
head(iris)

boxplot(Petal.Width ~ Species, data = iris)



# ----------------------------------------------
# - II. Advanced Graphics using ggplot2 -
# ----------------------------------------------

install.packages('ggplot2')
library(ggplot2)

# A. The ggplot function

# Base q plot function
qplot(x = sugars, y = calories, color = as.factor(shelf), data = UScereal) 

# Use the ggplot function to get started
p1 <- ggplot(UScereal, aes(x = calories))



# B. Layers 

# Add layers to the original ggplot object with a '+'
p1 + geom_dotplot()

p1 + geom_density()

p1 + geom_histogram(binwidth = 10)


# And we add more layers
p1 + geom_histogram(binwidth = 10) + xlab('Calories in a Single Cup') +
  ylab('Frequency') + 
  ggtitle('Distribution of Calories in US Cereals') + theme_bw()


# The order of the layers does not matter
p1 + geom_histogram(binwidth = 10) + xlab('Calories in a Single Cup') +
  ylab('Frequency') + ggtitle('Distribution of Calories in US Cereals') + 
  theme_bw() + theme(text = element_text(size = 30))


# You can also add multiple geom_function layers to the same graph
p2 <- ggplot(UScereal, aes(x = sugars, y = calories, color = mfr))
p2  + geom_point() + geom_smooth() + geom_line()


# C. Aesthetics: x position, y position, size of elements, shape of elements, color of elements
# elements: geometric shapes such as points, lines, line segments, bars and text
# geomitries have their own aesthetics i.e. points have their own shape and size


# To color by manufacturer - we can put the color in the ggplot function:
p2 <- ggplot(UScereal, aes(x = sugars, y = calories, color = mfr))

p2 + geom_point() 

my_colors <- c('#9ebcda', '#8c96c6', '#8c6bb1', '#88419d', '#810f7c', '#4d004b')

p2 + geom_point() + scale_color_manual(values = my_colors) 


# Or inside the geom_point function:
p2 <- ggplot(UScereal, aes(x = sugars, y = calories))

p2 + geom_point(aes(color = mfr)) 


# Adding Labels to points

# Use the geom_text() layer
p2 + geom_point(aes(color = mfr)) + 
  geom_text(aes(label = row.names(UScereal)), hjust = 1.1)


# Changing point size
p2 + geom_point(aes(color = mfr), size = 4) 


# Editing the legend

# Use the scale_color_manual() layer, and the color argument in the labs() layer 
p2 + geom_point(aes(color = mfr), size = 5) + labs(color = 'Manufacturer') + 
  scale_color_manual(values = c('blue', 'green', 'purple', 'navyblue', 'red', 'orange'),  
                     labels = c('General Mills', 'Kelloggs', 'Nabisco', 'Post', 'Quaker Oats', 'Ralston Purina'))  + theme(text = element_text(size = 30)) 


# D. Faceting  - divide a plot into subplots based on the valuesof one or more discrete variables

# Tip: Use facets to help tell your story
# Q: How is the distribution of sugar across different shelves?
# Q: Are cereals with higher sugar content on lower shelves/at a child's eye level?

p3 <- ggplot(UScereal, aes(x = sugars))

p3 + geom_histogram(binwidth = 4)

# Each graph is in a separate row of the window
p3 + geom_histogram(binwidth = 4) + facet_grid(shelf ~ .)

# Each graph is in a separate column of the window
p3 + geom_histogram(binwidth = 4) + facet_grid(. ~ shelf)

# Finished product 
p3 + geom_histogram(fill = '#3182bd', color = '#08519c', binwidth = 4) +
  facet_grid(shelf ~ .) + theme(text = element_text(size = 20)) + 
  labs(title = 'Are Sugary Cereals on Lower Shelves?',
       x = 'Sugars (grams)', y = 'Count')

  
# Box Plots
p4 <- ggplot(UScereal, aes(mfr, calories))
p4 + geom_boxplot()
p4 + geom_boxplot(notch = TRUE)
p4 + geom_violin()

p4 + geom_boxplot(outlier.shape = 8, outlier.size = 4, fill = '#3182bd') + coord_flip() + 
  labs(x = 'Manufacturer', y = 'Calories') + theme_bw() + 
  scale_x_discrete(labels = c('General Mills', 'Kelloggs', 'Nabisco', 'Post', 'Quaker Oats', 'Ralston Purina'))

# Add median value to boxplot

install.packages('dplyr')
library(dplyr)
p4_meds <- UScereal %>% group_by(mfr) %>% summarise(med = median(calories))

p4 + geom_boxplot(outlier.shape = 8, outlier.size = 4, fill = '#8c96c6') + 
  labs(x = 'Manufacturer', y = 'Calories') + theme_bw() + 
  scale_x_discrete(labels = c('General Mills', 'Kelloggs', 'Nabisco', 'Post', 'Quaker Oats', 'Ralston Purina')) + 
  geom_text(data = p4_meds, aes(x = mfr, y = med, label = round(med,1)), size = 4, vjust = 1.2)


# Exercise 2a. Use ggplot2 to improve the graph below:
data(cats)
head(cats)
plot(Hwt~Bwt, data = cats)


# Exercise 2b. Use ggplot2 to improve the graph below:
data(iris)
head(iris)
boxplot(Petal.Width ~ Species, data = iris)




# ----------------------------------------------
# - III. Other Graphics -
# ----------------------------------------------

# Scatterplot Matrix
install.packages('GGally')
library(GGally)
ggpairs(UScereal[, c(2, 8, 9, 11)],
        upper = list(continuous = 'smooth', combo = 'facetdensity', discrete = 'blank') ,
        lower = list(continuous = 'cor', combo = 'box'))

# Maps
# http://bcb.dfci.harvard.edu/~aedin/courses/R/CDC/maps.html
# http://rstudio.github.io/leaflet/
# maps, choroplethr, 

# Network Analysis 

# ----------------------------
# - III. Exercise Solutions -
# ----------------------------

# Below are just examples of cleaned up graphics. There are many solutions on how to improve these graphs

# Exercise 1a 

palette(c('#fa9fb5', '#9ebcda'))
plot(Hwt~Bwt, data = cats, ylab  = 'Heart Weight (grams)', xlab = 'Body Weight (kg)', main = 'Measurements of Cats', pch = 16, col = Sex)

# Exercise 1b
boxplot(Petal.Width ~ Species, data = iris, xlab = 'Petal Width (centimeters)', 
        main = 'Distribution of Petal Length by Species', pch = 8, horizontal = TRUE, col = 'lightgray')



# Exercise 2a
e2a <- ggplot(cats, aes(Bwt, Hwt, color = Sex))
e2a + geom_point() + theme_bw() + labs(title = 'Measurements of Cats', x = 'Body Weight (kg)', y = 'Heart Weight (grams)')

# Exercise 2b
e2b <- ggplot(iris, aes(Species, Petal.Width))
e2b + geom_boxplot(fill = '#addd8e') + theme_bw() + coord_flip() + ggtitle('Distribution of Petal Length by Species' )


