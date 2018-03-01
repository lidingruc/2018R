# model answer with thanks to Naeem Majeed 2015

rpts <- function(v, pts)
{
  runif(pts, min = v[1], max = v[2]) 
}

library(MASS)
wine <- read.csv("wine.csv", sep = ",")
View(wine)
LDA <- lda(Cultivar ~ ., data = wine)
X <- data.frame(predict(LDA)$x)
X$class <- factor(ifelse(predict(LDA)$class == wine$Cultivar, as.numeric(wine$Cultivar), 0))

library(ggplot2)
color <- c("red", "green", "blue","purple")
pts <- 5000
cube <- data.frame(apply(apply(wine[,2:14],2,range),2,rpts,pts))
cube.pred <- predict(LDA,cube)
X.grid <- data.frame(cube.pred$x)
X.grid$class <- factor(as.numeric(cube.pred$class))
p <- ggplot(X, aes(x = LD1, y = LD2)) 
p <- p + geom_point(data=X.grid,aes(colour=class),size=0.002)
p <- p + geom_point(aes(colour=class),size=1.5)
p
sum(wine$Cultivar != predict(LDA)$class)



