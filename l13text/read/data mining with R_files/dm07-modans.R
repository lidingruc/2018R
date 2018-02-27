# dm07 model answers (with some credit to Simon Reade)

######Proof######
# if x . w + b > 0, then x is positive, otherwise x is negative
# 
# if s > 0, then 
# x . s w + s b = s ( x . w + b ) which is > 0 if x . w + b > 0
# 
# therefore scaling by s of w and b does not change classification
# 
#################

rm(list = ls())

distance.from.plane = function(z,w,b) 
{
   sum(z*w) + b
}

classify.linear = function(x,w,b) 
{
  distances = apply(x, 1, distance.from.plane, w, b)
  return(ifelse(distances < 0, -1, +1))
}

euclidean.norm = function(x) 
{
  sqrt(sum(x * x))
}

perceptron = function(x, y, learning.rate=1, max_iteration=100, termination_error=0) 
{
  w = vector(length = ncol(x)) # initialize w
  b = 0 # Initialize b
  k = 0 # count updates
  R = max(apply(x, 1, euclidean.norm))
  made.mistake = TRUE # to enter the while loop
  
  best_error <- 1
  iteration <-  1
  
  while (made.mistake) 
  {
    made.mistake=FALSE # hopefully
    yc <- classify.linear(x,w,b)
    current_error <- round(sum(yc != y) / length(y), 2)
    if(current_error < best_error) # check error for termination 
    {
      best_w <- w
      best_b <- b
      best_error <- current_error # update error
    }
    
    print(paste("iteration:", iteration, "| error: ", current_error))
    
    for (i in 1:nrow(x)) 
    {
      if (y[i] != yc[i]) 
      {
        w <- w + learning.rate * y[i]*x[i,]
        b <- b + learning.rate * y[i]*R^2
        k <- k+1
        made.mistake=TRUE
      }
    }
    iteration <- iteration + 1
    if(iteration > max_iteration || best_error <= termination_error) # check termination condition
    {
      made.mistake = FALSE
    }
  }
  
  s = euclidean.norm(w)
  return(list(w=best_w/s,b=best_b/s,updates=k))
}

perceptron.plot <- function(x,y,w,b)
{
  plot(x, cex=0.0)
  points(subset(x,Y==1),col="black",pch="+",cex=1.5)
  points(subset(x,Y==-1),col="red",pch="-",cex=1.5)
  intercept <- - b / w[[2]]
  slope <- - w[[1]] / w[[2]]  
  abline(intercept,slope,col="blue")
}


x1 <- runif(30,-1,1)
x2 <- runif(30,-1,1)
x <- cbind(x1,x2)
Y <- ifelse(x2>0.5+x1,+1,-1)

p <- perceptron(x,Y)

perceptron.plot(x,Y,p$w,p$b)


data(iris)
names(iris)
pairs(iris[,1:4], col=iris$Species)
summary(iris$Species)


iris$class <- ifelse(iris$Species == "virginica", +1, -1)

x <- iris[,c(3,4)]
Y <- iris$class

# Apply perceptron to generate decision boundary
( p <- perceptron(x,Y,5,100,termination_error=0.0) )

# Plot peceptron decision boundary
perceptron.plot(x,Y,p$w,p$b)

sum(classify.linear(x,p$w,p$b) != Y)
