# R script, week 01
# author: hugh murrell

n <- 500
x <- runif(n,0,1)
y <- 1 + 2*x + (x/4)*rnorm(n,0)

fit <- lsfit(x,y)   # or use lm(y~x)

plot(x,y,main=paste("y = 1 + 2*x + err","\n fitted with straight line \nintercept:",
                      round(coef(fit)[1],2),"slope:", round(coef(fit)[2],2)))
abline(fit,col='red')
