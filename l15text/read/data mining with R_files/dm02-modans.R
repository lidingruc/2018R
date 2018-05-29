# dm02 model answer with credit to: Shahil Rajpaul

x <- runif(500, min = 0, max = 1)            
error = rnorm(length(x),mean=0,sd=2)*(x/3)   
y = 1 + (2*x) + error

#R's built in function
mod <- lm(y ~ x)
coef(mod)

myfit <- function(x,y) {
  n <- length(x)
  slope <-(n*sum(x*y)-(sum(x))*(sum(y)))/(n*sum(x*x)-((sum(x))*(sum(x))))
  intercept <- (sum(y)-(((n*sum(x*y)-(sum(x))*(sum(y)))/(n*sum(x*x)-((sum(x))*(sum(x)))))*sum(x)))/n
  
  return (list("intercept"=intercept,"slope"=slope))
}

fit <- myfit(x,y)
fit

plot(x,y,col="dodgerblue",main=paste("y = 1 + 2*x + error","\n fitted with straight line \nIntercept:",
                    round(fit[1],2),"Slope:", round(fit[2],2)))
abline(fit,col="firebrick",lwd=2, lty=6)
abline(mod,col="green",lwd=2, lty=6)

