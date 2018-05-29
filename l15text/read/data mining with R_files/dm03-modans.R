# model answer, Week 03,   partial credit to Shahil Rajpaul and Bhavna Harbhajan   

treeg <- read.csv("treeg.csv",row.names=1)
head(treeg)

my.grate <- function(d,h,a) {
  v <- pi*(d/24)*(d/24)*h
  gr <- v/a
  return (gr)
}

# compute a growth rate so far column for all trees 
treeg$gr.rate <- my.grate(treeg$dbh.in,treeg$height.ft,treeg$age)   

# compute mean growth rate for each tree and forest id for each tree
grateTree <- tapply(treeg$gr.rate,treeg$tree.ID,max)     # could use mean here
forestTree <- tapply(treeg$forest,treeg$tree.ID,mean)

# now compute average growth rates for each forest
tapply(grateTree,forestTree,mean)

#fit straight line to a tree to determine slope/growth rate
fit.growthRate<-function(age,volume){ 
  fit <- lsfit(age,volume)
  x<-c(fit$coefficients)
  return (x[2])
}

calc.volume <- function(d,h) {
  v <- pi*(d/24)*(d/24)*h
  return (v)
}

ages <- tapply(treeg$age,treeg$tree.ID,identity)                 
volumes<-tapply(mapply(calc.volume,treeg$dbh.in,treeg$height.ft),
                treeg$tree.ID,identity)

forests<-tapply(treeg$forest,treeg$tree.ID,mean)              #categorise forests according to tree ID
tapply(mapply(fit.growthRate,ages,volumes),forests,mean)   


# using dplyr

library(dplyr)

calc.volume <- function(d,h) {
  v <- pi*(d/24)*(d/24)*h
  return (v)
}

fit.growthRate<-function(age,volume){ 
  fit <- lsfit(age,volume)
  x<-c(fit$coefficients)
  return (x[2])
}

trees_with_vol <- mutate(treeg, vol=calc.volume(dbh.in,height.ft))
trees_by_id <- group_by(trees_with_vol, tree.ID)
trees <- summarise(trees_by_id, growth.rate=fit.growthRate(age,vol), forest=max(forest))
trees_by_forest <- group_by(trees, forest)
summarise(trees_by_forest, forest_growth = mean(growth.rate))
