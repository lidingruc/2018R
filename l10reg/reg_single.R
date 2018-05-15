set.seed(1234)
new.data<-data.frame(math=jitter(sort(rnorm(50, 75, 8))), iq=jitter(rnorm(50, 100, 15), factor=28))
