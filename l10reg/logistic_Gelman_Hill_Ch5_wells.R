#### Gelman and Hill (2007) dataset for Chapter 5
  ## Section 5.4 - Wells in Bangladesh

library("arm")
wells <- read.table ("http://www.stat.columbia.edu/~gelman/arm/examples/arsenic/wells.dat")
attach.all (wells)