#### CFA of the Holzinger-Swineford Data
library(MBESS)
data(HS.data)
HS.cov<-cov(HS.data[,7:30])

HS.paths<-specify.model()
  reading->wordc, lambda11, NA
  reading->wordm, lambda12, NA
  reading->sentence, lambda13, NA
  reading->paragrap, lambda14, 1
  mathem->figurer, lambda21, NA
  mathem->object, lambda22, NA
  mathem->deduct, lambda23, NA
  mathem->numeric, lambda24, 1
  reading <-> mathem, phi11, NA
  wordc <-> wordc, wordc.var, NA
  wordm <-> wordm, wordm.var, NA
  sentence <-> sentence, sentence.var, NA
  paragrap <-> paragrap, paragrap.var, NA
  figurer <-> figurer, figurer.var, NA
  object <-> object, object.var, NA
  deduct <-> deduct, deduct.var, NA
  numeric <-> numeric, numeric.var, NA
  reading <-> reading, NA, 1
  mathem <-> mathem, NA, 1
  
#### Run CFA
sem.HS<-sem(HS.paths, HS.cov, 301)
summary(sem.HS)