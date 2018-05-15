
threeway<-data.frame(score=c(170,175,165,180,160,158,161,173,157,152,181,190,186,194,201,215,219,209,164,166,159,182,187,174,180,187,199,170,204,194,162,184,183,156,180,173,173,194,197,190,176,198,164,190,169,164,176,175,187,194,217,206,199,195,171,173,196,199,180,203,202,228,190,206,224,204,205,199,170,160,179,179), biofeed=rep(c("yes", "no"), each=36), diet=rep(rep(c("absent", "present"), each=6), 6), drug=rep(rep(c("X", "Y", "Z"), each=12), 2))

par(mfrow=c(1,2))
with(threeway[threeway$biofeed=="yes",], interaction.plot(diet, drug, score, main="Biofeedback = Yes"))

with(threeway[threeway$biofeed=="no",], interaction.plot(diet, drug, score, main="Biofeedback = No"))

par(mfrow=c(1,2))
with(threeway[threeway$diet=="absent",], interaction.plot(biofeed, drug, score, main="Diet is Absent"))

with(threeway[threeway$diet=="present",], interaction.plot(biofeed, drug, score, main="Diet is Present"))

par(mfrow=c(1,3))
with(threeway[threeway$drug="X",], interaction.plot(biofeed, diet, score, main="Drug X"))

with(threeway[threeway$drug="Y",], interaction.plot(biofeed, diet, score, main="Drug Y"))

with(threeway[threeway$drug="Z",], interaction.plot(biofeed, diet, score, main="Drug Z"))

#### Run the full factorial ANOVA
m1<-aov(score~biofeed*diet*drug, threeway)
summary(m1)
model.tables(m1, type="means")

#### Run ANOVA with only main effects and three-way interaction
m2<-aov(score~biofeed+diet+drug+biofeed:diet:drug, threeway)
summary(m2)