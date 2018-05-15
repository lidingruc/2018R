#### Data taken from Pedhazur (1997), pp. 753.
#### In this data, the dv notes whether or not
  ## someone was admitted to a program. The 
  ## two ivs are a measure of mechanical 
  ## aptitude and gender.
  
#### Data
admin<-data.frame(mech=c(8,7,5,3,3,5,7,8,5,5,4,7,3,2,4,2,3,4,3,2), gender=rep(1:2, each=10), admit=c(1,0,1,0,0,1,1,1,1,1,0,1,1,0,0,0,0,1,0,0))

#### Task 1 - Run a logistic regression where
  ## admit is modeled as a function of student
  ## mechanical ability.
  

#### Task 2 - Run a logistic regression where
  ## admit is modeled as a function of student
  ## gender.
  

#### Task 3 - Run a logistic regression where
  ## admit is modeled as a function of student
  ## mechanical ability and gender.
  
  
#### Task 4 - Run a logistic regression where
  ## admit is modeled as a function of student
  ## mechanical ability, gender, and the
  ## interaction effect.