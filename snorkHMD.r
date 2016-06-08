########################################################################
## provides cleanHMD() which when given a life table from the HMD
## read with read.table() such as this:
##Ltab<-read.table(file='US1X5.lifeTable',
##                 skip=2,header=T,
##                 as.is=T)
##
## returns the same thing with with an upper.age and lower.age
## column appended.
########################################################################
cleanHMD<-function(ltab){
  
### Convert Age to something useful
  ## if you are just using a single life table from the HMD you would
  ## not botther with these R tricks for parsing the age category
  ## strings into numbers. You would just edit the file you got from HMD
  ## in emacs instead. So don't sweat it if this stuff is too
  ## arcane. It's not the important part of this week's adventure
  
  
  ## use strsplit to create a list of the two numbers in each age category
  age.list<-strsplit(x=ltab$Age,split='-')
  
  upper.age<-function(x){
### for use in sapply  just returns first element of a vector unless
### the vector  has only one element in which case we get clever and
### figure out if it's 0 or 110+
    if(length(x)==2){return(x[2])}
    
    if(x == "0"){ return(0)}
    if(x =="110+"){return(114)}
  }
  
  
  ages.upper<- as.numeric(  sapply(age.list,upper.age))
  
  lower.age<-function(x){
### for use in lapply  just returns second element of a vector unless
### the vector  has only one element in which case we get clever and
### figure out if it's 0 or 110+
    if(x[1] =="110+"){return(110)}
    return(x[1])
    
  }
  
  ages.lower<- as.numeric(  sapply(age.list,lower.age))
  
  cbind(ages.lower,ages.upper,ltab$Age)
  
  ltab$age.lower<-ages.lower
  ltab$age.upper<-ages.upper
return(ltab)
}  
