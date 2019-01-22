x = 1:10
y = x[x<4]
x[-4]

vector1 = c(1,3,5,7,9,10)
vector2 = c(1,3)

match(vector1, vector2)

vector1[!is.na(match(vector1, vector2))]

# MATRIX stuff
x = matrix(0, nrow = 3, ncol = 3 )
x[,1] = rnorm(3,1,1)
x[,2] = rnorm(3,1,1)
x[,3] = rnorm(3,1,1)

x[,2]
x[,1] + x[,2]
x[,1] * x[,2]

cbind(x[,1],x[,2])
rbind(x[,1],x[,2])

cbind(x,1:3)

dim(x)

cbind(x,apply(x,2,mean))

centeringFunction = function(x){ x - mean(x) }

cbind(x,apply(x,2,centeringFunction))

# LOOPS
for(i in 1:10){
  print("hello")
}
# create factorial function with a for loop, just for fun
myFactorial <- function(x){
  n = 1
  for(i in 1:x){
    n = n * (x-i+1)
  }
  return(n)
}
######################################################################
a = 10; b = "Hola";
if(a>5 & b=="Hola") {print("OK")}
######################################################################
a = 10; b = "ola";
if(a>5 & b=="Hola") {print("OK")} else {print("Cannot")}
######################################################################
a = 10; b = "Hola";
x = matrix(0, nrow = 3, ncol = 3 )
x[,1] = rnorm(3,1,1)
x[,2] = rnorm(3,1,1)
x[,3] = rnorm(3,1,1)
if(a>5 & b=="Hola") { apply(x,1,mean) }
######################################################################
myList <- list(1,"calor",1:10,x)

lapply(myList,print)
lapply(myList,length)

class(myList[[1]])
class(myList[[2]])
class(myList[[3]])
class(myList[[4]])
######################################################################
# READ LIZARD DATA

myData = read.table("~/Downloads/allDatasets/lizards.txt",header=T)
data(CO2)
write.table(CO2,"CO2_test.txt",sep="\t",quote=FALSE,row.names=F)
myData2 = read.table("CO2_test.txt",header=T)

rowSums(apply(CO2,2,is.na))
#######################################################################
protectArea <- c('typeI','typeII','typeIII','typeIV','typeV') 
PAtype <- sample(protectArea, 1000, replace = T)
deforest <- runif(1000,0,1)
area <- rnorm(1000,5000,200)
poachEvents <- rpois(1000,30)
PAdata = data.frame(PAtype, deforest, area, poachEvents)

library(dplyr)

# Several subsects, equal to [ x<10 & y<20 ]
filter(PAdata, PAtype == 'typeI', area >= 5000)

# is like SORT, sorts by PAtype alphabetically first and then numerically by area
arrange(PAdata, PAtype, area)

# subset takes only column area and deforest
PAdata2 = select(PAdata, area,deforest)

PAtypeGroup <- group_by(PAdata, PAtype)
PAtypeSummary <- summarise(PAtypeGroup,
  count = n(), meanPoach = mean(poachEvents, na.rm =
TRUE))
