# 1
data <- read.table("/Users/wone/Downloads/allDatasets/datasetTutorial1corrected.txt",sep="\t",header=T)
# 2
root <- data$Root
# 3
data2 <- cbind( data, newVar=1:dim(data)[1] )
write.table(data2,"data2.txt",sep="\t",quote=F,row.names=F)
# 4
mean(data[,2])
sd(data[,2])
# 5
data$Fruit[data$Fruit<50]
data[data$Grazing=="Grazed",]
# 6
round((data[,2]^3.5) / log(data[,1]),digits=0)
# 7
print("Grazed mean:")
mean(data[data$Grazing=="Grazed",2])
print("Ungrazed mean:")
mean(data[data$Grazing=="Grazed",2])
# 8 
plot(data$Root,data$Fruit,xlab="Root length",ylab="Fruit weight")
# 9 
howManyNAs = function(x) { return( print( paste("Number of NAs in your data:", sum(is.na(x)) ) ) ) }
howManyNAs(data)
# 10
(data$Root - mean(data$Root,na.rm=T)) / sd(data$Root,na.rm=T)
# 11
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
par(mfrow=c(2,2))
m1 <- rpart(
  formula = Root ~ . ,
  data    = data,
  method  = "anova"
  )
rpart.plot(m1,main="Root ~ .")

m1 <- rpart(
  formula = Fruit ~ . ,
  data    = data,
  method  = "anova"
  )
rpart.plot(m1,main="Fruit ~ .")

m1 <- rpart(
  formula = Root ~ Fruit ,
  data    = data,
  method  = "anova"
  )
rpart.plot(m1,main="Root ~ Fruit")

m1 <- rpart(
  formula = Fruit ~ Root ,
  data    = data,
  method  = "anova"
  )
rpart.plot(m1,main="Fruit ~ Root")

# 12
