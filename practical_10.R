library(nlme)

data = read.table("spatialdata.txt",header=T)

par(mfrow=c(2,2))
plot(data$latitude,data$yield)
plot(data$longitude,data$yield)
barplot(tapply(data$yield,data$variety,mean))
barplot(tapply(data$yield,data$Block,mean))


model_noSpace<-gls(yield~ Block+variety+latitude+longitude,data=data)
model_onlyVariety<-gls(yield~ variety,data=data)

f1 <- formula(yield~variety)

B1A<- gls(f1,correlation=corSpher(form=~latitude+longitude,nugget=T),data=data)
control1 = glsControl(maxIter = 10, msMaxIter = 10, tolerance = 1e-5)
B1B<- gls(f1,correlation=corLin(form=~latitude+longitude,nugget=T),data=data,control = control1)
B1C<- gls(f1,correlation=corRatio(form=~latitude+longitude,nugget=T),data=data)
B1D<- gls(f1,correlation=corGaus(form=~latitude+longitude,nugget=T),data=data)
B1E<- gls(f1,correlation=corExp(form=~latitude+longitude,nugget=T),data=data)
AIC(model_noSpace,model_onlyVariety,B1A,B1C,B1D,B1E)

plot(Variogram(model,form=~data$x+data$y))
