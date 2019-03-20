Boreality<- read.table("Boreality.txt",header=T )

Boreality$Bor<-sqrt(1000*(Boreality$nBor+1)/(Boreality$nTot))

B.lm<-lm(Bor~Wet,data=Boreality) 
summary(B.lm)
par(mfrow=c(2,2))
plot(B.lm)

library(gstat)
library(sp)

E<-rstandard(B.lm)
residAndCoord<-data.frame(E,Boreality$x,Boreality$y)

coordinates(residAndCoord) = c("Boreality.x","Boreality.y")

bubble(residAndCoord,"E",col= c("black","grey"), main="Residuals",xlab="X- coordinates", ylab="Y- coordinates")
####
library(nlme)
model<-gls(Bor~Wet,data=Boreality)
plot(Variogram(model,form=~Boreality$x+Boreality$y))
#
f1 <- formula(Bor ~ Wet)
B1.gls<-gls(f1, data =Boreality)
var1<-Variogram(B1.gls,form=~x+y, maxDist=2000, resType="pearson")
plot(var1,smooth=T)

B1A<- gls(f1,correlation=corSpher(form=~x+y,nugget=T),data=Boreality)
control1 = glsControl(maxIter = 10, msMaxIter = 10, tolerance = 1e-5)
B1B<- gls(f1,correlation=corLin(form=~x+y,nugget=T),data=Boreality,control = control1)
B1C<- gls(f1,correlation=corRatio(form=~x+y,nugget=T),data=Boreality)
B1D<- gls(f1,correlation=corGaus(form=~x+y,nugget=T),data=Boreality)
B1E<- gls(f1,correlation=corExp(form=~x+y,nugget=T),data=Boreality)
AIC(B1.gls,B1A,B1C,B1D,B1E)
