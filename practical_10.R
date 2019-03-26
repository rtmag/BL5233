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
lme1 <- lme(yield~variety, random = ~1|Block ,data = data)
lme1_B1C<- lme(yield~variety,correlation=corRatio(form=~latitude+longitude,nugget=T), random = ~1|Block,data=data)

AIC(model_onlyVariety,B1A,B1C,B1D,B1E,lme1,lme1_B1C)

plot(Variogram(model,form=~data$latitude+data$longitude))
# The alternative is to construct a variogram. A variogram measures how quickly spatial autocorrelation 
# γ(h) falls off with increasing distance.
# We see that the autocorrelation increases up to approximately 22, then it drops. That would be the range. 
# Points within 22 distance tend to be related.

library(MuMIn)
r.squaredGLMM(lme1)
#The random intercept model explains 29% of the variance and 15% is explained by the fixed effects alone.
r.squaredGLMM(lme1_B1C)
#The random intercept model explains 9% of the variance and 9% is explained by the fixed effects alone.


data2 = read.table("splityield.txt",header=T)


M1 <- lmer (yield ~ irrigation + 
            (1 |block/density/fertilizer),data=data2)

#random slope siempre es una variable continua
