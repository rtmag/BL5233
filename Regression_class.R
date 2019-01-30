data<- read.table("spArea.txt",header=T)
names(data)

par(mfrow=c(2,2))

plot(log(data$Species)~log(data$Area),pch=16,xlab= "Area",ylab="Species")

model1<- lm(log(Species)~log(Area),data=data)
plot(log(data$Area),resid(model1))
# shows that the residual is low then big then low, meaning that a simple liner model is not gonna work.

#seeting two separated models depending on whether the area is lower o higher than 148
model2<-lm(log(Species)~ (log(Area)*(Area<148))+(log(Area)*(Area>=148)),data=data)

area=sort(unique(data$Area))
plot(log(data$Species)~log(data$Area ))
lines(log(area),predict(model2,list( Area=area)))

anova(model1, model2)

############
# non linear regression
deer = read.table("jaws.txt",header=T)
plot(deer$age,deer$bone)

model<-nls(bone~a-b*exp(-c*age), start=list(a=120,b=115,c=0.065), data=deer)
# a is where the line tends to infinity, b is the starting point of 0 on X and C is the ratio between X~Y for a given point.
summary(model)

av<-seq(0,50,0.1) 
bv<-predict(model,list(age=av)) 
lines(av,bv,col="red")

model2<-nls(bone~a*age/(1+b*age), start=list(a=10.6,b=0.06), data=deer)
summary(model2)
ymm<-predict(model2,
list(age=av))
lines(av,ymm,lty=2)
############################################################################################################
############################################################################################################
################## MULTIPLE REGRESSION

# find 2 ways to remove variables that dont explain the structure in the dataset.

# Multiple regression coefficient = What happens to Y (dependand variable) if we increase a unit of the explanatory variable.

dataO3<-read.table("ozone.data.txt",header=T)
names(dataO3)
pairs(dataO3,panel=panel.smooth)

#First we fit the full model with all the interactions and cuadratic terms and the idea is to strat to trim out terms and
# interactions until we have a reduced model.
model1<- lm(ozone~temp*wind*rad +I(rad^2)+I(temp^2)+I(wind^2) , data = dataO3)
summary(model1)
# we look at the last column (the pvalues) and remove the most complicated that is not significant.

model2<-update(model1,~. -temp:wind:rad)
anova(model1,model2)#not significant, can continue 
summary(model2)

model3<-update(model2,~. - temp:rad)
anova(model2, model3)#p-value = 0.13, can continue 
summary(model3)

model4<-update(model3,~. - temp:wind) 
anova(model3, model4)#p-value = 0.056, continue 
summary(model4)

model5<-update(model4,~. - wind:rad) 
anova(model4, model5)#p-value = 0.09 
summary(model5)

model6<-update(model5,~. - I(rad^2)) 
anova(model5,model6)
summary(model6)
# we stop here cause the anova pvalue is lower than 0.05

plot(model6)
#Residual vs Fitted has trumpet shape so it has heterocedasticity
# Q-Q plot doesn't looks normal-ish
# for this reason we try log transform the data to make it more normal and see if the variance gets equal

# we should start from the very beginning(full model) but to save time we continue with our previous model
model7<-lm(log(ozone) ~ temp + wind + rad + I(temp^2) + I(wind^2), data = dataO3)
summary(model7)

model8<-update(model7,~. - I(temp^2)) 
anova(model7,model8)
summary(model8)

plot(model8)
# heterocedasticity seems fix as well as qqplot

#function step automatically removes interactions until the top AIC is none.
model10<-step(model1)

##########
library(MuMIn)
models_sel <- model.sel(model1,model2,model3,model4,model5,model6,model7, rank="AICc")
mod.avg <- summary(model.avg(models_sel,subset=delta<2))

model1<-lm(log(ozone) ~ 1 , data = dataO3)
model2<-lm(log(ozone) ~ temp , data = dataO3)
model3<-lm(log(ozone) ~ wind , data = dataO3)
model4<-lm(log(ozone) ~ rad , data = dataO3)
model5<-lm(log(ozone) ~ temp + wind , data = dataO3)
model6<-lm(log(ozone) ~ wind + rad , data = dataO3)
model7<-lm(log(ozone) ~ temp + rad , data = dataO3)
model8<-lm(log(ozone) ~ temp + wind + rad , data = dataO3)
model9<-lm(log(ozone) ~ temp + wind + rad + I(temp^2) , data = dataO3)
model10<-lm(log(ozone) ~ temp + wind + rad + I(wind^2), data
= dataO3)
model11<-lm(log(ozone) ~ temp + wind + rad + I(temp^2) + I(wind^2), data = dataO3)

library(MuMIn)
models_sel <- model.sel(model1,model2,model3,model4,model5,model6,model7,
model8,model9,model10,model11, rank="AIC")

