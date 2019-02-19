//
-Poisson distribution for count data.
-Binomial errors useful for porportions
-Gamma errors or neegative binomial, for data showing variance increases faster than linearly witht the mean.

The coeficients in GLM map to the transform dependant variable... we need to back transform them using the link function.

Deviance  is how far is your model from a model that explains all the data 
(-2 times the difference in log-likelihood between the current model and a saturated model (that fits the data perfectly).
So by trying to minimize the deviance we maximaze the likelihood.

To check whether there is an overdisersion issue. You check the deviance and the degrees of freedom.

model1<-glm(Cancers~Distance,poisson,data=clusters)
model2<-glm(Cancers~Distance,quasipoisson,data=clusters)
summary(model1)

Call:
glm(formula = Cancers ~ Distance, family = poisson, data = clusters)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.5504  -1.3491  -1.1553   0.3877   3.1304  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)  
(Intercept)  0.186865   0.188728   0.990   0.3221  
Distance    -0.006138   0.003667  -1.674   0.0941 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 149.48  on 93  degrees of freedom
Residual deviance: 146.64  on 92  degrees of freedom
AIC: 262.41

Number of Fisher Scoring iterations: 5

## THERE is overdispersion so we use the quasipoisson

# there is no clear relationship between living close to powerstation and having cancer.
xv<-seq(0,100,0.1)
yv<-predict(model2,list(Distance=xv))
lines(xv,exp(yv))

##############################################
count<-read.table("cells.txt",header=T)
names(count)
tapply(count$cells,count$smoker,mean)

model1<-glm(cells~smoker*sex*age*weight,poisson,data=count)
summary(model1)
model2<-glm(cells~smoker*sex*age*weight,quasipoisson, data=count) 
summary(model2)

# QUASIPOISSON SHOULD BE USED WHEN HAVING OVERDISPERSSION
# If using Quasi-likelyhood you cant use chi-square, you need to use F-TEST

model3<-update(model2,~. -smoker:sex:age:weight)
anova(model2,model3,test="F")

model4<-update(model3,~. - sex:age:weight)
anova(model3,model4,test="F")
##############################################
 Instead of using quasi, we can use the negative binomial distribution.
 "How many test do we need to achive a number of successes....."?
 
 numbers <- read.table("sexratio.txt",header=T)
par(mfrow=c(1,2))
 p<-numbers$males/(numbers$males+numbers$females)
 plot(numbers$density,p,ylab="Pro portion male")
 plot(log(numbers$density),p,ylab="Proportion male")
 
 y<-cbind(numbers$males,numbers$females)
model<-glm(y~density,binomial, data=numbers)
summary(model) 
 
model1<-glm(y~log(density),binomial, data=numbers)
 summary(model1) 
