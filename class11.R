library(lme4)
model1<-model1<-glmer(y~trt+(week|ID),family=binomial,
nAGQ = 1, data=bacteria)
summary(model1)
