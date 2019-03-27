library(MASS)
data(bacteria)
names(bacteria)

bacteria$y <- 1*(bacteria$y=="y")

library(lme4)
model1<-model1<-glmer(y~trt+(week|ID),family=binomial,
nAGQ = 1, data=bacteria)
summary(model1)

################
DeerEcervi<-read.table("DeerEcervi.txt",header=T)
names(DeerEcervi)
DeerEcervi$Ecervi.01 <- DeerEcervi$Ecervi
DeerEcervi$Ecervi.01[DeerEcervi$Ecervi>0]<-1
DeerEcervi$fSex <- factor(DeerEcervi$Sex)
DeerEcervi$fFarm <- factor(DeerEcervi$Farm)
DeerEcervi$CLength <- DeerEcervi$Length - mean(DeerEcervi$Length)

library(MASS)
DE.PQL<-glmmPQL(Ecervi.01 ~ CLength * fSex,random = ~ 1 | fFarm, family = binomial, data = DeerEcervi)
summary(DE.PQL)
