library(MASS)
data(quine)
summary(quine)

# A

model1 = glm(Days ~ Eth*Sex*Age*Lrn, family = poisson, data = quine )
summary(model1)
# Residual deviance not same as degree of freedom so we switch to quasiPoisson.

model1_quasi = glm(Days ~ Eth*Sex*Age*Lrn, family = quasipoisson, data = quine )
summary(model1_quasi)

model2<-update(model1_quasi,~. - Eth:Sex:Age:Lrn)
anova(model1_quasi,model2,test="F") # test with F-test because we using quassi model
# Pr val of .69 we can continue cause it is not significantly worst than the previous

model3_a<-update(model2,~. - Eth:Sex:Age)
anova(model2,model3_a,test="F") 
# gives a pval of 0.4986, so we keep the previous model [[model2]].

model3_b<-update(model2,~. - Eth:Sex:Lrn)
anova(model2,model3_b,test="F") 
# gives a pval of 0.31, continue

model4_a<-update(model3_b,~. - Sex:Age:Lrn)
anova(model3_b,model4_a,test="F") 

model4_b<-update(model3_b,~. - Eth:Sex:Age)
anova(model3_b,model4_b,test="F") 
# model4_b pval .1 we can proceed

model4_c<-update(model3_b,~. - Eth:Age:Lrn)
anova(model3_b,model4_c,test="F") 

model5_a<-update(model4_b,~. - Eth:Age:Lrn)
anova(model4_b,model5_a,test="F") 

model5_b<-update(model4_b,~. - Sex:Age:Lrn)
anova(model4_b,model5_b,test="F") 
# Pval 0.1 we can proceed

model6<-update(model5_b,~. - Eth:Age:Lrn)
anova(model5_b,model6,test="F") 
# cannot remove the remaining 3 way interaction.
# This is the last model, dont remove contained variables. Contained means that are significant in the other higher way interaction

model7<-update(model5_b,~. - Eth:Sex)
anova(model5_b,model7,test="F") 
# Pval 0.18 we can proceed

model8<-update(model7,~. - Eth:Age)
anova(model7,model8,test="F") 
# lower than 0.05

model9<-update(model7,~. - Age:Lrn)
anova(model7,model9,test="F")
# lower than 0.05


# B

data = read.table("flowering.txt",header=T)
flow = data.frame(data,proportion=data$flowered/data$number,NOTflowered=data$number-data$flowered)
y = cbind(flow$flowered,flow$NOTflowered)
