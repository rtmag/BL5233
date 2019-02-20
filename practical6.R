library(MASS)
data(quine)
summary(quine)

model1 = glm(Days ~ Eth*Sex*Age*Lrn, family = poisson, data = quine )
summary(model1)
# Residual deviance not same as degree of freedom so we switch to quasiPoisson.
model1_quasi = glm(Days ~ Eth*Sex*Age*Lrn, family = quasipoisson, data = quine )
summary(model1_quasi)
