data = read.table("Ovary.txt",header=T)

library(lattice)

xyplot(follicles ~ Time, data, grid = TRUE)

library(nlme )

lme1 <- lme(follicles ~ Time * Mare, data, random =~1|Mare)
lme2 <- lme(follicles ~ 1 * Mare, data, random =~1|Mare)

plot(ACF(lme1),alpha=0.05)

cs2 = corARMA(c(.3), p = 1, q = 0)
cs3 = corARMA(c(.3), p = 0, q = 1)
cs4 = corARMA(c(.3,-.3), p = 0, q = 2)
cs5 = corARMA(c(.3,-.3), p = 2, q = 0)
cs6 = corARMA(c(.3,-.3), p = 1, q = 1)
cs7 = corARMA(c(.3,-.3,.2), p = 1, q = 2)
cs8 = corARMA(c(.3,-.3,.2), p = 2, q = 1)
cs9 = corARMA(c(.3,-.3,.2,-.2), p = 2, q = 2)

arma1 <- lme(follicles ~ Time * Mare, data = data, correlation = cs1, random =~1|Mare)
arma2 <- lme(follicles ~ Time * Mare, data = data, correlation = cs2, random =~1|Mare)
arma3 <- lme(follicles ~ Time * Mare, data = data, correlation = cs6, random =~1|Mare)
arma4 <- lme(follicles ~ Time * Mare, data = data, correlation = cs4)
arma5 <- lme(follicles ~ Time * Mare, data = data, correlation = cs5)
arma6 <- lme(follicles ~ Time * Mare, data = data, correlation = cs6)
arma7 <- lme(follicles ~ Time * Mare, data = data, correlation = cs7)
arma8 <- lme(follicles ~ Time * Mare, data = data, correlation = cs8)
arma9 <- lme(follicles ~ Time * Mare, data = data, correlation = cs9)


AIC(arma1,arma2,arma3,arma4,arma5,arma6,arma7,arma8,arma9)
#########################################################
install.packages("MCMCglmm")
library(MCMCglmm)
BTdata = data("BTdata")
BTped = data("BTped")

m <- MCMCglmm(y ~ (a + b + c)^3,
                       ~ us(1 + (a + b + c)^3):subject +
                         us(1 + (a + b    )^2):item,
               data   = d,
               family = "categorical",
               prior  = prior.m3,
               thin   = 20,
               burnin = 3000,
               nitt   = 23000)
