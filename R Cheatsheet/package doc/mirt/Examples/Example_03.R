#------------------------------------------------------------------
# Example 3 -- lower level arguments and package specific functions
#------------------------------------------------------------------

library(mirt)

#using lsat7 data, and creating a group variable
lsat <- expand.table(LSAT7)

#crossprod information matrix
(mod1 <- mirt(lsat, 1, SE = TRUE, SE.type = 'crossprod'))
coef(mod1)
coef(mod1, printSE = TRUE)

# sandwhich standard errors
sw <- mirt(lsat, 1, SE = TRUE, SE.type = 'sandwich')
coef(sw)

# Profiled-likelihood/boostrapped confidence intervals. Run in parallel
mirtCluster()
help(PLCI.mirt)
PLCI.mirt(mod1)

help(boot.mirt)
boot.mirt(mod1)

#constrain first and second item slopes to be equal
model <- mirt.model('Theta = 1-5
                    CONSTRAIN = (1-2, a1)')
mod2 <- mirt(lsat, model)
coef(mod2, simplify=TRUE)
anova(mod1, mod2)

#estimate 3PL models, but use a normal prior since the 'g' parameters are coded internally on a logit scale
withoutprior <- mirt(lsat, 1, itemtype = '3PL')
coef(withoutprior, simplify=TRUE)

#apply a norm (i.e., logit) prior to better control g parameters
model <- mirt.model('Theta = 1-5
                    PRIOR = (1-5, g, norm, -1.5, 3)')
withprior <- mirt(lsat, model, itemtype = '3PL')
coef(withprior, simplify=TRUE)

#create simulated data for Rasch model
a <- matrix(1, nrow=8, ncol=1)
d <- matrix(rnorm(8, 0, 0.5))

set.seed(1234)
newdata <- simdata(a, d, N = 10000, itemtype = 'dich')
coef(mod <- mirt(newdata, 1, itemtype = 'Rasch'), simplify=TRUE)
data.frame(a=a,d=d)

# -------------------------------------------------------
# modifying function inputs

# MHRM and quasi-Monte Carlo EM with Newton-Raphson optimizer
QMCEM <- mirt(lsat, 2, method = 'QMCEM', optimizer = 'NR')
MHRM <- mirt(lsat, 2, method = 'MHRM') #MHRM always is NR

QMCEM
MHRM

#decrease convergence tolerance, disable acceleration, and increase number of max computing cycles
mod <- mirt(lsat, 1, SE = TRUE, TOL = 1e-15,
            accelerate = 'none',
            technical = list(NCYCLES = 1000))

# do some Wald tests, test whether first and second slopes are equal to 0
help(wald)
wald(mod)
L <- matrix(0, 2, 10)
L[1,1] <- 1; L[2,3] <- 1
wald(mod, L = L)

# test whether first and last item intercepts are different
L <- matrix(0, 1, 10)
L[1,2] <- 1; L[1,10] <- -1
wald(mod, L=L)
