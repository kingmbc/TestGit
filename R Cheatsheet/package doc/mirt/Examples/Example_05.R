#----------------------------------------------------
#Example 5
#----------------------------------------------------

library(mirt)


## custom prior
den <- function(Theta, Etable) dunif(Theta, min = -6, max = 6)
Theta <- matrix(seq(-6, 6, length.out = 100))
custom_mod <- mirt(Science, 1, technical =
                       list(customPriorFun = den, customTheta = Theta))
coef(custom_mod, simplify=TRUE)$items

# compare with N(0,1)
mod <- mirt(Science, 1)
coef(mod, simplify=TRUE)

## custom item
P.old2PL <- function(par, Theta, ncat){
    a <- par[1]; b <- par[2]
    P1 <- 1 / (1 + exp(-1*a*(Theta - b)))
    cbind(1-P1, P1)
}
x <- createItem('old2PL', par = c(a = .5, b = -2), est =
                    c(TRUE, TRUE), P = P.old2PL)
dat <- expand.table(LSAT7)
custom_mod <- mirt(dat, 1, itemtype = 'old2PL',
                   customItems = list(old2PL = x))
coef(custom_mod, simplify=TRUE)$items

# compare
mod <- mirt(dat, 1)
coef(mod, simplify=TRUE, IRTpars=TRUE)

##---------------------------------------------------------
### starting values

dat <- expand.table(LSAT7)
sv <- mirt(expand.table(LSAT7), 1, pars = 'values')
head(sv)
sv$value[1] <- 1 #change starting slope to 1
mod <- mirt(dat, 1, pars = sv)

# following convergence, can use mod2values
help(mod2values)
(values <- mod2values(mod)) #converged model
mod2 <- mirt(dat, 1, pars = values, TOL = 1e-5) #use 'values' as starting values

#----------------------------------------------------
# Empirical histograms

# draws some data so we know what the latent density looks like
set.seed(1234)
a <- matrix(rlnorm(50, .2, .2))
d <- matrix(rnorm(50))
ThetaNormal <- matrix(rnorm(2000))
ThetaBimodal <- scale(matrix(c(rnorm(1000, -2), rnorm(1000,2)))) #bimodal
ThetaSkew <- scale(matrix(rchisq(2000, 3))) #positive skew
datNormal <- simdata(a, d, 2000, itemtype = 'dich', Theta=ThetaNormal)
datBimodal <- simdata(a, d, 2000, itemtype = 'dich', Theta=ThetaBimodal)
datSkew <- simdata(a, d, 2000, itemtype = 'dich', Theta=ThetaSkew)

normal <- mirt(datNormal, 1, empiricalhist = TRUE)
plot(normal, type = 'empiricalhist')
histogram(ThetaNormal, breaks=30)

bimodal <- mirt(datBimodal, 1, empiricalhist = TRUE)
plot(bimodal, type = 'empiricalhist')
histogram(ThetaBimodal, breaks=30)

skew <- mirt(datSkew, 1, empiricalhist = TRUE)
plot(skew, type = 'empiricalhist', npts=500)
histogram(ThetaSkew, breaks=30)

# MG empirical histogram
set.seed(1234)
a <- matrix(rlnorm(50, .2, .2))
d <- matrix(rnorm(50))
ThetaNormal <- matrix(rnorm(2000))
ThetaBimodal <- scale(matrix(c(rnorm(1000, -2), rnorm(1000,2)))) #bimodal
Theta <- rbind(ThetaNormal, ThetaBimodal)
dat <- simdata(a, d, 4000, itemtype = 'dich', Theta=Theta)
group <- rep(c('G1', 'G2'), each=2000)
EH <- multipleGroup(dat, 1, group=group, empiricalhist = TRUE, invariance = colnames(dat))

histogram(~ThetaNormal, breaks=30)
histogram(~ThetaBimodal, breaks=30)
plot(EH, type = 'empiricalhist', npts = 60)
