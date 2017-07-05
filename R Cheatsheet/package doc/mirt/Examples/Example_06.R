#----------------------------------------------------
#Example 6
#----------------------------------------------------

# latent regression model with mirt()

#simulate data
set.seed(1234)
N <- 1000

# covariates
X1 <- rnorm(N); X2 <- rnorm(N)
covdata <- data.frame(X1, X2)
Theta <- matrix(0.5 * X1 + -1 * X2 + rnorm(N, sd = 0.5))

#items and response data
a <- matrix(1, 20); d <- matrix(rnorm(20))
dat <- simdata(a, d, 1000, itemtype = 'dich', Theta=Theta)

#unconditional Rasch model
mod0 <- mirt(dat, 1, 'Rasch')

#conditional model using X1 and X2 as predictors of Theta
mod1 <- mirt(dat, 1, 'Rasch', covdata=covdata, formula = ~ X1 + X2)
coef(mod1, simplify=TRUE)
anova(mod0, mod1)

#bootstrapped confidence intervals
boot.mirt(mod1, R=5)

#draw plausible values for secondary analyses
help(fscores)
pv <- fscores(mod1, plausible.draws = 5)
pvmods <- lapply(pv, function(x, covdata) lm(x ~ covdata$X1 + covdata$X2),
                 covdata=covdata)
#population characteristics recovered well, and can be averaged over
so <- lapply(pvmods, summary)
so

# compute Rubin's multiple imputation average
par <- lapply(so, function(x) x$coefficients[, 'Estimate'])
SEpar <- lapply(so, function(x) x$coefficients[, 'Std. Error'])
averageMI(par, SEpar)



###################################################
# mixed effects models with mixedmirt()

#make some arbitrary data to analyze
library(mirt)
set.seed(1234)
N <- 750
a <- matrix(rlnorm(10,.3,1),10,1)
d <- matrix(rnorm(10), 10)
Theta <- matrix(sort(rnorm(N)))
pseudoIQ <- scale(Theta * 5 + 100  + rnorm(N, 0 , 5))
group <- factor(rep(c('G1','G2','G3'), each = N/3))
data <- simdata(a,d,N, itemtype = rep('dich',10), Theta=Theta)
covdata <- data.frame(group, pseudoIQ)

head(data) #typical data
head(covdata) #person level covariates

# set up parallel computing
mirtCluster()

#specify IRT model
model <- mirt.model('Theta = 1-10')

#estimate a model with no person predictors (unconditional model)
mod0 <- mirt(data, model, itemtype = 'Rasch')

#group as a fixed effect predictor
help(mixedmirt)
mod1 <- mixedmirt(data, covdata, model, fixed = ~ 0 + group + items)
anova(mod0, mod1)
summary(mod1)
coef(mod1)

#model using 2PL items instead of Rasch
mod1b <- mixedmirt(data, covdata, model, fixed = ~ 0 + group + items, itemtype = '2PL')
anova(mod1, mod1b) #much better with 2PL models using all criteria (as expected, given simdata pars)

#continuous predictor and interaction model with group
mod2 <- mixedmirt(data, covdata, model, fixed = ~ 0 + group + pseudoIQ + items)
mod3 <- mixedmirt(data, covdata, model, fixed = ~ 0 + group * pseudoIQ + items)
summary(mod2)
summary(mod3)
anova(mod1b, mod2)
anova(mod2, mod3)

###################################################
##LLTM, and 2PL version of LLTM using SAT12 dataset

data(SAT12)
data <- key2binary(SAT12,
                   key = c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5))
model <- mirt.model('Theta = 1-32')

itemdesign <- data.frame(itemorder = factor(c(rep('easier', 16), rep('harder', 16))))
itemdesign

#notice that the 'fixed = ~ ... + items' argument is omitted
LLTM <- mixedmirt(data, model = model, fixed = ~ 0 + itemorder, itemdesign = itemdesign)
summary(LLTM)
coef(LLTM)
wald(LLTM)
L <- matrix(c(-1, 1, 0), 1)
wald(LLTM, L) #first half potentially different from second

#compare to items with estimated slopes (2PL)
twoPL <- mixedmirt(data, model = model, fixed = ~ 0 + itemorder, itemtype = '2PL',
                   itemdesign = itemdesign, technical = list(NCYCLES = 3000))
anova(twoPL, LLTM) #much better fit
summary(twoPL)
coef(twoPL)

wald(twoPL)
L <- matrix(0, 1, 34)
L[1, 1] <- 1
L[1, 2] <- -1
wald(twoPL, L) #n.s.

##LLTM with item error term
LLTMwithError <- mixedmirt(data, model = model, fixed = ~ 0 + itemorder, random = ~ 1|items,
                           itemdesign = itemdesign, technical = list(NCYCLES = 3000))
summary(LLTMwithError)
#large item level variance after item-order is regressed; not a great predictor of item difficulty
coef(LLTMwithError)
