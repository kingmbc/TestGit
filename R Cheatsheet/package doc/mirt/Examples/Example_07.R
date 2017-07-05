#----------------------------------------------------
#Example 7
#----------------------------------------------------

library(mirt)

# SE computations only needed for imputation plot
mod1 <- mirt(Science, 1, SE = TRUE)
mod2 <- mirt(Science, 2)

plot(mod1, type = 'infotrace')
itemplot(mod1, 3, type = 'info')
itemplot(mod1, 3, type = 'info', CE = TRUE)
itemplot(mod1, 3, type = 'infoSE')
itemplot(mod1, 3, type = 'infotrace')

itemplot(mod2, 3, type = 'info')
itemplot(mod2, 3, type = 'infocontour')


#-------------------------------------------------------------

# latent trait/factor scores

help(fscores)

dat <- expand.table(LSAT7)
mod <- mirt(dat, 1)
mod2 <- mirt(dat, mirt.model('F1 = 1-3
                             F2 = 4-5
                             COV = F1 * F2'))

plot(mod, type = 'info')
plot(mod2, type = 'info')

# basic fscores inputs using EAP estimator
fscores(mod)
fscores(mod, full.scores=TRUE)
fscores(mod, full.scores=TRUE, full.scores.SE=TRUE)

#change method argument to use MAP estimates
fullscores <- fscores(mod, full.scores = TRUE, method='MAP')
head(fullscores)

#calculate MAP for a given response vector
fscores(mod, method='MAP', response.pattern = c(0,1,0,1,0))
#or matrix
fscores(mod, method='MAP', response.pattern = rbind(c(0,1,0,1,0),
                                                    c(1,1,0,1,1)))

#use custom latent variable properties (diffuse prior for MAP is very close to ML)
fscores(mod, method='MAP', cov = matrix(1000))
fscores(mod, method='ML') # the most diffuse prior!
fscores(mod, method='WLE') # i.e., Jeffreys prior

#multiple imputation using 30 draws for EAP scores. Requires information matrix
set.seed(1)
mod <- mirt(dat[sample(1:1000, 200), ], 1, SE=TRUE, SE.type = 'Louis')
fscores(mod)
fscores(mod, MI = 30)

# plausible values for future work
pv <- fscores(mod, plausible.draws = 5)
(out <- sapply(pv, function(x) c(mean=mean(x), var=var(x), min=min(x), max=max(x))))
rowMeans(out)

## define a custom_den function. MAP with a uniform prior between -3 and 3
fun <- function(Theta, ...) as.numeric(dunif(Theta, min = -3, max = 3))
head(fscores(mod, custom_den = fun, method = 'MAP'))



## -----------------------------------------------------------
# change implicit calls to fscores() in other functions

Rasch <- mirt(dat, 1, 'Rasch')

# item fit
itemfit(Rasch)
itemfit(Rasch, method = 'WLE')

# person fit
head(personfit(Rasch, method = 'MAP'))

# Q3 co-dependence statistic
residuals(Rasch, type = 'Q3')
residuals(Rasch, type = 'Q3', method = 'MAP')
