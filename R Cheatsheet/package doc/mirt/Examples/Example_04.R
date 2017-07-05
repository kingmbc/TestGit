#----------------------------------------------------
# Example 4 - Multiple group estimation, DIF, and DTF
#----------------------------------------------------

library(mirt)

#using lsat7 data, and creating a group variable
lsat <- expand.table(LSAT7)
set.seed(1234)
group <- sample(c('group.1', 'group.2'), 1000, replace=TRUE)
xtabs(~group)

#completely independent model
help(multipleGroup)
(mod <- multipleGroup(lsat, model=1, group=group))
coef(mod, simplify=TRUE)

# theoretically the same as:
# mod1 <- mirt(lsat[group == 'group.1', ], 1)
# coef(mod1)
# mod2 <- mirt(lsat[group == 'group.2', ], 1)
# coef(mod2)
# mod1@logLik + mod2@logLik

#plot curves between groups
plot(mod)
plot(mod, type = 'trace')
plot(mod, type = 'info')
plot(mod, type = 'RE')

itemplot(mod, 2)
itemplot(mod, 2, type = 'info')
itemplot(mod, 2, type = 'RE')

## model/item fit also applicable here
itemfit(mod)
M2(mod)

## --------------
# More constrained MG models

# constrain only the intercepts to be equal across groups
mod.int <- multipleGroup(lsat, model=1, group=group, invariance='intercepts')
coef(mod.int, simplify=TRUE)

#check whether significantly worse fit?
anova(mod, mod.int)

# same model above could have been run with the CONSTRAINB syntax
model <- mirt.model('theta = 1-5
                    CONSTRAINB = (1-5, d)')
mod.int2 <- multipleGroup(lsat, model=model, group=group)
anova(mod.int, mod.int2)

#fit a new model, constraining the intercepts and slopes to be equal across groups, while
#   freeing the latent mean and variance.
mod.freegroup <- multipleGroup(lsat, model=1, group=group,
                         invariance=c('intercepts', 'slopes', 'free_means', 'free_varcov'))
coef(mod.freegroup, simplify=TRUE)

# ------------------------

# The mod.freegroup is one good starting point for locating DIF.
#  - completely constrain the groups, but freely estimate the group hyper-parameters
#  - gradually add more free parameters to the model to see if the fit gets better

itemnames <- colnames(lsat)
mod.drop5 <- multipleGroup(lsat, model=1, group=group,
                          invariance=c(itemnames[1:4], 'free_means', 'free_varcov'))
anova(mod.freegroup, mod.drop5) #didn't get worse, no DIF

# One could also start the opposite way:
#  - freeing the latent mean and variance but defining an small set of 'anchor' items
#  - 'add' constraints to see if the model gets worse

# use items 1 and 2 as anchor
model <- mirt.model('F = 1-5
                    CONSTRAINB = (1-2, a1), (1-2, d)')
mod.freegroup2 <- multipleGroup(lsat, model=model, group=group,
                                invariance=c('free_means', 'free_varcov')) #or itemnames[1:2]
mod.add3 <- multipleGroup(lsat, model=1, group=group,
                                invariance=c('free_means', 'free_varcov', itemnames[1:3]))
anova(mod.freegroup2, mod.add3) #maybe?

##---------------------------------
# manually DIF testing can become laborious, especially if DIF is being done in an exploratory way
# So, available is an automated function for testing differential item functioning
help(DIF)

mirtCluster() ## to run in parallel
DIF(mod.freegroup2, which.par = c('a1', 'd'), items2test = 3:5) #add constrains
DIF(mod.freegroup, which.par = c('a1', 'd'), items2test = 3:5, scheme = 'drop') #drop constraints

# add constrains sequentially, updating the baseline model after DIF is found
DIF(mod.freegroup2, which.par = c('a1', 'd'), items2test = 3:5, scheme = 'add_sequential',
    seq_stat = .1) # add if p < .10


### ---------------------------------------
# DIF is fun, but after DIF is found the effects at the test level should be observed

# in this example, lets simulate some Rasch data so that we know whats going on
set.seed(1234)
nitems <- 15
d1 <- d2 <- matrix(rnorm(nitems,0,.7),ncol=1)
d2[1:3, ] <- d2[1:3, ] + c(.5, 1, 1.5)
d2[4:6, ] <- d2[4:6, ] - c(.5, 1, 1.5)
data.frame(d.group1 = d1, d.group2 = d2)

itemtype <- rep('dich', nitems)
N <- 500
dataset1 <- simdata(matrix(1,nitems), d1, N, itemtype)
dataset2 <- simdata(matrix(1,nitems), d2, N, itemtype, mu = .25, sigma = matrix(1.5))
dat <- rbind(dataset1, dataset2)
group <- c(rep('D1', N), rep('D2', N))

model <- mirt.model('Theta = 1-15
                    CONSTRAINB = (7-15, d)')
mod <- multipleGroup(dat, model, group, itemtype = 'Rasch', invariance = 'free_means', SE=TRUE)
coef(mod, simplify=TRUE)
plot(mod, type = 'trace', which.items = 1:6)
DIF(mod, 'd', items2test = 1:6)

# however, looking at the expected score plot, some cancellation appears to be occurring
plot(mod)

help(DTF)
DTF(mod)
DTF(mod, MI=1000)
DTF(mod, MI=1000, plot=TRUE)

# evaluate DTF statistics across upper and lower dist
DTF(mod, MI=1000, theta_lim = c(-6,0))
DTF(mod, MI=1000, theta_lim = c(0,6))
