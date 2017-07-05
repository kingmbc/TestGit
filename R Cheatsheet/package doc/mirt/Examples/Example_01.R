#-----------------------------------
# Example 1 -- Introduction to mirt
#-----------------------------------

# install.packages('mirt') #install mirt from CRAN

# or, if you are feeling wild, install the dev version (requires additional compilers)
# install.packages('devtools')
# devtools::install_github('mirt', 'philchalmers')

library(mirt) #make mirt package available

#expand dataset since LSAT7 is in tabular form (see ?expand.table)
help(LSAT7)
head(LSAT7)
lsat <- expand.table(LSAT7)
lsat[sample(1:1000, 10), ]

# refer to documentation for mirt() inputs
help(mirt)

#one factor, 2PL default item types (2PL)
twoPL <- mirt(lsat, 1)

help('coef-method')
coef(twoPL) #in slope-intercept form, use b = -d/a to obtain traditional metric
(b2 <- -0.808/1.081)
#original IRT metric for all items can be obtained using
coef(twoPL, IRTpars = TRUE, simplify=TRUE)

#3PL model for first 2 items, 2PL for remaining
threePL <- mirt(lsat, 1, itemtype=c('3PL','3PL','2PL','2PL','2PL'))

#gessing parameter for item 1 hits the lower bound of 0, use a logit prior for g parameters instead
# (learn more about this later)
coef(threePL, simplify=TRUE)

#imposed tracelines
help('plot-method')
plot(twoPL, type = 'trace')
plot(twoPL, type = 'trace', auto.key = FALSE) #without legend

# does the model fit better?
help('anova-method')
anova(twoPL, threePL)

# estimate with ideal point models
ideal <- mirt(lsat, 1, itemtype = 'ideal')
plot(ideal, type = 'trace', which.items = 1:3)

#-------------------------------------------------------------------------
# Polytomous models
help(Science)
head(Science)

# graded response, generalized partial credit, and nominal models
mod_graded <- mirt(Science, 1, verbose = FALSE)
mod_gpcm <- mirt(Science, 1, itemtype = 'gpcm', verbose = FALSE)
mod_nominal <- mirt(Science, 1, itemtype = 'nominal', verbose = FALSE)

plot(mod_graded)
plot(mod_graded, type = 'trace')

help(itemplot)
itemplot(mod_graded, 1)
itemplot(mod_graded, 1, type = 'score')

coef(mod_graded, simplify=TRUE)
coef(mod_gpcm, simplify=TRUE)
coef(mod_nominal, simplify=TRUE)

# classical IRT parameters
coef(mod_graded, simplify=TRUE, IRTpars = TRUE)
coef(mod_gpcm, simplify=TRUE, IRTpars = TRUE)
coef(mod_nominal, simplify=TRUE, IRTpars = TRUE)
