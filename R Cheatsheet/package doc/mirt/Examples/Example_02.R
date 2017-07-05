#----------------------------------------------------------------
# Example 2 - Multidimensional IRT, itemtypes, and fit statistics
#----------------------------------------------------------------

library(mirt)

#use SAT12 data, adopted from the TESTFACT software, which has not yet been scored.
#  (treat the 8's, which are the code for missing, as incorrect for now)

help(SAT12)
head(SAT12)
key <- c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5)
data <- key2binary(SAT12, key = key)
head(data)

#extract 1 and 2 traits (exploratory) with 3PL models, but fix guessing parameters to .1
mod1 <- mirt(data, 1, guess = .1)
mod2 <- mirt(data, 2, guess = .1)

mod1
mod2

#compare models
anova(mod1, mod2)

#summary function provides rotating options/factor loadings metric
help('summary-method')
summary(mod1)
summary(mod2)
summary(mod2, rotate = 'varimax')
summary(mod2, suppress = .2)

# coef
coef(mod2)
coef(mod2, simplify=TRUE)
coef(mod2, simplify=TRUE, rotate = 'oblimin') #also accepts rotate option

# some 3d item plots
itemplot(mod1, 1)
itemplot(mod2, 1)
itemplot(mod2, 1, rotate = 'oblimin')

# define a confirmatory model from summary(mod2, suppress = .2)
help(mirt.model)
model <- mirt.model('F1 = 1-11,13-32
                     F2 = 4,7,9-15,21,23-25,27,29
                     COV = F1*F2')
cmod <- mirt(data, model, guess = .1, SE=TRUE)
coef(cmod)
summary(cmod)

itemplot(cmod, 1)
itemplot(cmod, 1, drop.zeros = TRUE) #drop zeros from plot, lowers dimensions

#--------------------------------------------------------------------------

# item 12, 30, and 32 appears to have very small commonalities and should be investigated.
# For diagnostic reasons we should look to see if these have been miss-scored, or if
# the distractors provide any useful information to warrant keeping them in the test.
newdata <- data
newdata[,c(12, 30, 32)] <- as.matrix(SAT12[,c(12, 30, 32)])
newdata[newdata == 8] <- NA #treat the 8's as missing data
head(newdata)

#fit a nominal model to suspicious items, and 2PL with fixed guessing elsewhere
itemtype <- rep('2PL', 32)
pick <- c(12,30,32)
itemtype[pick] <- 'nominal'
itemtype
mod <- mirt(newdata, 1, itemtype = itemtype, guess = .1)
key[pick]
coef(mod)[pick]

itemplot(mod, 12) #generally poor, categories 3 and 4 appear to be functioning the same (both correct?)
itemplot(mod, 30) #correctly scored, distractor patterns give next to no information
itemplot(mod, 32) #looks to be incorrectly scored! key was 5, but 3rd category appears correct

# all at once
plot(mod, type = 'trace', which.items = c(12,30,32))

#--------------------
# possible to use more exploratory investigation with non-parametric kernel smoothing
library(KernSmoothIRT)
help(ksIRT)

# on scored data
ks_scored <- ksIRT(data, key=1, format=1)
plot(ks_scored, item=c(1,2,12,30,32))

# on hybrid data
key2 <- key
key2[-c(12,30,32)] <- 1
ks <- ksIRT(na.omit(newdata), key=key2, format=1)
plot(ks, item=c(1,12,30,32))

# fix the key according to what mirt suggested
key2[32] <- 3
ks <- ksIRT(na.omit(newdata), key=key2, format=1)
plot(ks, item=32)
ks <- ksIRT(key2binary(SAT12, key2), key=1, format=1)
plot(ks, item=32)

# Did TESTFACT provide the wrong SAT12 key for item 32? Does item 12 may contain 2 correct answers?
# Worth checking out the original items that were administered for clues.

###########
# same result when considering multidimensional model (run if you don't believe me!)
# newmodel to let suspicious items load on both factors, for diagnostic purposes
newmodel <- mirt.model('F1 = 1-32
                     F2 = 4,7,9-15,21,23-25,27,29,30,32
                     COV = F1*F2')
mod2 <- mirt(newdata, newmodel, itemtype = itemtype, guess = .1)
coef(mod2) #again, cagetory 3 (i.e., ak2) is the highest for item 32, indicating it is the most correct
###########

#use corrected key for item 32
key[32] <- 3
data <- key2binary(SAT12, key)
newmod <- mirt(data, 1, guess = .1)
coef(newmod)[['Item.32']]
summary(newmod, suppress = .2)
itemplot(newmod, 32)

#--------------------------------------------------------------------------
## Diagnostics

#### How well do the above models fit the data?

# pretty well actually
help(M2)
M2(mod1)
M2(mod2)

fscores(mod1, method = 'EAPsum')

# item fit statistics
help(itemfit)
(ii <- itemfit(mod1))
p.adjust(ii$p.S_X2, 'fdr')
(ii <- itemfit(newmod))
p.adjust(ii$p.S_X2, 'fdr')

# S-X2 total score tables
SX2_tabs <- itemfit(newmod, S_X2.tables = TRUE)
SX2_tabs$O.org[[1]]
SX2_tabs$O[[1]]
SX2_tabs$E[[1]]

# item residual covaration
help('residuals-method')
residuals(newmod)
residuals(newmod, tables=TRUE)

mod_Rasch <- mirt(data, 1, 'Rasch')
anova(mod_Rasch, mod1) #expect a bad fit given how much the slopes vary
M2(mod_Rasch)
itemfit(mod_Rasch, method = 'ML')
itemfit(mod_Rasch, method = 'ML', empirical.plot = 2, empirical.CI = .99)

