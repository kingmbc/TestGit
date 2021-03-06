packages.list = c("ROCR", "merror")

for(i in 1:length(packages.list))
{
  install.packages(packages.list[i])
  require(packages.list[i], character.only = TRUE)
}
library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf)

## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)

## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)

data(pm2.5)

# Make various calibration plots for pm2.5 measurements
par(mfrow=c(2,2))
cplot(pm2.5,2,1)
cplot(pm2.5,3,1)
cplot(pm2.5,4,1)
# Add the naive regression lines JUST for comparison
cplot(pm2.5,5,1,regress=TRUE,t.size=0.9)

# This is redundant but illustrates using the
# argument alpha.beta.sigma
a <- ncb.od(pm2.5)$sigma.table$alpha.ncb[1:5]
b <- ncb.od(pm2.5)$sigma.table$beta[1:5]
s <- ncb.od(pm2.5)$sigma.table$sigma[1:5]

alpha.beta.sigma <- t(data.frame(a,b,s))

cplot(pm2.5,2,1,alpha.beta.sigma=alpha.beta.sigma)
cplot(pm2.5,2,1,alpha.beta.sigma=alpha.beta.sigma,regress=TRUE)
