###############################################################
# Log-linear Model and Analysis
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/loglin.html
# https://ww2.coastal.edu/kingw/statistics/R-tutorials/loglin.html
###############################################################

data(HairEyeColor);
describe(HairEyeColor)
head(HairEyeColor)

## Model of joint independence of sex from hair and eye color.
fm <- loglin(HairEyeColor, list(c(1, 2), c(1, 3), c(2, 3)))
fm
1 - pchisq(fm$lrt, fm$df)
## Model with no three-factor interactions fits well.




#generating the data
n=500
x <- 1:n
set.seed(10)
y <- 1*log(x)-6+rnorm(n)

#plot the data
plot(y~x)

#fit log model
fit <- lm(y~log(x))
#Results of the model
summary(fit)

coef(fit)

#plot 
x=seq(from=1,to=n,length.out=1000)
y=predict(fit,newdata=list(x=seq(from=1,to=n,length.out=1000)), interval="confidence")
head(y)
matlines(x,y,lwd=2)