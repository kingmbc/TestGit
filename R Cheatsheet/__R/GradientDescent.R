##########################################################################
## http://vis.supstat.com/2013/03/gradient-descent-algorithm-with-r/
##########################################################################
install.packages("animation")
library(animation)
par(mar = c(4, 4, 2, 0.1))
grad.desc()

ani.options(nmax = 70)
par(mar = c(4, 4, 2, 0.1))
f2 = function(x, y) sin(1/2 * x^2 - 1/4 * y^2 + 3) * cos(2 * x + 1 - 
                                                           exp(y))
grad.desc(f2, c(-2, -2, 2, 2), c(-1, 0.5), gamma = 0.3, tol = 1e-04)

##########################################################################
## http://digitheadslabnotebook.blogspot.kr/2012/07/linear-regression-by-gradient-descent.html
## http://digitheadslabnotebook.blogspot.kr/2011/10/machine-learning-gradient-descent.html
##########################################################################
# generate random data in which y is a noisy function of x
x <- runif(1000, -5, 5)
y <- x + rnorm(1000) + 3

# fit a linear model
res <- lm( y ~ x )
print(res)

# plot the data and the model
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
abline(res, col='blue')

# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# learning rate and iteration limit
alpha <- 0.01
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

print(theta)

# plot data and converging fit
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue')

plot(cost_history, type='line', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')


##########################################################################
## http://www.r-bloggers.com/gradient-descent-in-r/
##########################################################################

#  ----------------------------------------------------------------------------------
# |PROGRAM NAME: gradient_descent_R
# |DATE: 11/27/11
# |CREATED BY: MATT BOGARD 
# |PROJECT FILE:              
# |----------------------------------------------------------------------------------
# | PURPOSE: illustration of gradient descent algorithm
# | REFERENCE: adapted from : http://www.cs.colostate.edu/~anderson/cs545/Lectures/week6day2/week6day2.pdf                
# | 
#  ---------------------------------------------------------------------------------

xs <- seq(0,4,len=20) # create some values

# define the function we want to optimize
f <-  function(x) {
  1.2 * (x-2)^2 + 3.2
}

# plot the function 
plot(xs , f (xs), type="l",xlab="x",ylab=expression(1.2(x-2)^2 +3.2))

# calculate the gradeint df/dx
grad <- function(x){
  1.2*2*(x-2)
}


# df/dx = 2.4(x-2), if x = 2 then 2.4(2-2) = 0
# The actual solution we will approximate with gradeint descent
# is  x = 2 as depicted in the plot below
lines (c (2,2), c (3,8), col="red",lty=2)
text (2.1,7, "Closedform solution",col="red",pos=4)


# gradient descent implementation
x <- 0.1 # initialize the first guess for x-value
xtrace <- x # store x -values for graphing purposes (initial)
ftrace <- f(x) # store y-values (function evaluated at x) for graphing purposes (initial)
stepFactor <- 0.6 # learning rate 'alpha'
for (step in 1:100) {
  x <- x - stepFactor*grad(x) # gradient descent update
  xtrace <- c(xtrace,x) # update for graph
  ftrace <- c(ftrace,f(x)) # update for graph
}

lines ( xtrace , ftrace , type="b",col="blue")
text (0.5,6, "Gradient Descent",col="blue",pos= 4)

# print final value of x
print(x) # x converges to 2.0
