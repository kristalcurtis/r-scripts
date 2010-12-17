# Try using mixdist for weibull param estimation

library(mixdist)

?weibullpar

failures = c(16, 34, 53, 75, 93, 120)

mean=mean(failures)

sigma=sd(failures)

params=weibullpar(mean, sigma, 5)


# another option from PB
# disadv:  doesn't consider location param
library(MASS)
params=fitdistr(failures, "weibull")


# using optim function to optimize wrt all 3 params


weibull.loglik = function(params, y) {
	print(params)
	
	k = params[1] # shape
	lambda = params[2] # scale
	theta = params[3] # location

	n = length(y)
	
	loglik = n * log(k/lambda) + sum((k-1) * log((y-theta)/lambda) - ((y-theta)/lambda)^k)
	print(loglik)
	
	return(-loglik)
}

optim(c(1, 50, 5), weibull.loglik, y=failures, method="L-BFGS-B")
optim(c(1, 50, 0), weibull.loglik, y=failures, method="L-BFGS-B")

optim(c(2, 73.6, 0), weibull.loglik, y=failures, method="L-BFGS-B", upper=c(Inf, Inf, 0.95*failures[1]))


weibull.loglik(c(1.05, 49.29, 16.26), failures)

# checking weibull.loglik function
params=c(1.05, 49.29, 16.26)
y=failures


# try mle
library(stats4)
?mle

mle(weibull.loglik, y=failures)


