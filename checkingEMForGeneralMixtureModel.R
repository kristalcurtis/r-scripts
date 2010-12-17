# check computeLogLikelihood - compare with mclust's value (returned from em method)

library(mclust)

observationsVector = op3$latency_ms
k=4

# used lines from fitEMGaussian in emFitAndSample.R to get emRun

names(emRun)
emRun$loglik


# make params list like my function is expecting
params=list(mixingProportions=emRun$parameters$pro, mean=emRun$parameters$mean, sd=sqrt(emRun$parameters$variance$sigmasq))


computeLoglik(op3$latency_ms, params, "normal")


# check eStep function against mclust's estep
library(mclust)
estep = estep(modelName="V", data=observationsVector, parameters=mstepRun$parameters)
names(estep)
estep$z[1:10,]

params=list(mixingProportions=mstepRun$parameters$pro, mean=mstepRun$parameters$mean, sd=sqrt(mstepRun$parameters$variance$sigmasq))

eStep = eStep(op3$latency_ms, params, "normal")
eStep[1:10,]

eStep == estep$z # no, b/c of precision.  but, = up to printable precision
weights=eStep

estep$z[100:110,]
eStep[100:110,]



# figuring out how to weight data
weightedData = vector()

mixtureComponent=3
for (i in 1:length(op3$latency_ms)) {
	weightedData = c(weightedData, rep(op3$latency_ms[i], floor(weights[i,mixtureComponent]*10)))
}

length(weightedData)

mean(weightedData)

# compare to MLE formula
length(weights[,mixtureComponent])
sum(weights[,mixtureComponent]*op3$latency_ms)/sum(weights[,mixtureComponent])

# looks good

# using weightData function
weightedData = weightData(op3$latency_ms, weights[,3], multiplier=10)
mean(weightedData)


# keeping this b/c i may want to copy & paste it
	if (distrType == "normal") {
	} else if (distrType == "exponential") {
	} else if (distrType == "gamma") {
	} else if (distrType == "weibull") {
	} else {
		print("Unsupported distr type.")
	}



# check mStep
params = mStep(op3$latency_ms, weights, "normal", 10)
params

# compare to mclust's mstep
mstep = mstep(modelName="V", data=op3$latency_ms, z=estep$z)
names(mstep)
mstep$parameters
sqrt(mstep$parameters$variance$sigmasq)


# checking initEM
initEM(op3$latency_ms[1:10], 4)

# checking overall alg
em(op3$latency_ms, 4, "normal", "random", 10)

# compare against mclust's em
names(emRun)
emRun$loglik


# try fit with mixture of gamma
gammaParams=em(op3$latency_ms, 4, "gamma", "random", 10)


# check sampleFromMixtureModel
params=list(mixingProportions=c(.2,.8), mean=c(5,25), sd=c(0.5,5))
samples = sampleFromMixtureModel("normal", params, 1000)
hist(samples, breaks=100)

# 2 => both means go to around 25
# 5 => some means go to around 5 too
emRun = em(samples, 5, "normal", "random", 10)
emSamples = sampleFromMixtureModel("normal", emRun$params, 1000)

par(mfrow=c(2,1))
hist(samples, breaks=100)
hist(emSamples, breaks=100)

emRun$params$mean

# try out new init functions on test data
source("emForGeneralMixtureModel.R")
emRun.evenlySpacedInit = em(samples, 2, "normal", "evenlySpaced")
emRun.evenlySpacedInit$params$mean

emRun.tailFocusedInit = em(samples, 2, "normal", "tailFocused")
emRun.tailFocusedInit$params$mean

# try on real data
emRun.op3 = em(op3$latency_ms, 3, "normal", "evenlySpaced")
emRun.op3
plot(density(op3$latency_ms))

for (i in 1:length(emRun.op3$mixingProportions)) {
	abline(v=emRun.op3$params$mean, lw=2, col="red")
}

# cf to samples
par(mfrow=c(2,1))

op3MMSamples=sampleFromMixtureModel("normal", emRun.op3$params, 1000)
hist(op3MMSamples, xlim=c(0,300))
quantile(op3MMSamples,0.99)
quantile(op3$latency_ms,0.99)

# try gamma
emRun.op3 = em(op3$latency_ms, 3, "gamma", "tailFocused")
par(mfrow=c(2,1))
plot(density(op3$latency_ms))
plot(density(sampleFromMixtureModel("gamma", emRun.op3$params, 1000)))

par(mfrow=c(4,1))
plot(density(sampleFromMixtureModel("gamma", emRun.op3$params, 1000)))
hist(sampleFromMixtureModel("gamma", emRun.op3$params, 1000), breaks=100)
plot(density(rgamma(1000, shape=emRun.op3$params$shape[1], rate=emRun.op3$params$rate[1])))
plot(density(rgamma(1000, shape=emRun.op3$params$shape[2], rate=emRun.op3$params$rate[2])))
plot(density(rgamma(1000, shape=emRun.op3$params$shape[3], rate=emRun.op3$params$rate[3])))

summary(rgamma(1000, shape=emRun.op3$params$shape[1], rate=emRun.op3$params$rate[1]))
summary(rgamma(1000, shape=emRun.op3$params$shape[2], rate=emRun.op3$params$rate[2]))
summary(rgamma(1000, shape=emRun.op3$params$shape[3], rate=emRun.op3$params$rate[3]))


hist(sampleFromMixtureModel("gamma", emRun.op3$params, 1000), breaks=100, xlim=c(50,300), ylim=c(0,50))
summary(sampleFromMixtureModel("gamma", emRun.op3$params, 1000))
par(mfrow=c(2,1))
hist(op3$latency_ms[op3$latency_ms>50], breaks=100)

#plot(density(c(rgamma(1000, shape=emRun.op3$params$shape[1], rate=emRun.op3$params$rate[1]), rgamma(1000, shape=emRun.op3$params$shape[2], rate=emRun.op3$params$rate[2]), rgamma(1000, shape=emRun.op3$params$shape[3], rate=emRun.op3$params$rate[3]))))

# making em more efficient
# check weightData
source("emForGeneralMixtureModel.R")
length(weightData(c(1,3,5,7,9), c(.2, .3, .2, .8, .4), multiplier=10))

?apply
repPts = function(pt) {
	return(rep(pt, pt))
}
data=matrix(c(1,3,5,7,9), nrow=5,ncol=1)
dim(data[drop=FALSE])
as.vector(apply(data[drop=FALSE], 1, repPts))

pts = apply(data[drop=FALSE], 1, repPts)
pts
pts[[2]]
unlist(pts)

data=op3$latency_ms[1:20]
data=matrix(data, nrow=length(data),ncol=1)
weights=c(rep(0.32,4), rep(0.21,4), rep(0.72,4), rep(0.56,3), rep(0.44,5))
weightData(data, weights, 10)


# testing new init
data=op3$latency_ms
numMixtureComponents=4

# try new init
source("emForGeneralMixtureModel.R")
emRun.op3 = em(op3$latency_ms, 3, "gamma", "evenlySpacedByQuantile")
gammaSamples = sampleFromMixtureModel("gamma", emRun.op3$params, 10000)
hist(gammaSamples, breaks=100)
quantile(gammaSamples, 0.99)
quantile(op3$latency_ms, 0.99)


par(mfrow=c(2,1))
hist(op3$latency_ms, breaks=100)
values=op3$latency_ms
abline(v=mean(values), lw=2, col="purple")
abline(v=median(values), lw=2, col="red")
abline(v=quantile(values,0.9), lw=2, col="blue")
abline(v=quantile(values,0.99), lw=2, col="green")
legend("topright", legend=c(paste("mean=", round(mean(values), digits=2)), paste("median=", round(median(values), digits=2)), paste("90th=", round(quantile(values, 0.9), digits=2)), paste("99th=", round(quantile(values, 0.99), digits=2))), col=c("purple", "red", "blue", "green"), lw=2)

hist(gammaSamples, breaks=30, xlim=range(op3$latency_ms))
values = gammaSamples
abline(v=mean(values), lw=2, col="purple")
abline(v=median(values), lw=2, col="red")
abline(v=quantile(values,0.9), lw=2, col="blue")
abline(v=quantile(values,0.99), lw=2, col="green")
legend("topright", legend=c(paste("mean=", round(mean(values), digits=2)), paste("median=", round(median(values), digits=2)), paste("90th=", round(quantile(values, 0.9), digits=2)), paste("99th=", round(quantile(values, 0.99), digits=2))), col=c("purple", "red", "blue", "green"), lw=2)


# checking dMixtureModel
op3gammaDensity = dMixtureModel(sort(unique(op3$latency_ms)), "gamma", emRun.op3$params)
plot(op3gammaDensity, type="l", xlim=c(0,50))
plot(op3gammaDensity, type="l")
plot(density(op3$latency_ms), xlim=c(0,50))

op3gammaDensity = dMixtureModel(seq(from=0,to=300,by=0.01), "gamma", emRun.op3$params)


hist(gammaSamples, breaks=100)
hist(op3$latency_ms, xlim=c(0,140), breaks=250)

source("convolutionForOpCombination-functions.R")
getQuantileFromDensity(op3gammaDensity, 0.99)
quantile(op3$latency_ms, 0.99)

quantile(gammaSamples,0.99)