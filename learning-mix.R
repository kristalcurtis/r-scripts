library(mixdist)
?mix

rm(list=ls())
data(pike65)

pike65
?plot
plot(pike65, type="s")

data(pikepar)
pikepar

fitpike1 = mix(pike65, pikepar, "lnorm", constr=mixconstr(consigma="CCV"), emsteps=3)
names(fitpike1)

plot(fitpike1)  #wow!!!
fitted(fitpike1)

data(pike65sg)
pike65sg
fitpike2 = mix(pike65sg, pikepar, "lnorm", emsteps=3, usecondit=TRUE)
fitpike2
plot(fitpike2)

data(bindat)
bindat

data(binpar)
binpar

fitbin1 = mix(bindat, binpar, "binom", constr=mixconstr(consigma="BINOM", size=c(20,20,20,20)))
plot(fitbin1)

fitbin2 = mix(bindat, binpar, "binom", constr=mixconstr(conpi="PFX", fixpi=c(TRUE,TRUE,TRUE,TRUE), consigma="BINOM", size=c(20,20,20,20)))
plot(fitbin2)


# Looking at my data
load("~/Desktop/ops.RData")
h1=hist(op1$latency_ms)
names(h1)
h1$breaks # left side of break



h3=hist(op3$latency_ms, breaks=100)
op3GroupedData = matrix(c(h3$breaks, h3$counts, 0), nrow=length(h3$breaks), ncol=2)

plot(op3GroupedData, type="s")

length(h3$breaks)
length(h3$counts)


# using mixgroup
data(pikeraw)
pikeraw
colnames(pikeraw)
mixgroup(pikeraw)
pikemd = mixgroup(pikeraw, breaks=c(0,seq(19.75, 65.75, 2), 80))
plot(pikemd)

pikemd = mixgroup(pikeraw, breaks=c(0,seq(19.75, 65.75, 2), 80), usecondit=TRUE, k=5)
plot(pikemd)
mixgroup(pikeraw, usecondit=TRUE)
mixgroup(pikeraw, usecondit=TRUE, k=3)
mixgroup(pikeraw, usecondit=TRUE, k=8)


# trying mixgroup on my data
op1Grouped=mixgroup(op1$latency_ms)
quantile(op1$latency_ms, 0.99)
quantile(op1$latency_ms, 0.999)
quantile(op1$latency_ms, 0.9999)
# might want to try pred at 3 9's


# using mixparam
mixparam(mu=c(20,30,40), sigma=c(2,3,4))
mixparam(c(20,30,40), c(3), c(0.15, 0.78, 0.07))


# try using mixparam for my data
library(mclust)
source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")

op=op3
k=3

?mixgroup
opGrouped=mixgroup(op$latency_ms, breaks=100)

rdmInit = randomInitEM(length(op$latency_ms), k)
em = mstep(modelName="V", data=op$latency_ms, z=rdmInit)

params = em$parameters

# sort so means are in ascending order;
# use same order for sd, pi

sortedMean = sort(params$mean, index.return=TRUE)

paramsMeanSorted = matrix(c(sortedMean$x, sqrt(params$variance$sigmasq[sortedMean$ix]), params$pro[sortedMean$ix]), nrow=length(sortedMean$x), ncol=3)
colnames(paramsMeanSorted)=c("mean", "sd", "pi")


mixParam = mixparam(mu=paramsMeanSorted[,"mean"], sigma=paramsMeanSorted[,"sd"], pi=paramsMeanSorted[,"pi"])
mixParamA = mixparam(mu=paramsMeanSorted[,"mean"], sigma=paramsMeanSorted[,"sd"])



# try it out!
fit = mix(opGrouped, mixParam, dist="gamma", print.level=2, constr=mixconstr(consigma="CCV"), emsteps=0)
plot(fit)

# this works pretty well!
#mixParam=mixparam(mu=c(3, 17, 28), sigma=c(1,1,1))
mixParam=mixparam(mu=c(3, 25), sigma=c(2,12.5))
fit = mix(opGrouped, mixParam, dist="gamma", print.level=2, constr=mixconstr(consigma="SFX", fixsigma=c(TRUE,TRUE)), emsteps=0)
#fit = mix(opGrouped, mixParam, dist="gamma", print.level=2, emsteps=0)
plot(fit)

# this doesn't work yet
mixParam=mixparam(mu=c(3, 25, 26), sigma=c(1,1,1))
fit = mix(opGrouped, mixParam, dist="weibull", print.level=2, constr=mixconstr(consigma="CCV"), emsteps=0)
plot(fit)

names(fit)
fit$parameters$mu

# try sampling from the fitted distr
sampleFromGammaMixtureModel = function(params, numSamples) {
	numMixtures = length(params$mu)

	# Get scale, shape params for each component
	scale = vector(mode="numeric", length=numMixtures)
	shape = vector(mode="numeric", length=numMixtures)	
	for (i in 1:numMixtures) {
		shape = (params$mu[i]^2)/(params$sigma[i]^2)
		scale = params$mu[i]/shape
		
		scale[i] = scale
		shape[i] = shape
	}
	
	# Get samples
	mixture = vector(mode="numeric", length=numSamples)
	samples = vector(mode="numeric", length=numSamples)
	
	for (i in 1:numSamples) {
		# Get mixture component
		mixture[i] = sample(x=seq(from=1,to=numMixtures,by=1), size=1, prob=params$pi)
		
		# Get sample from that mixture's gamma
		samples[i] = rgamma(n=1, shape=shape[mixture[i]], scale=scale[mixture[i]])
	}
	
	return(samples)
	#return(mixture)
}


gammaSamples = sampleFromGammaMixtureModel(fit$parameters, 1000)
length(which(gammaSamples == 1))/length(gammaSamples)
length(which(gammaSamples == 2))/length(gammaSamples)


par(mfrow=c(2,1))
xmax=25+quantile(op$latency_ms, 0.99)
hist(op$latency_ms, xlim=c(0,xmax), breaks=100)
hist(gammaSamples, xlim=c(0,xmax), breaks=100)



par(mfrow=c(2,1))
plot(fit)
hist(op$latency_ms, breaks=100)