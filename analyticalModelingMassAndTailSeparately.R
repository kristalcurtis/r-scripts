## 6.16.10
## Produce analytical models for mass, tail of op distr by splitting and modeling the mass & tail separately
## Explore where to split
## Explore what distr to use for mass, tail
## Does truncating make sense (eg, if negative values are obtained or if values on the wrong side of the split point are obtained)?

# Start by taking the 90th percentile as the split point
# Do this for all ops, then plug into query latency estimation
# Try fitting a gamma by moment matching for left of split, regular gamma for right of split
rm(list=ls())

library(moments)
source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/getDistrSamplers.R")

load("~/Desktop/ops.RData")
ls()
op = op3

#splitLatency = quantile(op$latency_ms, 0.9)
splitLatency = 10
opLeftOfSplit = op[which(op$latency_ms <= splitLatency),]
opRightOfSplit = op[which(op$latency_ms > splitLatency),]
numSamples = 1000

# Fit a gamma by moment matching to left of split
#skew = skewness(opLeftOfSplit$latency_ms)
#k.mm = 4/(skew^2)
#variance = var(opLeftOfSplit$latency_ms)
#theta.mm = sqrt(variance/k.mm)
#mu = mean(opLeftOfSplit$latency_ms)
#shift = mu - k.mm*theta.mm 
#samplesLeftOfSplit = rgamma(10000, shape=k.mm, scale=theta.mm) + shift
samplesLeftOfSplit.mmGamma = getGammaSamplesMomentMatching(opLeftOfSplit$latency_ms, numSamples)


# Fit a regular gamma to left of split
#s = log(1/(nrow(opLeftOfSplit)) * sum(opLeftOfSplit$latency_ms)) - 1/nrow(opLeftOfSplit) * sum(log(opLeftOfSplit$latency_ms))
#k.left = (3 - s + sqrt((s-3)^2 + 24*s))/(12*s)
#theta.left = 1/(k.left*nrow(opLeftOfSplit)) * sum(opLeftOfSplit$latency_ms)
#samplesLeftOfSplit.regularGamma = rgamma(10000, shape=k.left, scale=theta.left)
samplesLeftOfSplit.regularGamma = getGammaSamples(opLeftOfSplit$latency_ms, numSamples)


# Fit a log normal to left of split
samplesLeftOfSplit.logNormal = getLogNormalSamples(opLeftOfSplit$latency_ms, numSamples)


# Visually inspect fit of left side
par(mfrow=c(5,1))
#xmax=max(quantile(op$latency_ms, 0.99), samplesLeftOfSplit)
xmax=12
hist(op$latency_ms, xlim=c(0,xmax), breaks=1000)
hist(opLeftOfSplit$latency_ms, xlim=c(0,xmax), 30)
hist(samplesLeftOfSplit.mmGamma, xlim=c(0,xmax), 30)
hist(samplesLeftOfSplit.regularGamma, xlim=c(0,xmax), 30)  # very symmetrical; we need a skew
hist(samplesLeftOfSplit.logNormal, xlim=c(0,xmax), 30)  # very symmetrical; we need a skew


# Visually inspect using density instead of histogram
pdf("~/Desktop/op3-left-of-split.pdf")
par(mfrow=c(4,1))
xmax = 12
plot(density(opLeftOfSplit$latency_ms), xlim=c(0,xmax))
abline(v=median(opLeftOfSplit$latency_ms), lw=2, col="purple")

plot(density(samplesLeftOfSplit.mmGamma), xlim=c(0,xmax))
abline(v=median(samplesLeftOfSplit.mmGamma), lw=2, col="purple")

plot(density(samplesLeftOfSplit.regularGamma), xlim=c(0,xmax))
abline(v=median(samplesLeftOfSplit.regularGamma), lw=2, col="purple")

plot(density(samplesLeftOfSplit.logNormal), xlim=c(0,xmax))
abline(v=median(samplesLeftOfSplit.logNormal), lw=2, col="purple")
dev.off()



# Fit a regular gamma to right of split
#s = log(1/(nrow(opRightOfSplit)) * sum(opRightOfSplit$latency_ms)) - 1/nrow(opRightOfSplit) * sum(log(opRightOfSplit$latency_ms))
#k = (3 - s + sqrt((s-3)^2 + 24*s))/(12*s)
#theta = 1/(k*nrow(opRightOfSplit)) * sum(opRightOfSplit$latency_ms)
#samplesRightOfSplit = rgamma(10000, shape=k, scale=theta)
samplesRightOfSplit.regularGamma = getGammaSamples(opRightOfSplit$latency_ms, numSamples)


# Visually inspect fit from regular gamma to tail
par(mfrow=c(2,1))
xmax=max(quantile(op$latency_ms, 0.99), samplesRightOfSplit.regularGamma)
#hist(op$latency_ms, xlim=c(0,xmax), breaks=1000)
hist(opRightOfSplit$latency_ms, xlim=c(0,xmax), 500)
plotKeyQuantilesAndLegend(opRightOfSplit$latency_ms, c(0.5, 0.9, 0.99), c("red", "green", "blue"))

hist(samplesRightOfSplit.regularGamma, xlim=c(0,xmax), 150)
plotKeyQuantilesAndLegend(samplesRightOfSplit.regularGamma, c(0.5, 0.9, 0.99), c("red", "green", "blue"))


# Plotting mass & tail modeling attempts
pdf(file="~/Desktop/op3-mass-and-tail.pdf")
par(mfrow=c(3,2))

#xmaxMass=max(quantile(op$latency_ms, 0.99), samplesLeftOfSplit)
xmaxMass=25
xmaxTail=max(quantile(op$latency_ms, 0.99), samplesRightOfSplit)

hist(op$latency_ms, xlim=c(0,xmaxMass), breaks=1000)
plotMed90th99thQuantilesAndLegend(op$latency_ms)
hist(op$latency_ms, xlim=c(0,xmaxTail), breaks=1000)
plotMed90th99thQuantilesAndLegend(op$latency_ms)

hist(opLeftOfSplit$latency_ms, xlim=c(0,xmaxMass), 30)
plotMed90th99thQuantilesAndLegend(opLeftOfSplit$latency_ms)
hist(opRightOfSplit$latency_ms, xlim=c(0, xmaxTail), 500)
plotMed90th99thQuantilesAndLegend(opRightOfSplit$latency_ms)

hist(samplesLeftOfSplit.mmGamma, xlim=c(0,xmaxMass), 30)
plotMed90th99thQuantilesAndLegend(samplesLeftOfSplit.mmGamma)
#hist(samplesLeftOfSplit.regularGamma, xlim=c(0,xmaxMass), 30)  # looks much worse than the above, which was obtained by moment matching
hist(samplesRightOfSplit.regularGamma, xlim=c(0, xmaxTail), 150)
plotMed90th99thQuantilesAndLegend(samplesRightOfSplit.regularGamma)
dev.off()


# Plotting op latency, all samples
pdf("~/Desktop/op3-actual-vs-all-samples.pdf")
par(mfrow=c(2,1))
xmax=max(quantile(op$latency_ms, 0.99), samplesRightOfSplit)
hist(op$latency_ms, xlim=c(0,xmax), breaks=1000)
plotMed90th99thQuantilesAndLegend(op$latency_ms)

# tail is too heavy in this one
samples = c(samplesLeftOfSplit.mmGamma, samplesRightOfSplit.regularGamma)
hist(c(samples), xlim=c(0,xmax), breaks=200)
plotMed90th99thQuantilesAndLegend(samples)
dev.off()


# Save to use for predicting uBH's latency distr
op3distr.gamma = hist(c(samplesLeftOfSplit.mmGamma, samplesRightOfSplit.regularGamma), breaks=1000)
save(op3distr.gamma, file="~/Desktop/op3-massMMGamma-tailRegularGamma.RData")




# Try for uBH
rm(list=ls())
load("~/Desktop/opDistrEmpirical.RData")
#load("~/Desktop/op3Gamma.RData")
load("~/Desktop/op3-massMMGamma-tailRegularGamma.RData")
ls()
rm(op3distr)
op3distr = op3distr.gamma
save(list=ls(), file="~/Desktop/opDistr-allOps-op3Gamma.RData")

source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/modeling-harness-analytical-distr.R")
opDistrFile = "~/Desktop/uBH-with-op3Gamma-massMM-tailReg/opDistr-allOps-op3Gamma.RData"
destinationPath = "~/Desktop/uBH-with-op3Gamma-massMM-tailReg"
queryType="userByHometown"

computeAndComparePredictedQueryDistr(opDistrFile=opDistrFile, destinationPath=destinationPath, queryType=queryType)


checkSingleOpModelImpactOnQueryModel = function(opSamples, opNum, prefix, queryType) {
	dir.create(prefix)

	load("~/Desktop/opDistrEmpirical.RData")
	do.call("rm", args=list(paste("op", opNum, "distr", sep="")))
	
	if (opNum == 1) {
		op1distr = hist(opSamples, breaks=1000)
	} else if (opNum == 2) {
		op2distr = hist(opSamples, breaks=1000)
	} else if (opNum == 3) {
		op3distr = hist(opSamples, breaks=1000)
	} else if (opNum == 4) {
		op4distr = hist(opSamples, breaks=1000)
	} else if (opNum == 5) {
		op5distr = hist(opSamples, breaks=1000)
	} else if (opNum == 6) {
		op6distr = hist(opSamples, breaks=1000)
	} else if (opNum == 7) {
		op7distr = hist(opSamples, breaks=1000)
	} else if (opNum == 8) {
		op8distr = hist(opSamples, breaks=1000)
	} else if (opNum == 9) {
		op9distr = hist(opSamples, breaks=1000)
	}
	print(ls())
	
	save(list=ls(), file=paste(prefix, "/opDistr.RData", sep=""))
	
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/modeling-harness-analytical-distr.R")
	opDistrFile=paste(prefix, "/opDistr.RData", sep="")
	destinationPath=paste(prefix)
	
	computeAndComparePredictedQueryDistr(opDistrFile=opDistrFile, destinationPath=destinationPath, queryType=queryType)
}



checkSingleOpModelImpactOnQueryModel(c(samplesLeftOfSplit.mmGamma, samplesRightOfSplit.regularGamma), 3, "~/Desktop/op3", "userByHometown")