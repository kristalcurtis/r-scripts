# Replace all values less than 90th %ile latency by that latency
# Retain tail

rm(list=ls())
load("~/Desktop/ops.RData")
ls()

h1Full = hist(op1$latency_ms, breaks=1000)
h2Full = hist(op2$latency_ms, breaks=1000)
h3Full = hist(op3$latency_ms, breaks=1000)
h4Full = hist(op4$latency_ms, breaks=1000)
h5Full = hist(op5$latency_ms, breaks=1000)
h6Full = hist(op6$latency_ms, breaks=1000)
h7Full = hist(op7$latency_ms, breaks=1000)
h8Full = hist(op8$latency_ms, breaks=1000)
h9Full = hist(op9$latency_ms, breaks=1000)

lumpToQuantile = function(op, quantile=0.9) {
	h=hist(op$latency_ms, breaks=1000, plot=FALSE)

	q=quantile(op$latency_ms, quantile)

	# Find closest break to 90th percentile
	breakDists = h$breaks - q
	bin=which.min(abs(breakDists))

	## Lump all counts for breaks to the left into that bin
	# Zero out all the bins to the left
	#h$counts[bin] = sum(h$counts[1:bin])
	#h$counts[1:bin-1] = 0
	h$density[1:bin-1] = 0
	
	return(h)
}

h1Trun=lumpToQuantile(op1)
h2Trun=lumpToQuantile(op2)
h3Trun=lumpToQuantile(op3)
h4Trun=lumpToQuantile(op4)
h5Trun=lumpToQuantile(op5)
h6Trun=lumpToQuantile(op6)
h7Trun=lumpToQuantile(op7)
h8Trun=lumpToQuantile(op8)
h9Trun=lumpToQuantile(op9)

plot(h2)
plot(h3)
plot(h6)

# Try for uBH
source("~/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/querySamplers.R")

nAsamplesTrun=needsApprovalSampler(h1Trun, h3Trun, h4Trun, h6Trun, h9Trun, 1000)
nAsamplesFull=needsApprovalSampler(h1Full, h3Full, h4Full, h6Full, h9Full, 1000)

uBHsamplesTrun=userByHometownSampler(h2Trun, h3Trun, h6Trun, 1000)
uBHsamplesFull=userByHometownSampler(h2Full, h3Full, h6Full, 1000)

mFsamplesTrun=myFollowingSampler(h1Trun, h4Trun, h5Trun, h6Trun, 1000)
mFsamplesFull=myFollowingSampler(h1Full, h4Full, h5Full, h6Full, 1000)

mTsamplesTrun=myThoughtsSampler(h1Trun, h4Trun, h6Trun, h9Trun, 1000)
mTsamplesFull=myThoughtsSampler(h1Full, h4Full, h6Full, h9Full, 1000)


# Compare full, truncated
#samplesTrun=nAsamplesTrun
#samplesFull=nAsamplesFull

#samplesTrun=uBHsamplesTrun
#samplesFull=uBHsamplesFull

#samplesTrun=mFsamplesTrun
#samplesFull=mFsamplesFull

#samplesTrun=mTsamplesTrun
#samplesFull=mTsamplesFull


par(mfrow=c(2,1))
xmax=max(samplesFull, samples)
hist(samplesFull, breaks=100, main="full", xlim=c(0,xmax))
abline(v=quantile(samplesFull,0.99), lw=2, col="green")

hist(samplesTrun, breaks=100, main="truncated", xlim=c(0,xmax))
abline(v=mean(samplesTrun), lw=2, col="purple")
abline(v=quantile(samplesTrun,0.5), lw=2, col="red")
abline(v=quantile(samplesTrun,0.6), lw=2, col="red")
abline(v=quantile(samplesTrun,0.7), lw=2, col="red")
abline(v=quantile(samplesTrun,0.8), lw=2, col="red")
abline(v=quantile(samplesTrun,0.9), lw=2, col="red")
abline(v=quantile(samplesTrun,0.99), lw=2, col="red")

summary(as.vector(samplesTrun))
summary(as.vector(samplesFull))


par(mfrow=c(2,1))
hist(op3$latency_ms, breaks=1000)
plot(h3)


# how noisy is it?
means = vector(length=10)
for(i in 1:10) {
	uBHsamplesTrun=userByHometownSampler(h2Trun, h3Trun, h6Trun, 1000)
	means[i]=mean(uBHsamplesTrun)
}


