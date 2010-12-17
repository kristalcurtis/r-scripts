# Testing

# check "getPredictedOpDensityUsingMM"

# get density domain
source("convolutionForOpCombination-functions.R")
source("useOpDensitiesForQueryDensityPrediction.R")

from=min(op1$latency_ms, op2$latency_ms, op3$latency_ms, op4$latency_ms, op5$latency_ms, op6$latency_ms, op7$latency_ms, op8$latency_ms, op9$latency_ms)
to=max(op1$latency_ms, op2$latency_ms, op3$latency_ms, op4$latency_ms, op5$latency_ms, op6$latency_ms, op7$latency_ms, op8$latency_ms, op9$latency_ms)
length.out=20000
densityDomain=seq(from=from, to=to, length.out=length.out)
length(densityDomain)

d = getPredictedOpDensityUsingMM(op3$latency_ms, 5, "weibull", "evenlySpacedByQuantile", densityDomain)
par(mfrow=c(2,1))
plot(density(op3$latency_ms), xlim=c(0,50))
plot(d, type="l", xlim=c(0,50))
getQuantileFromDensity(d, 0.99)

d.7mixtures = getPredictedOpDensityUsingMM(op3$latency_ms, 7, "weibull", "evenlySpacedByQuantile", densityDomain)
getQuantileFromDensity(d.7mixtures, 0.99)

quantile(op3$latency_ms, 0.99)


# build up list of op densities
d1 = getPredictedOpDensityUsingMM(op1$latency_ms, 3, "weibull", "evenlySpacedByQuantile", densityDomain)
d1$y[which(d1$y == Inf)] = 0
q99.d1 = getQuantileFromDensity(d1, 0.99)
quantile(op1$latency_ms, 0.99)
# nice!

d2 = getPredictedOpDensityUsingMM(op2$latency_ms, 3, "weibull", "evenlySpacedByQuantile", densityDomain)
q99.d2 = getQuantileFromDensity(d2, 0.99)
quantile(op2$latency_ms, 0.99)
# off quite a bit.  try to find a better fit.

d3 = d

d4 = getPredictedOpDensityUsingMM(op4$latency_ms, 3, "weibull", "evenlySpacedByQuantile", densityDomain)
d4$y[which(d4$y == Inf)] = 0
q99.d4 = getQuantileFromDensity(d4, 0.99)
quantile(op4$latency_ms, 0.99)
# off by quite a bit -- about 6 ms

d5 = getPredictedOpDensityUsingMM(op5$latency_ms, 4, "weibull", "evenlySpacedByQuantile", densityDomain)
q99.d5 = getQuantileFromDensity(d5, 0.99)
quantile(op5$latency_ms, 0.99)
# nice!

# check "getPredictedOpDensityUsingSingleDistr"
d6 = getPredictedOpDensityUsingSingleDistr(op6$latency_ms, "gamma", densityDomain)
q99.d6 = getQuantileFromDensity(d6, 0.99)
quantile(op6$latency_ms, 0.99)
# nice!

d7 = getPredictedOpDensityUsingSingleDistr(op7$latency_ms, "gamma", densityDomain)
q99.d7 = getQuantileFromDensity(d7, 0.99)
quantile(op7$latency_ms, 0.99)
# underestimates 99th, but who cares? under 1 ms

d8 = getPredictedOpDensityUsingSingleDistr(op8$latency_ms, "gamma", densityDomain)
q99.d8 = getQuantileFromDensity(d8, 0.99)
quantile(op8$latency_ms, 0.99)
# not bad

d9 = getPredictedOpDensityUsingSingleDistr(op9$latency_ms, "gamma", densityDomain) # doesn't work
d9 = getPredictedOpDensityUsingSingleDistr(op9$latency_ms, "exponential", densityDomain)
q99.d9 = getQuantileFromDensity(d9, 0.99)
quantile(op9$latency_ms, 0.99)


opDensityList = list(d1=d1, d2=d2, d3=d3, d4=d4, d5=d5, d6=d6, d7=d7, d8=d8, d9=d9)
save(opDensityList, file="~/Desktop/opDensityList.RData")


# make op density list using empirical op densities
d1.empirical = density(op1$latency_ms, from=from, to=to, n=20000)
d2.empirical = density(op2$latency_ms, from=from, to=to, n=20000)
d3.empirical = density(op3$latency_ms, from=from, to=to, n=20000)
d4.empirical = density(op4$latency_ms, from=from, to=to, n=20000)
d5.empirical = density(op5$latency_ms, from=from, to=to, n=20000)
d6.empirical = density(op6$latency_ms, from=from, to=to, n=20000)
d7.empirical = density(op7$latency_ms, from=from, to=to, n=20000)
d8.empirical = density(op8$latency_ms, from=from, to=to, n=20000)
d9.empirical = density(op9$latency_ms, from=from, to=to, n=20000)

opDensityList.empirical = list(d1=d1.empirical, d2=d2.empirical, d3=d3.empirical, d4=d4.empirical, d5=d5.empirical, d6=d6.empirical, d7=d7.empirical, d8=d8.empirical, d9=d9.empirical)

source("convolutionForOpCombination-functions.R")
load("~/Desktop/queryObs.RData")
d.nA=getQueryDensityViaConvolution(opDensityList.empirical, "needsApproval")
q99.nA.empirical = getQuantileFromDensity(d.nA, 0.99)
quantile(nAqueries$latency_ms, 0.99)

d.uBH=getQueryDensityViaConvolution(opDensityList.empirical, "userByHometown")
q99.uBH.empirical=getQuantileFromDensity(d.uBH, 0.99)
quantile(uBHqueries$latency_ms, 0.99)

d.mF=getQueryDensityViaConvolution(opDensityList.empirical, "myFollowing")
q99.mF.empirical=getQuantileFromDensity(d.mF, 0.99)
quantile(mFqueries$latency_ms, 0.99)

d.mT=getQueryDensityViaConvolution(opDensityList.empirical, "myThoughts")
q99.mT.empirical=getQuantileFromDensity(d.mT, 0.99)
quantile(mTqueries$latency_ms, 0.99)


# see how well the MM fits do (compared to the empirical)
opDensityList.mm = opDensityList
d.nA.MM = getQueryDensityViaConvolution(opDensityList.mm, "needsApproval")
q99.nA.MM = getQuantileFromDensity(d.nA.MM, 0.99)

d.uBH.MM = getQueryDensityViaConvolution(opDensityList.mm, "userByHometown")
q99.uBH.MM = getQuantileFromDensity(d.uBH.MM, 0.99)

d.mF.MM = getQueryDensityViaConvolution(opDensityList.mm, "myFollowing")
q99.mF.MM = getQuantileFromDensity(d.mF.MM, 0.99)

d.mT.MM = getQueryDensityViaConvolution(opDensityList.mm, "myThoughts")
q99.mT.MM = getQuantileFromDensity(d.mT.MM, 0.99)


