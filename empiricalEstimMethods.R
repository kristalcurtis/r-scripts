# Diff methods for estimating 99th %ile latency

# Sampling from raw data (extreme version of histogram - each bin has only one data pt in it)

rm(list=ls())
load("~/Desktop/ops.RData")

setwd("~/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R")
source("empiricalEstimMethods-functions.R")

# checking "createExtremeHistogramForRawData"
h3 = createExtremeHistogramForRawData(op3$latency_ms)
names(h3)
length(h3$mids)
length(h3$density)
nrow(op3)

unique(h3$density)
unique(h3$density)*length(h3$density)

summary(h3$mids)
summary(op3$latency_ms)

h1.raw = createExtremeHistogramForRawData(op1$latency_ms)
h2.raw = createExtremeHistogramForRawData(op2$latency_ms)
h3.raw = createExtremeHistogramForRawData(op3$latency_ms)
h4.raw = createExtremeHistogramForRawData(op4$latency_ms)
h5.raw = createExtremeHistogramForRawData(op5$latency_ms)
h6.raw = createExtremeHistogramForRawData(op6$latency_ms)
h7.raw = createExtremeHistogramForRawData(op7$latency_ms)
h8.raw = createExtremeHistogramForRawData(op8$latency_ms)
h9.raw = createExtremeHistogramForRawData(op9$latency_ms)


# using raw data "histograms" to predict query latency
source("querySamplers.R")

nAsamples.raw = needsApprovalSampler(h1.raw, h3.raw, h4.raw, h6.raw, h9.raw, 10000)
uBHsamples.raw = userByHometownSampler(h2.raw, h3.raw, h6.raw, 10000)
mFsamples.raw = myFollowingSampler(h1.raw, h4.raw, h5.raw, h6.raw, 10000)
mTsamples.raw = myThoughtsSampler(h1.raw, h4.raw, h6.raw, h9.raw, 10000)

quantile(nAsamples.raw, 0.99)
quantile(uBHsamples.raw, 0.99)
quantile(mFsamples.raw, 0.99)
quantile(mTsamples.raw, 0.99)

load("~/Desktop/queryObs.RData")
ls()
quantile(nAqueries$latency_ms, 0.99)
quantile(uBHqueries$latency_ms, 0.99)
quantile(mFqueries$latency_ms, 0.99)
quantile(mTqueries$latency_ms, 0.99)

# checking bootstrap function
source("bootstrapFunctions.R")
ops = getOpList("~/Desktop/ops.RData")
names(ops)

nA99th.raw = predictWithExtremeOpHist("needsApproval", 0.99, ops, 10000)
uBH99th.raw = predictWithExtremeOpHist("userByHometown", 0.99, ops, 10000)
mF99th.raw = predictWithExtremeOpHist("myFollowing", 0.99, ops, 10000)
mT99th.raw = predictWithExtremeOpHist("myThoughts", 0.99, ops, 10000)


