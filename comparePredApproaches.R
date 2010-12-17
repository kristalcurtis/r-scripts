# Setup op list
load("~/Desktop/ops.RData")
ops = list(op1=op1$latency_ms, op2=op2$latency_ms, op3=op3$latency_ms, op4=op4$latency_ms, op5=op5$latency_ms, op6=op6$latency_ms, op7=op7$latency_ms, op8=op8$latency_ms, op9=op9$latency_ms)
save(ops, file="~/Desktop/opsList.RData")

# Save queriesList
load("~/Desktop/queryObs.RData")
queriesList = list(nA=nAqueries$latency_ms, uBH=uBHqueries$latency_ms, mF=mFqueries$latency_ms, mT=mTqueries$latency_ms)
save(queriesList, file="~/Desktop/queriesList.RData")
names(queriesList)
par(mfrow=c(2,1))
hist(uBHqueries$latency_ms, breaks=1000)
hist(queriesList$uBH, breaks=1000)

# checking truncateOpHistToLeftOfQuantile
h3T = truncateOpHistToLeftOfQuantile(op3$latency_ms, 0.9)
h3F = hist(op3$latency_ms, breaks=1000)

par(mfrow=c(2,1))
plot(h3T)
plot(h3F)

# checking querySampler

opHists = list(op1Hist=h1Full, op2Hist=h2Full, op3Hist=h3Full, op4Hist=h4Full, op5Hist=h5Full, op6Hist=h6Full, op7Hist=h7Full, op8Hist=h8Full, op9Hist=h9Full)

numSamples=10000
samples = querySampler(opHists, "userByHometown", numSamples)
samplesOrig = userByHometownSampler(h2Full, h3Full, h6Full, numSamples)

par(mfrow=c(2,1))
hist(samples, breaks=100)
abline(v=quantile(samples, 0.99), lw=2, col="blue")
hist(samplesOrig, breaks=100)
abline(v=quantile(samplesOrig, 0.99), lw=2, col="blue")

quantile(samples, 0.99)
quantile(samplesOrig, 0.99)

# checking predictWithTrucatedOpHist
predictWithTruncatedOpHist("userByHometown", ops=ops, numSamples=1000)


# checking predictWithFullOpHist
full99th = predictWithFullOpHist("userByHometown", 0.99, ops, 10000)
full = predictWithFullOpHist("userByHometown", c(0.5,0.99), ops, 10000)

quantile(samplesOrig, 0.99)
full99th

# checking predictWithGMMOpFits
gmm99th = predictWithGMMOpFits("userByHometown", 0.99, ops, 10000)

# checking resampleOps
ops2 = resampleOps(ops)
names(ops2)

par(mfrow=c(2,1))
hist(ops$op3, breaks=1000)
hist(ops2$op3, breaks=1000)

# checking bootstrapActualQueryLatencyDistr
quantileEstimatesActual=bootstrapActualQueryLatencyDistr(10, "userByHometown", "~/Desktop/queriesList.RData", desiredOutput="values")
# compared results against barplots i made earlier => looks good

# checking bootstrapQueryLatencyDistr
source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/bootstrapFunctions.R")

quantileEstimatesWithNoise = bootstrapQueryLatencyDistr("empiricalFull", 10, "userByHometown", "~/Desktop/opsList.RData", numSamples=1000)
bootstrapQueryLatencyDistr("empiricalFull", 10, "userByHometown", "~/Desktop/opsList.RData", numSamples=10000)
bootstrapQueryLatencyDistr("empiricalFull", 100, "userByHometown", "~/Desktop/opsList.RData", numSamples=1000)
#bootstrapQueryLatencyDistr("empiricalFull", 100, "userByHometown", "~/Desktop/opsList.RData", numSamples=10000) # crashes

quantileEstimatesF = bootstrapQueryLatencyDistr("empiricalFull", 10, "userByHometown", "~/Desktop/opsList.RData", numSamples=1000, desiredOutput="values")
quantileEstimatesT = bootstrapQueryLatencyDistr("empiricalTruncated", 10, "userByHometown", "~/Desktop/opsList.RData", numSamples=1000, desiredOutput="values", latencyQuantilesToEstimate=0.99)
quantileEstimatesGMM = bootstrapQueryLatencyDistr("gmm", 10, "userByHometown", "~/Desktop/opsList.RData", numSamples=1000, desiredOutput="values")


# figuring out boxplot
?boxplot
queryType="userByHometown"
par(mar=c(5,5,4,2)+0.1)
b=boxplot(quantileEstimatesActual[,"0.99"],quantileEstimates[,"0.99"], quantileEstimatesT, quantileEstimatesGMM[,"0.99"],ylim=c(0,80), names=c("actual", "full empirical", "truncated empirical", "gmm"), boxwex=0.5, col="turquoise", xlab="Estimation Method", ylab="Latency (ms)", main=paste(queryType, ": bootstrap estimates of 99th %ile latency", sep=""))

?text
text(0.6,b$stats[3,1], round(b$stats[3,1], digits=1))
text(1.6,b$stats[3,2], round(b$stats[3,2], digits=1))
text(2.6,b$stats[3,3], round(b$stats[3,3], digits=1))
text(3.6,b$stats[3,4], round(b$stats[3,4], digits=1))

legend("bottomright", legend="median", lw=3)

# setup predictions matrix
predictionsMatrix = matrix(data=c(quantileEstimatesActual[,"0.99"],quantileEstimates[,"0.99"], quantileEstimatesT, quantileEstimatesGMM[,"0.99"]), nrow=nrow(quantileEstimatesActual), ncol=4)
colnames(predictionsMatrix) = c("actual", "empFull", "empTruncated", "gmm")

source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/graphical-compare-actual-predicted-query-distr.R")
plotActualAndPredictedBootstrapBoxAndWhisker(predictionsMatrix, 0.99, "userByHometown", "~/Desktop/bootstrap")


# checking getPredictionsMatrix
nAmtx = getPredictionsMatrix("needsApproval", 10, 1000, 0.99)
plotActualAndPredictedBootstrapBoxAndWhisker(nAmtx, 0.99, "needsApproval", "~/Desktop/bootstrap")

mtx = getPredictionsMatrix("userByHometown", 10, 1000, 0.99)
uBHmtx = mtx
plotActualAndPredictedBootstrapBoxAndWhisker(uBHmtx, 0.99, "userByHometown", "~/Desktop/bootstrap")

mFmtx = getPredictionsMatrix("myFollowing", 10, 1000, 0.99)
plotActualAndPredictedBootstrapBoxAndWhisker(mFmtx, 0.99, "myFollowing", "~/Desktop/bootstrap")

mTmtx = getPredictionsMatrix("myThoughts", 10, 1000, 0.99)
plotActualAndPredictedBootstrapBoxAndWhisker(mTmtx, 0.99, "myThoughts", "~/Desktop/bootstrap")

save(nAmtx, uBHmtx, mFmtx, mTmtx, file="~/Desktop/predMtces-10resamples-1000samples-99th.RData")

# checking 95th %ile
nAmtx2 = getPredictionsMatrix("needsApproval", 10, 1000, 0.95)
plotActualAndPredictedBootstrapBoxAndWhisker(nAmtx2, 0.95, "needsApproval", "~/Desktop/bootstrap-0.95")

uBHmtx2 = getPredictionsMatrix("userByHometown", 10, 1000, 0.95)
plotActualAndPredictedBootstrapBoxAndWhisker(uBHmtx2, 0.95, "userByHometown", "~/Desktop/bootstrap-0.95")

mFmtx2 = getPredictionsMatrix("myFollowing", 10, 1000, 0.95)
plotActualAndPredictedBootstrapBoxAndWhisker(mFmtx2, 0.95, "myFollowing", "~/Desktop/bootstrap-0.95")

mTmtx2 = getPredictionsMatrix("myThoughts", 10, 1000, 0.95)
plotActualAndPredictedBootstrapBoxAndWhisker(mTmtx2, 0.95, "myThoughts", "~/Desktop/bootstrap-0.95")
