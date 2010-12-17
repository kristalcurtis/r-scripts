# 3.9.10
# Goal: predict userByEmail's 99th percentile latency using op models from thoughtstream & thoughtsByHashTag

# Note:  userByEmail has ops 2, 3, & 6

queryType="userByEmail"
numSampleSets=10
latencyQuantile=0.99

source("/work/ksauer/scads/experiments/client/performance/logparsing/src/main/R/experiment-functions.R")

# Setting up paths
basePathThoughtstream = "/work/ksauer/2.12.10-thoughtstream-experiment"
basePathThoughtsByHashTag = "/work/ksauer/3.8.10-thoughtsByHashTag-experiment"
basePathUserByEmail = "/work/ksauer/2.23.10-userByEmail-experiment"
basePath = "/work/ksauer/3.9.10-userByEmail-experiment-2"
outputPath1 = paste(basePath, "/option1", sep="")
outputPath2 = paste(basePath, "/option2", sep="")

# Training phase:  get histograms
createAndSaveUserByEmailOpHistogramsFromOtherQueries(basePathThoughtstream, basePathThoughtsByHashTag, outputPath1, outputPath2)

# Validation phase:
# Uses validation data from userByEmail experiment

## Version 1:  
# h2=h2.thoughtsByHashTag, h3=h3.thoughtstream, h6=h6.thoughtstream
print("Getting predicted latency...")
getPredictedQueryLatencyQuantiles2(queryType, numSampleSets, basePathUserByEmail, outputPath1, latencyQuantile)
getPredictionError2(basePathUserByEmail, outputPath1)


latencyQuantiles=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
getPredictedQueryLatencyMultipleQuantiles(queryType, numSampleSets, basePathUserByEmail, outputPath1, latencyQuantiles)


startingThread=51
endingThread=100
numValidationRuns=10
getMultipleValidationLatencyQuantiles(startingThread, endingThread, basePathUserByEmail, numValidationRuns, latencyQuantiles)


plotPredictionErrorMultipleQuantiles(basePathUserByEmail, outputPath1, "userByEmail", latencyQuantiles)


load(file="~/Desktop/actualAndPredictedQuantiles.RData")
latencyQuantiles=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
plot(latencyQuantiles, actualMedians, ylim=c(0,max(actualMedians, predictedMedians)), col=0)
lines(latencyQuantiles, actualMedians, lw=2, col="red")
lines(latencyQuantiles, predictedMedians, lw=2, col="blue")
legend("topleft", legend=c("actual", "predicted"), col=c("red", "blue"), lwd=2)

plot(latencyQuantiles, actualMedians)


## Version 2:  
# h2=h2.thoughtsByHashTag, h3=h3.thoughtstream, h6=h6.thoughtsByHashTag
print("Getting predicted latency...")
getPredictedQueryLatencyQuantiles2(queryType, numSampleSets, basePathUserByEmail, outputPath2, latencyQuantile)
getPredictionError2(basePathUserByEmail, outputPath2)


## 3.30.10
## Follow-up:  Look into why it didn't work.

# Version 1:
basePath = "/work/ksauer/3.9.10-userByEmail-experiment-2/option1/3.30.10-debugging"
sampledDataPath = "/work/ksauer/3.9.10-userByEmail-experiment-2/option1"
validationDataPath = "/work/ksauer/2.23.10-userByEmail-experiment"

# Examine actual data
startingThread = 51
endingThread = 100
logPath = "/work/ksauer/2.23.10-userByEmail-experiment/validation-logs/validation1-logs"

source("/work/ksauer/scads/experiments/client/performance/logparsing/src/main/R/experiment-functions.R")
data = getSingleDataset(startingThread, endingThread, logPath) 
save(data, file=paste(basePath, "/validationDataset1.RData", sep=""))
dim(data)
length(which(data$opLevel==3))
colnames(data)

pdf(file=paste(basePath, "/validation1-hist.pdf", sep=""), width=5, height=5)
hist(data$latency_ms[data$opLevel==3], breaks=25, xlab="Latency (ms)", main="userByEmail: Validation Run 1")
abline(v=median(data$latency_ms[data$opLevel==3]), col="red", lw=2)
legend("topright", legend=c(paste("median = ", round(median(data$latency_ms[data$opLevel==3]), digits=2), "ms", sep="")), lwd=2, col="red")
dev.off()


# Plot sample set 1
load(file=paste(sampledDataPath, "/sample1.RData", sep=""))  # => samples
dim(samples)

pdf(file=paste(basePath, "/sample1-hist.pdf", sep=""), width=5, height=5)
hist(samples, breaks=25, xlab="Latency (ms)", main="userByEmail: Sample Set 1")
abline(v=median(samples), col="red", lw=2)
legend("topright", legend=c(paste("median = ", round(median(samples), digits=2), "ms", sep="")), lwd=2, col="red")
dev.off()


# Bin actual queries by variance
medianValidationLatency = median(data$latency_ms[data$opLevel==3])
targetBinVariance = 0.01*medianValidationLatency

validationMedianBin = data[which(data$opLevel==3 & abs(medianValidationLatency - data$latency_ms) < targetBinVariance),]
dim(validationMedianBin)
range(validationMedianBin$latency_ms)

validationMedianBin[1:10,]

# => (threadNum, queryNum) id of each query whose latency falls within this range.

data[data$threadNum==51 & data$queryNum==12 & data$opLevel==2,]

# Next, get its ops and add those rows to another array.
currentThreadNum = validationMedianBin[1,"threadNum"]
currentQueryNum = validationMedianBin[1,"queryNum"]

print(1)
validationMedianBinOps = data[data$threadNum==currentThreadNum & data$queryNum==currentQueryNum & data$opLevel==2,]

for (i in 2:nrow(validationMedianBin)) {
	print(i)	

	currentThreadNum = validationMedianBin[i,"threadNum"]
	currentQueryNum = validationMedianBin[i,"queryNum"]

	binOps = data[data$threadNum==currentThreadNum & data$queryNum==currentQueryNum & data$opLevel==2,]
	
	validationMedianBinOps = rbind(validationMedianBinOps, binOps)
}

dim(validationMedianBinOps)
validationMedianBinOps[1:10,]

# Next, look at each op's histogram

# Op 2
pdf(file=paste(basePath, "/opType2-hist.pdf", sep=""), width=5, height=5)
histData = validationMedianBinOps$latency_ms[validationMedianBinOps$opType==2]
hist(histData, breaks=25, xlab="Latency (ms)", main="opType2 (userByEmail): Validation Run 1")
medianVal = median(histData)
abline(v=medianVal, col="red", lw=2)
legend("topright", legend=c(paste("median = ", round(medianVal, digits=2), "ms", sep="")), lwd=2, col="red")
dev.off()

histWithMedianToPdf(validationMedianBinOps$latency_ms[validationMedianBinOps$opType==2], filename=paste(basePath, "/opType2-hist.pdf", sep=""), breaks=25, xlab="Latency (ms)", main="opType2", col="blue", legendLocation="topright")


# Op 3
pdf(file=paste(basePath, "/opType3-hist.pdf", sep=""), width=5, height=5)
histData = validationMedianBinOps$latency_ms[validationMedianBinOps$opType==3]
hist(histData, breaks=25, xlab="Latency (ms)", main="opType3 (userByEmail): Validation Run 1")
medianVal = median(histData)
abline(v=medianVal, col="red", lw=2)
legend("topright", legend=c(paste("median = ", round(medianVal, digits=2), "ms", sep="")), lwd=2, col="red")
dev.off()



# Op 6
pdf(file=paste(basePath, "/opType6-hist.pdf", sep=""), width=5, height=5)
histData = validationMedianBinOps$latency_ms[validationMedianBinOps$opType==6]
hist(histData, breaks=25, xlab="Latency (ms)", main="opType6 (userByEmail): Validation Run 1")
medianVal = median(histData)
abline(v=medianVal, col="red", lw=2)
legend("topright", legend=c(paste("median = ", round(medianVal, digits=2), "ms", sep="")), lwd=2, col="red")
dev.off()



# Repeat for 2% and 10% variance => bin (plug above into a function)

# Debugging function
opsWithin1PercentBinAroundMedian = binActualDataByDifferenceFromQuantileLatency(data, 0.5, 1)

quantileLatency=0.5
percentDifferenceFromQuantileLatency=1



# Sampling should be extended; in addition to tracking the sampled query latency, also record the sampled op latencies.

load(file=paste(validationDataPath, "/validationStats.RData", sep="")) # => validationStats
numSamplesPerSet = floor(mean(validationStats[,"numQueries"]))
print(paste("Using", numSamplesPerSet, "samples per set."))

userByEmailQueryAndOpSampler(sampledDataPath, 1, numSamplesPerSet)

load(file=paste(sampledDataPath,"/sample", 1,"-queriesAndOps.RData",sep="")) # => samples, opSamples



# Bin
sampledOpsWithin1PercentBinAroundMedian = binSamplesByDifferenceFromQuantileLatency(samples, opSamples, 0.5, 1)

# debugging function
querySamples=samples
quantileLatency=0.5
percentDifferenceFromQuantileLatency=1



histWithMedianToPdf(sampledOpsWithin1PercentBinAroundMedian[,"op2"], filename=paste(basePath, "/opType2-sampled-1percent-bin-hist.pdf", sep=""), breaks=25, xlab="Latency (ms)", main="opType2 - Sampled - 1% bin", col="green", legendLocation="topright")


histWithMedianToPdf(sampledOpsWithin1PercentBinAroundMedian[,"op3"], filename=paste(basePath, "/opType3-sampled-1percent-bin-hist.pdf", sep=""), breaks=25, xlab="Latency (ms)", main="opType3 - Sampled - 1% bin", col="green", legendLocation="topright")


histWithMedianToPdf(sampledOpsWithin1PercentBinAroundMedian[,"op6"], filename=paste(basePath, "/opType6-sampled-1percent-bin-hist.pdf", sep=""), breaks=25, xlab="Latency (ms)", main="opType6 - Sampled - 1% bin", col="green", legendLocation="topright")





# Also look at hists of ops for all queries in the sample
histWithMedianToPdf(opSamples[,"op2"], filename=paste(basePath,"/opType2-allsamples-hist.pdf", sep=""), breaks=25, xlab="Latency (ms)", main="opType2 - Sampled", col="green", legendLocation="topright")

histWithMedianToPdf(opSamples[,"op3"], filename=paste(basePath,"/opType3-allsamples-hist.pdf", sep=""), breaks=25, xlab="Latency (ms)", main="opType3 - Sampled", col="green", legendLocation="topright")

histWithMedianToPdf(opSamples[,"op6"], filename=paste(basePath,"/opType6-allsamples-hist.pdf", sep=""), breaks=25, xlab="Latency (ms)", main="opType6 - Sampled", col="green", legendLocation="topright")



## More detail, per-op

opType=6
opString = paste("op",opType,sep="")

xmax = max(c(data$latency_ms[data$opLevel==2 & data$opType==opType], opsWithin1PercentBinAroundMedian$latency_ms[opsWithin1PercentBinAroundMedian$opType==opType], opSamples[,opString], sampledOpsWithin1PercentBinAroundMedian[,opString]))


pdf(file=paste(basePath, "/", opString, "-hists.pdf", sep=""), height=10, width=10)
par(mar=c(5,5,4,2)+0.1)
par(mfrow=c(2,2))

hist(data$latency_ms[data$opLevel==2 & data$opType==opType], xlab="Latency (ms)", main=paste(opString, ", all queries in validation run", sep=""), xlim=c(0,xmax))
medianLatency = median(data$latency_ms[data$opLevel==2 & data$opType==opType])
abline(v=medianLatency, col="red", lw=2)
legend("topright", legend=paste("median = ", round(medianLatency, digits=0), " ms", sep=""), col="red", lwd=2)

hist(opsWithin1PercentBinAroundMedian$latency_ms[opsWithin1PercentBinAroundMedian$opType==opType], xlab="Latency (ms)", main=paste(opString, ", from queries within 1% bin around median", sep=""), xlim=c(0,xmax))
medianLatency = median(opsWithin1PercentBinAroundMedian$latency_ms[opsWithin1PercentBinAroundMedian$opType==opType])
abline(v=medianLatency, col="red", lw=2)
legend("topright", legend=paste("median = ", round(medianLatency, digits=0), " ms", sep=""), col="red", lwd=2)

hist(opSamples[,opString], xlab="Latency (ms)", main=paste(opString,", all samples in sample set",sep=""), xlim=c(0,xmax))
medianLatency = median(opSamples[,opString])
abline(v=medianLatency, col="blue", lw=2)
legend("topright", legend=paste("median = ", round(medianLatency, digits=0), " ms", sep=""), col="blue", lwd=2)

hist(sampledOpsWithin1PercentBinAroundMedian[,opString], xlab="Latency (ms)", main=paste(opString, ", from samples within 1% bin around median", sep=""), xlim=c(0,xmax))
medianLatency=median(sampledOpsWithin1PercentBinAroundMedian[,opString])
abline(v=medianLatency, col="blue", lw=2)
legend("topright", legend=paste("median = ", round(medianLatency, digits=0), " ms", sep=""), col="blue", lwd=2)

dev.off()


