# End-to-end modeling of op => latency distr for query (for other ops, empirical distr is used).

rm(list=ls())
load("~/Desktop/ops.RData")

checkSingleOpModelImpactOnQueryModel(opSamples=modelSingleOpMassTailSeparately(opObservations=op3$latency_ms, splitLatency=10, leftDistr="gammaMM", rightDistr="gammaRegular"), opNum=3, prefix="~/Desktop/op3", queryType="userByHometown")


tryDifferentSplitPointsSingleOp(opObservations=op3$latency_ms, splitLatencyVector=seq(from=5, to=40, by=5), leftDistr="gammaMM", rightDistr="gammaRegular", opNum=3, prefix="~/Desktop/op3-diff-splits", queryType="userByHometown")


# for now, op3 is hard-coded
# in the future, use the opNum arg
tryDifferentSplitPointsSingleOp = function(opObservations, splitLatencyVector, leftDistr, rightDistr, opNum, prefix, queryType) {
	dir.create(prefix)
	for (i in 1:length(splitLatencyVector)) {
		checkSingleOpModelImpactOnQueryModel(opSamples=modelSingleOpMassTailSeparately(opObservations=op3$latency_ms, splitLatency=splitLatencyVector[i], leftDistr=leftDistr, rightDistr=rightDistr), opNum=3, prefix=paste(prefix, "/split=", splitLatencyVector[i], sep=""), queryType=queryType)
	}
}

# Helper functions

# Doesn't do any truncating if samples go beyond split
modelSingleOpMassTailSeparately = function(opObservations, splitLatency, leftDistr, rightDistr, numSamples=1000) {
	opLeftOfSplit = opObservations[which(opObservations <= splitLatency)]
	opRightOfSplit = opObservations[which(opObservations > splitLatency)]
	
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/getDistrSamplers.R")
	
	if (leftDistr == "gammaRegular") {
		samplesLeftOfSplit = getGammaSamples(opLeftOfSplit, numSamples)
	} else if (leftDistr == "gammaMM") {
		samplesLeftOfSplit = getGammaSamplesMomentMatching(opLeftOfSplit, numSamples)
	} else if (leftDistr == "normal") {
		samplesLeftOfSplit = getNormalSamples(opLeftOfSplit, numSamples)
	} else if (leftDistr == "logNormal") {
		samplesLeftOfSplit = getLogNormalSamples(opLeftOfSplit, numSamples)
	} else {
		print("Invalid distribution type specified for left of split.")
	}


	if (rightDistr == "gammaRegular") {
		samplesRightOfSplit = getGammaSamples(opRightOfSplit, numSamples)
	} else if (rightDistr == "gammaMM") {
		samplesRightOfSplit = getGammaSamplesMomentMatching(opRightOfSplit, numSamples)
	} else if (rightDistr == "normal") {
		samplesRightOfSplit = getNormalSamples(opRightOfSplit, numSamples)
	} else if (rightDistr == "logNormal") {
		samplesRightOfSplit = getLogNormalSamples(opRightOfSplit, numSamples)
	} else {
		print("Invalid distribution type specified for right of split.")
	}

	
	return(c(samplesLeftOfSplit, samplesRightOfSplit))

}


checkSingleOpModelImpactOnQueryModel = function(opSamples, opNum, prefix, queryType) {
	dir.create(prefix, recursive=TRUE)

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
	#print(ls())
	
	save(list=ls(), file=paste(prefix, "/opDistr.RData", sep=""))
	
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/modeling-harness-analytical-distr.R")
	opDistrFile=paste(prefix, "/opDistr.RData", sep="")
	destinationPath=paste(prefix)
	
	computeAndComparePredictedQueryDistr(opDistrFile=opDistrFile, destinationPath=destinationPath, queryType=queryType)
}


