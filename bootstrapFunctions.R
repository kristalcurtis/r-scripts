# Setup
getOpList = function(opFilename) {
	load(opFilename)
	
	ops = list(op1=op1$latency_ms, op2=op2$latency_ms, op3=op3$latency_ms, op4=op4$latency_ms, op5=op5$latency_ms, op6=op6$latency_ms, op7=op7$latency_ms, op8=op8$latency_ms, op9=op9$latency_ms)

	return(ops)
}


# Prediction procedures
predictWithTruncatedOpHist = function(queryType, quantileToTruncate=0.9, quantileOfTruncatedDistr=0.9, ops, numSamples) {
	# Truncate op hist
	op1Hist = truncateOpHistToLeftOfQuantile(ops$op1, quantile=quantileToTruncate)
	op2Hist = truncateOpHistToLeftOfQuantile(ops$op2, quantile=quantileToTruncate)
	op3Hist = truncateOpHistToLeftOfQuantile(ops$op3, quantile=quantileToTruncate)
	op4Hist = truncateOpHistToLeftOfQuantile(ops$op4, quantile=quantileToTruncate)
	op5Hist = truncateOpHistToLeftOfQuantile(ops$op5, quantile=quantileToTruncate)
	op6Hist = truncateOpHistToLeftOfQuantile(ops$op6, quantile=quantileToTruncate)
	op7Hist = truncateOpHistToLeftOfQuantile(ops$op7, quantile=quantileToTruncate)
	op8Hist = truncateOpHistToLeftOfQuantile(ops$op8, quantile=quantileToTruncate)
	op9Hist = truncateOpHistToLeftOfQuantile(ops$op9, quantile=quantileToTruncate)
	
	# Use truncated hist for query pred
	opHists=list(op1Hist=op1Hist, op2Hist=op2Hist, op3Hist=op3Hist, op4Hist=op4Hist, op5Hist=op5Hist, op6Hist=op6Hist, op7Hist=op7Hist, op8Hist=op8Hist, op9Hist=op9Hist)
	samples = querySampler(opHists, queryType, numSamples)
	
	return(quantile(samples, quantileOfTruncatedDistr))
}


truncateOpHistToLeftOfQuantile = function(op, quantile) {
	#print(length(op))
	h=hist(op, breaks=1000, plot=FALSE)

	q=quantile(op, quantile)

	# Find closest break to given quantile
	breakDists = h$breaks - q
	bin=which.min(abs(breakDists))

	# Truncate all latency values to the left of the bin
	h$density[1:bin-1] = 0	# used by samplers
	h$counts[1:bin-1] = 0	# used by R's plotting
	
	return(h)
}


predictWithFullOpHist = function(queryType, quantileToPredict, ops, numSamples) {
	# Obtain op hist
	op1Hist = hist(ops$op1, breaks=1000, plot=FALSE)
	op2Hist = hist(ops$op2, breaks=1000, plot=FALSE)
	op3Hist = hist(ops$op3, breaks=1000, plot=FALSE)
	op4Hist = hist(ops$op4, breaks=1000, plot=FALSE)
	op5Hist = hist(ops$op5, breaks=1000, plot=FALSE)
	op6Hist = hist(ops$op6, breaks=1000, plot=FALSE)
	op7Hist = hist(ops$op7, breaks=1000, plot=FALSE)
	op8Hist = hist(ops$op8, breaks=1000, plot=FALSE)
	op9Hist = hist(ops$op9, breaks=1000, plot=FALSE)
	
	opHists=list(op1Hist=op1Hist, op2Hist=op2Hist, op3Hist=op3Hist, op4Hist=op4Hist, op5Hist=op5Hist, op6Hist=op6Hist, op7Hist=op7Hist, op8Hist=op8Hist, op9Hist=op9Hist)

	samples = querySampler(opHists, queryType, numSamples)
	
	return(quantile(samples, quantileToPredict))
}


predictWithExtremeOpHist = function(queryType, quantileToPredict, ops, numSamples) {
	source("empiricalEstimMethods-functions.R")
	
	op1Hist = createExtremeHistogramForRawData(op1$latency_ms)
	op2Hist = createExtremeHistogramForRawData(op2$latency_ms)
	op3Hist = createExtremeHistogramForRawData(op3$latency_ms)
	op4Hist = createExtremeHistogramForRawData(op4$latency_ms)
	op5Hist = createExtremeHistogramForRawData(op5$latency_ms)
	op6Hist = createExtremeHistogramForRawData(op6$latency_ms)
	op7Hist = createExtremeHistogramForRawData(op7$latency_ms)
	op8Hist = createExtremeHistogramForRawData(op8$latency_ms)
	op9Hist = createExtremeHistogramForRawData(op9$latency_ms)

	opHists=list(op1Hist=op1Hist, op2Hist=op2Hist, op3Hist=op3Hist, op4Hist=op4Hist, op5Hist=op5Hist, op6Hist=op6Hist, op7Hist=op7Hist, op8Hist=op8Hist, op9Hist=op9Hist)

	samples = querySampler(opHists, queryType, numSamples)
	
	return(quantile(samples, quantileToPredict))
}



predictByConvolvingFullOpHist = function(queryType, quantileToPredict, ops, numAlignmentPoints) {
	source("convolutionForOpCombination-functions.R")

	queryDensity = getQueryDensityViaConvolution(ops, queryType, numAlignmentPoints)

	#return(getMultipleQuantilesFromDensity(queryDensity, quantileToPredict))
	return(getQuantileFromDensity(queryDensity, quantileToPredict))
}



predictWithGMMOpFits = function(queryType, quantileToPredict, ops, numSamples) {
	#source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/querySamplers.R")
	#source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")
	source("querySamplers.R")
	source("emFitAndSample.R")

	k=c(3,8,5,3,5,4,4,5,2) # from CV

	opParamFile="~/Desktop/opGMMParams.RData"
	
	if (queryType == "needsApproval") {
		op1GMMParams = fitEM(observationsVector=ops$op1, k=k[1])
		op3GMMParams = fitEM(observationsVector=ops$op3, k=k[3])
		op4GMMParams = fitEM(observationsVector=ops$op4, k=k[4])
		op6GMMParams = fitEM(observationsVector=ops$op6, k=k[6])
		op9GMMParams = fitEM(observationsVector=ops$op9, k=k[9])

		save(op1GMMParams, op3GMMParams, op4GMMParams, op6GMMParams, op9GMMParams, file=opParamFile)
		samples = needsApprovalGMMSampler(opParamFile, numSamples)
	} else if (queryType == "userByHometown") {
		op2GMMParams = fitEM(observationsVector=ops$op2, k=k[2])
		op3GMMParams = fitEM(observationsVector=ops$op3, k=k[3])
		op6GMMParams = fitEM(observationsVector=ops$op6, k=k[6])
		
		save(op2GMMParams, op3GMMParams, op6GMMParams, file=opParamFile)
		samples = userByHometownGMMSampler(opParamFile, numSamples)
	} else if (queryType == "myFollowing") {
		op1GMMParams = fitEM(observationsVector=ops$op1, k=k[1])
		op4GMMParams = fitEM(observationsVector=ops$op4, k=k[4])
		op5GMMParams = fitEM(observationsVector=ops$op5, k=k[5])
		op6GMMParams = fitEM(observationsVector=ops$op6, k=k[6])

		save(op1GMMParams, op4GMMParams, op5GMMParams, op6GMMParams, file=opParamFile)
		samples = myFollowingGMMSampler(opParamFile, numSamples)
	} else if (queryType == "myThoughts") {
		op1GMMParams = fitEM(observationsVector=ops$op1, k=k[1])
		op4GMMParams = fitEM(observationsVector=ops$op4, k=k[4])
		op6GMMParams = fitEM(observationsVector=ops$op6, k=k[6])
		op9GMMParams = fitEM(observationsVector=ops$op9, k=k[9])
		
		save(op1GMMParams, op4GMMParams, op6GMMParams, op9GMMParams, file=opParamFile)
		samples = myThoughtsGMMSampler(opParamFile, numSamples)
	} else {
		print("Unsupported query type.")
	}
	
	return(quantile(samples, quantileToPredict))
}

querySampler = function(opHists, queryType, numSamples) {
	#source("~/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/querySamplers.R")
	source("querySamplers.R")

	if (queryType == "needsApproval") {
		samples=needsApprovalSampler(opHists$op1Hist, opHists$op3Hist, opHists$op4Hist, opHists$op6Hist, opHists$op9Hist, numSamples)
	} else if (queryType == "userByHometown") {
		samples=userByHometownSampler(opHists$op2Hist, opHists$op3Hist, opHists$op6Hist, numSamples)
	} else if (queryType == "myFollowing") {
		samples=myFollowingSampler(opHists$op1Hist, opHists$op4Hist, opHists$op5Hist, opHists$op6Hist, numSamples)
	} else if (queryType == "myThoughts") {
		samples=myThoughtsSampler(opHists$op1Hist, opHists$op4Hist, opHists$op6Hist, opHists$op9Hist, numSamples)
	} else {
		print("Unsupported query type.")
	}
	
	return(samples)
}


resampleDataset = function(dataset) {
	return(sample(dataset, size=length(dataset), replace=TRUE, prob=rep.int(1/length(dataset), length(dataset))))
}


resampleOps = function(ops) {
	op1 = resampleDataset(ops$op1)
	op2 = resampleDataset(ops$op2)
	op3 = resampleDataset(ops$op3)
	op4 = resampleDataset(ops$op4)
	op5 = resampleDataset(ops$op5)
	op6 = resampleDataset(ops$op6)
	op7 = resampleDataset(ops$op7)
	op8 = resampleDataset(ops$op8)
	op9 = resampleDataset(ops$op9)
		
	resampledOps = list(op1=op1, op2=op2, op3=op3, op4=op4, op5=op5, op6=op6, op7=op7, op8=op8, op9=op9)

	return(resampledOps)
}

resampleQueries = function(queriesList) {
	nA = resampleDataset(queriesList$nA)
	uBH = resampleDataset(queriesList$uBH)
	mF = resampleDataset(queriesList$mF)
	mT = resampleDataset(queriesList$mT)
	
	resampledQueriesList = list(nA=nA, uBH=uBH, mF=mF, mT=mT)
	return(resampledQueriesList)
}


getRangeForQuantileValues = function(values) {
	#return(range(values))
	return(quantile(values, probs=c(0.025,0.975)))
}


getSummaryOfQuantileValues = function(values, quantiles) {
	return(quantile(values, probs=quantiles))
}


bootstrapQueryLatencyDistr = function(estimationMethod, numResamples, queryType, opListFilename, latencyQuantilesToEstimate=c(0.5,0.9,0.99), noiseQuantiles=c(0.025, 0.5, 0.975), numSamples, desiredOutput, quantileToTruncate=0.9, quantileOfTruncatedDistr=0.9, numAlignmentPoints) {
	load(opListFilename) # ops 	
	quantileEstimates = matrix(nrow=numResamples, ncol=length(latencyQuantilesToEstimate))
	colnames(quantileEstimates) = latencyQuantilesToEstimate

	for (i in 1:numResamples) {
		print(paste("resample", i))
		resampledOps = resampleOps(ops)
		
		if (estimationMethod == "empiricalFull") {
			quantileEstimates[i,] = predictWithFullOpHist(queryType, latencyQuantilesToEstimate, resampledOps, numSamples)
		} else if (estimationMethod == "empiricalTruncated") {
			quantileEstimates[i,] = predictWithTruncatedOpHist(queryType, quantileToTruncate=quantileToTruncate, quantileOfTruncatedDistr=quantileOfTruncatedDistr, resampledOps, numSamples)
		} else if (estimationMethod == "gmm") {
			quantileEstimates[i,] = predictWithGMMOpFits(queryType, latencyQuantilesToEstimate, resampledOps, numSamples)
		} else if (estimationMethod == "empiricalExtreme") {
			quantileEstimates[i,] = predictWithExtremeOpHist(queryType, latencyQuantilesToEstimate, resampledOps, numSamples)
		} else if (estimationMethod == "empiricalConvolution") {
			quantileEstimates[i,] = predictByConvolvingFullOpHist(queryType, latencyQuantilesToEstimate, resampledOps, numAlignmentPoints)
		} else {
			print("Unsupported estimation method.")
		}
	}

	if (desiredOutput == "quantiles") {
		return(getNoiseOfQuantileEstimates(quantileEstimates, noiseQuantiles))
	} else if (desiredOutput == "values") {
		return(quantileEstimates)
	}
}


bootstrapActualQueryLatencyDistr = function(numResamples, queryType, queryListFilename, latencyQuantilesToEstimate=c(0.5,0.9,0.99), noiseQuantiles=c(0.025, 0.5, 0.975), desiredOutput) {
	load(queryListFilename) # queriesList
	quantileEstimates=matrix(nrow=numResamples, ncol=length(latencyQuantilesToEstimate))
	colnames(quantileEstimates) = latencyQuantilesToEstimate
	
	for (i in 1:numResamples) {
		print(paste("resample", i))
		resampledQueriesList = resampleQueries(queriesList)
		
		if (queryType == "needsApproval") {
			queries = resampledQueriesList$nA
		} else if (queryType == "userByHometown") {
			queries = resampledQueriesList$uBH
		} else if (queryType == "myFollowing") {
			queries = resampledQueriesList$mF
		} else if (queryType == "myThoughts") {
			queries = resampledQueriesList$mT
		} else {
			print("Unsupported query type.")
		}
		
		quantileEstimates[i,] = quantile(queries, latencyQuantilesToEstimate)
	}
	
	if (desiredOutput == "quantiles") {
		return(getNoiseOfQuantileEstimates(quantileEstimates, noiseQuantiles))
	} else if (desiredOutput == "values") {
		return(quantileEstimates)
	}
}


getNoiseOfQuantileEstimates = function(quantileEstimates, noiseQuantiles) {
	# quantileEstimates:  rows = resamples, cols = quantiles of predictions corresponding to that resample
	
	bootstrapNoise = matrix(nrow=ncol(quantileEstimates), ncol=length(noiseQuantiles))
	rownames(bootstrapNoise) = colnames(quantileEstimates)
	colnames(bootstrapNoise) = noiseQuantiles
	
	for (j in 1:ncol(quantileEstimates)) {
		bootstrapNoise[j,] = quantile(quantileEstimates[,j], noiseQuantiles)
	}
	
	return(bootstrapNoise)
}


getPredictionsMatrix = function(queryType, numResamples, numSamples, quantile) {
	predictionsMatrix = matrix(nrow=numResamples, ncol=4)
	colnames(predictionsMatrix) = c("actual", "empFull", "empTruncated", "gmm")
	
	print("actual...")
	predictionsMatrix[,1] = bootstrapActualQueryLatencyDistr(numResamples, queryType, "~/Desktop/queriesList.RData", desiredOutput="values", latencyQuantilesToEstimate=quantile)
	
	print("full empirical...")
	predictionsMatrix[,2] = bootstrapQueryLatencyDistr("empiricalFull", numResamples, queryType, "~/Desktop/opsList.RData", numSamples=numSamples, desiredOutput="values", latencyQuantilesToEstimate=quantile)
	
	print("truncated empirical...")
	predictionsMatrix[,3] = bootstrapQueryLatencyDistr("empiricalTruncated", numResamples, queryType, "~/Desktop/opsList.RData", numSamples=numSamples, desiredOutput="values", latencyQuantilesToEstimate=quantile, quantileToTruncate=0.9, quantileOfTruncatedDistr=0.5)
	
	print("gmm...")
	predictionsMatrix[,4] = bootstrapQueryLatencyDistr("gmm", numResamples, queryType, "~/Desktop/opsList.RData", numSamples=numSamples, desiredOutput="values", latencyQuantilesToEstimate=quantile)
	
	return(predictionsMatrix)
}

