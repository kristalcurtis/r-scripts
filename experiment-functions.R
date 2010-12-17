###  Note:  all path args should not end in "/"



getSingleDataset = function(startingThread, endingThread, logPath) {
	print(startingThread)
	data = as.data.frame(read.csv(file=paste(logPath,"/Thread-", startingThread, ".csv", sep="")))

	for (i in (startingThread+1):endingThread) {
		print(i)
		newdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-", i, ".csv", sep="")))
		data = rbind(data, newdata)
	}
	
	return(data)
}


getTrainingData = function(startingThread, endingThread, basePath) {
	print("Reading in training data...")
	
	data = getSingleDataset(startingThread, endingThread, paste(basePath,"/training-logs", sep=""))
	
	print("Saving training data...")
	save(data, file=paste(basePath, "/trainingData.RData", sep=""))

	return(data)
}


getValidationStats = function(startingThread, endingThread, basePath, numValidationRuns, latencyQuantile) {
	validationStats = matrix(nrow=numValidationRuns, ncol=2)
	colnames(validationStats) = c("latencyQuantile", "numQueries")
	
	for (j in 1:numValidationRuns) {
		print(paste("Reading in validation data from run ", j, "...", sep=""))
		vdata = getSingleDataset(startingThread, endingThread, paste(basePath,"/validation-logs/validation",j,"-logs", sep=""))
		validationStats[j,"latencyQuantile"] = quantile(vdata$latency_ms[vdata$opLevel==3], latencyQuantile)
		validationStats[j,"numQueries"] = length(which(vdata$opLevel==3))
	}
	
	print("Saving validation stats...")
	save(validationStats, file=paste(basePath, "/validationStats.RData", sep=""))
	
	return(validationStats)
}


getMultipleValidationLatencyQuantiles = function(startingThread, endingThread, basePath, numValidationRuns, latencyQuantiles) {
	validationStats = matrix(nrow=numValidationRuns, ncol=length(latencyQuantiles))
	
	for (j in 1:numValidationRuns) {
		print(paste("Reading in validation data from run ", j, "...", sep=""))
		vdata = getSingleDataset(startingThread, endingThread, paste(basePath,"/validation-logs/validation",j,"-logs", sep=""))

		for (i in 1:length(latencyQuantiles))
			validationStats[j,] = quantile(vdata$latency_ms[vdata$opLevel==3], probs=latencyQuantiles)
	}
	
	print("Saving validation latency quantiles...")
	save(validationStats, file=paste(basePath, "/validationLatencyMultipleQuantiles.RData", sep=""))
	
	return(validationStats)
}


createAndSaveThoughtstreamOpHistograms = function(basePath) {
	print("Loading training data...")
	load(file=paste(basePath, "/trainingData.RData", sep=""))  # => "data"
	
	print("Creating histograms...")
	# #breaks may be an interesting thing to adjust.  What if I had 50 breaks per hist?  10?
	h1 = hist(data[data$opLevel == 2 & data$opType == 1,"latency_ms"], breaks=25)
	h3 = hist(data[data$opLevel == 2 & data$opType == 3,"latency_ms"], breaks=25)
	h4 = hist(data[data$opLevel == 2 & data$opType == 4,"latency_ms"], breaks=25)
	h5 = hist(data[data$opLevel == 2 & data$opType == 5,"latency_ms"], breaks=25)
	h6 = hist(data[data$opLevel == 2 & data$opType == 6,"latency_ms"], breaks=25)
	h7 = hist(data[data$opLevel == 2 & data$opType == 7,"latency_ms"], breaks=20)
	h8 = hist(data[data$opLevel == 2 & data$opType == 8,"latency_ms"], breaks=20)
	h9 = hist(data[data$opLevel == 2 & data$opType == 9,"latency_ms"], breaks=20)
	
	print("Saving histograms...")
	save(h1, h3, h4, h5, h6, h7, h8, h9, file=paste(basePath,"/histograms.RData", sep=""))
}


createAndSaveUserByEmailOpHistograms = function(basePath) {
	print("Loading training data...")
	load(file=paste(basePath, "/trainingData.RData", sep=""))  # => "data"
	
	print("Creating histograms...")
	h2 = hist(data[data$opLevel == 2 & data$opType == 2,"latency_ms"], breaks=25)
	h3 = hist(data[data$opLevel == 2 & data$opType == 3,"latency_ms"], breaks=25)
	h6 = hist(data[data$opLevel == 2 & data$opType == 6,"latency_ms"], breaks=25)

	print("Saving histograms...")
	save(h2, h3, h6, file=paste(basePath,"/histograms.RData", sep=""))
}


createAndSaveUserByNameOpHistograms = function(basePath) {
	print("Loading training data...")
	load(file=paste(basePath, "/trainingData.RData", sep=""))  # => "data"
	
	print("Creating histograms...")
	h1 = hist(data[data$opLevel == 2 & data$opType == 1,"latency_ms"], breaks=25)
	h6 = hist(data[data$opLevel == 2 & data$opType == 6,"latency_ms"], breaks=25)

	print("Saving histograms...")
	save(h1, h6, file=paste(basePath,"/histograms.RData", sep=""))
}


createAndSaveThoughtsByHashTagOpHistograms = function(basePath) {
	print("Loading training data...")
	load(file=paste(basePath, "/trainingData.RData", sep=""))  # => "data"
	
	print("Creating histograms...")
	h2 = hist(data[data$opLevel==2 & data$opType==2,"latency_ms"], breaks=25)
	h5 = hist(data[data$opLevel==2 & data$opType==5,"latency_ms"], breaks=25)
	h6 = hist(data[data$opLevel==2 & data$opType==6,"latency_ms"], breaks=25)
	h8 = hist(data[data$opLevel==2 & data$opType==8,"latency_ms"], breaks=25)

	print("Saving histograms...")
	save(h2, h5, h6, h8, file=paste(basePath,"/histograms.RData", sep=""))
}


createAndSaveUserByEmailOpHistogramsFromOtherQueries = function(basePathThoughtstream, basePathThoughtsByHashTag, outputPath1, outputPath2) {
	print("Loading histograms for thoughtstream...")
	load(file=paste(basePathThoughtstream, "/histograms.RData", sep=""))  
	# => h1, h3, h4, h5, h6, h7, h8, h9
	# want to keep h3 & h6
	
	h3.thoughtstream = h3
	h6.thoughtstream = h6
	
	print("Loading histograms for thoughtsByHashTag...")
	load(file=paste(basePathThoughtsByHashTag, "/histograms.RData", sep=""))
	# => h2, h5, h6, h8
	# want to keep h2 & h6
	
	h2.thoughtsByHashTag = h2
	h6.thoughtsByHashTag = h6
	
	print("Saving histogram files (2 options) for userByEmail...")
	
	# First option:  h2=h2.thoughtsByHashTag, h3=h3.thoughtstream, h6=h6.thoughtstream
	h2=h2.thoughtsByHashTag
	h3=h3.thoughtstream
	h6=h6.thoughtstream
	save(h2,h3,h6,file=paste(outputPath1,"/histograms.RData", sep=""))
	
	# Second option:  h2=h2.thoughtsByHashTag, h3=h3.thoughtstream, h6=h6.thoughtsByHashTag
	#h2=h2.thoughtsByHashTag
	#h3=h3.thoughtstream
	h6=h6.thoughtsByHashTag
	save(h2,h3,h6,file=paste(outputPath2,"/histograms.RData", sep=""))
}


## DEPRECATED
## Note more general version which follows.
getPredictedThoughtstreamQueryLatencyQuantiles = function(numSampleSets, basePath, latencyQuantile) {
	load(file=paste(basePath, "/histograms.RData", sep=""))  # => histograms
	
	predictedQueryLatencyQuantiles = matrix(nrow=1,ncol=numSampleSets)
	
	load(file=paste(basePath, "/validationStats.RData", sep="")) # => validationStats
	numSamplesPerSet = floor(mean(validationStats[,"numQueries"]))
	print(paste("Using", numSamplesPerSet, "samples per set."))

	for (j in 1:numSampleSets) {
		print(paste("Sample Set", j))
		samples=matrix(data=0, nrow=1, ncol=numSamplesPerSet)

		for (i in 1:numSamplesPerSet) {
			samples[i] = samples[i] + sum(sample(h1$mids, 2, replace=TRUE, prob=h1$density))
			samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
			samples[i] = samples[i] + sum(sample(h4$mids, 2, replace=TRUE, prob=h4$density))
			samples[i] = samples[i] + sample(h5$mids, 1, replace=TRUE, prob=h5$density)
			samples[i] = samples[i] + sum(sample(h6$mids, 5, replace=TRUE, prob=h6$density))
			samples[i] = samples[i] + sample(h7$mids, 1, replace=TRUE, prob=h7$density)
			samples[i] = samples[i] + sample(h8$mids, 1, replace=TRUE, prob=h8$density)
			samples[i] = samples[i] + sample(h9$mids, 1, replace=TRUE, prob=h9$density)
		}

		predictedQueryLatencyQuantiles[j]=quantile(samples, latencyQuantile)	}
		
	save(predictedQueryLatencyQuantiles, file=paste(basePath,"/predictedStats.RData", sep=""))
		
	return(predictedQueryLatencyQuantiles)
}


getPredictedQueryLatencyQuantiles = function(queryType, numSampleSets, basePath, latencyQuantile) {
	predictedQueryLatencyQuantiles = matrix(nrow=1,ncol=numSampleSets)
	
	load(file=paste(basePath, "/validationStats.RData", sep="")) # => validationStats
	numSamplesPerSet = floor(mean(validationStats[,"numQueries"]))
	print(paste("Using", numSamplesPerSet, "samples per set."))

	for (j in 1:numSampleSets) {
		print(paste("Sample Set", j))
		
		if (queryType == "thoughtstream") {
			samples=thoughtstreamSampler(basePath, j, numSamplesPerSet)
		} else if (queryType == "userByEmail") {
			samples=userByEmailSampler(basePath, j, numSamplesPerSet)
		} else if (queryType == "userByName") {
			samples=userByNameSampler(basePath, j, numSamplesPerSet)
		} else if (queryType == "thoughtsByHashTag") {
			samples=thoughtsByHashTagSampler(basePath, j, numSamplesPerSet)
		} else {
			print("Incorrect queryType specified.")
		}
			

		predictedQueryLatencyQuantiles[j]=quantile(samples, latencyQuantile)	}
		
	save(predictedQueryLatencyQuantiles, file=paste(basePath,"/predictedStats.RData", sep=""))
		
	return(predictedQueryLatencyQuantiles)
}


# Allows validationStats.RData & histogram.RData to be stored in different directories.
# Output is saved in directory represented by basePathHistograms.
getPredictedQueryLatencyQuantiles2 = function(queryType, numSampleSets, basePathValidationStats, basePathHistograms, latencyQuantile) {
	predictedQueryLatencyQuantiles = matrix(nrow=1,ncol=numSampleSets)
	
	load(file=paste(basePathValidationStats, "/validationStats.RData", sep="")) # => validationStats
	numSamplesPerSet = floor(mean(validationStats[,"numQueries"]))
	print(paste("Using", numSamplesPerSet, "samples per set."))

	for (j in 1:numSampleSets) {
		print(paste("Sample Set", j))
		
		if (queryType == "thoughtstream") {
			samples=thoughtstreamSampler(basePathHistograms, j, numSamplesPerSet)
		} else if (queryType == "userByEmail") {
			samples=userByEmailSampler(basePathHistograms, j, numSamplesPerSet)
		} else if (queryType == "userByName") {
			samples=userByNameSampler(basePathHistograms, j, numSamplesPerSet)
		} else if (queryType == "thoughtsByHashTag") {
			samples=thoughtsByHashTagSampler(basePathHistograms, j, numSamplesPerSet)
		} else {
			print("Incorrect queryType specified.")
		}
			

		predictedQueryLatencyQuantiles[j]=quantile(samples, latencyQuantile)	}
		
	save(predictedQueryLatencyQuantiles, file=paste(basePathHistograms,"/predictedStats.RData", sep=""))
		
	return(predictedQueryLatencyQuantiles)
}



getPredictedQueryLatencyMultipleQuantiles = function(queryType, numSampleSets, basePathValidationStats, basePathHistograms, latencyQuantiles) {
	predictedQueryLatencyQuantiles = matrix(nrow=numSampleSets,ncol=length(latencyQuantiles))
	
	load(file=paste(basePathValidationStats, "/validationStats.RData", sep="")) # => validationStats
	numSamplesPerSet = floor(mean(validationStats[,"numQueries"]))
	print(paste("Using", numSamplesPerSet, "samples per set."))

	for (j in 1:numSampleSets) {
		print(paste("Sample Set", j))
		
		if (queryType == "thoughtstream") {
			samples=thoughtstreamSampler(basePathHistograms, j, numSamplesPerSet)
		} else if (queryType == "userByEmail") {
			samples=userByEmailSampler(basePathHistograms, j, numSamplesPerSet)
		} else if (queryType == "userByName") {
			samples=userByNameSampler(basePathHistograms, j, numSamplesPerSet)
		} else if (queryType == "thoughtsByHashTag") {
			samples=thoughtsByHashTagSampler(basePathHistograms, j, numSamplesPerSet)
		} else {
			print("Incorrect queryType specified.")
		}
			

		predictedQueryLatencyQuantiles[j,]=quantile(samples, probs=latencyQuantiles)	}
		
	save(predictedQueryLatencyQuantiles, file=paste(basePathHistograms,"/predictedStats-multipleQuantiles.RData", sep=""))
		
	return(predictedQueryLatencyQuantiles)
}



# Samplers produce & save a single sample set.
# Called from "getPredictedQueryLatencyQuantiles" function.
thoughtstreamSampler = function(basePath, sampleID, numSamples) {
	load(file=paste(basePath, "/histograms.RData", sep=""))  # => histograms
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sum(sample(h1$mids, 2, replace=TRUE, prob=h1$density))
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sum(sample(h4$mids, 2, replace=TRUE, prob=h4$density))
		samples[i] = samples[i] + sample(h5$mids, 1, replace=TRUE, prob=h5$density)
		samples[i] = samples[i] + sum(sample(h6$mids, 5, replace=TRUE, prob=h6$density))
		samples[i] = samples[i] + sample(h7$mids, 1, replace=TRUE, prob=h7$density)
		samples[i] = samples[i] + sample(h8$mids, 1, replace=TRUE, prob=h8$density)
		samples[i] = samples[i] + sample(h9$mids, 1, replace=TRUE, prob=h9$density)
	}

	save(samples, file=paste(basePath,"/sample", sampleID,".RData",sep=""))

	return(samples)
}


userByEmailSampler = function(basePath, sampleID, numSamples) {
	load(file=paste(basePath, "/histograms.RData", sep=""))  # => histograms
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sample(h2$mids, 1, replace=TRUE, prob=h2$density)
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sample(h6$mids, 1, replace=TRUE, prob=h6$density)
	}

	save(samples, file=paste(basePath,"/sample", sampleID,".RData",sep=""))

	return(samples)
}


# Saves predicted query & op latencies (rather than just sampled query latencies)
userByEmailQueryAndOpSampler = function(basePath, sampleID, numSamples) {
	load(file=paste(basePath, "/histograms.RData", sep=""))  # => histograms
	
	samples = matrix(nrow=numSamples, ncol=1)
	opSamples = matrix(nrow=numSamples, ncol=3)
	colnames(opSamples) = c("op2", "op3", "op6")

	for (i in 1:numSamples) {
		opSamples[i,"op2"] = sample(h2$mids, 1, replace=TRUE, prob=h2$density)
		opSamples[i,"op3"] = sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		opSamples[i,"op6"] = sample(h6$mids, 1, replace=TRUE, prob=h6$density)
		
		samples[i] = sum(opSamples[i,])
	}

	save(samples, opSamples, file=paste(basePath,"/sample", sampleID,"-queriesAndOps.RData",sep=""))

	#return(samples)
}


userByNameSampler = function(basePath, sampleID, numSamples) {
	load(file=paste(basePath, "/histograms.RData", sep=""))  # => histograms
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sample(h1$mids, 1, replace=TRUE, prob=h1$density)
		samples[i] = samples[i] + sample(h6$mids, 1, replace=TRUE, prob=h6$density)
	}

	save(samples, file=paste(basePath,"/sample", sampleID,".RData",sep=""))

	return(samples)
	
}


thoughtsByHashTagSampler = function(basePath, sampleID, numSamples) {
	load(file=paste(basePath, "/histograms.RData", sep=""))  # => histograms
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sample(h2$mids, 1, replace=TRUE, prob=h2$density)
		samples[i] = samples[i] + sample(h5$mids, 1, replace=TRUE, prob=h5$density)
		samples[i] = samples[i] + sum(sample(h6$mids, 2, replace=TRUE, prob=h6$density))
		samples[i] = samples[i] + sample(h8$mids, 1, replace=TRUE, prob=h8$density)
	}

	save(samples, file=paste(basePath,"/sample", sampleID,".RData",sep=""))

	return(samples)
}


## Note: just for testing
testSwitch = function(queryType) {
	if(queryType == "thoughtstream") {
		print("thoughtstream")
	} else if (queryType == "userByEmail") {
		print("userByEmail")
	} else if (queryType == "userByName") {
		print("userByName")
	} else {
		print("no")
	}	
}


# Q:  is this a good way to measure error?  Could ask PB then MJ.
getPredictionError = function(basePath) {
	load(file=paste(basePath,"/validationStats.RData",sep=""))  # => validationStats
	load(file=paste(basePath,"/predictedStats.RData",sep=""))   # => predictedQueryLatencyQuantiles
	
	#error = abs(mean(validationStats[,"latencyQuantile"]) - mean(predictedQueryLatencyQuantiles))/mean(validationStats[,"latencyQuantile"])
	error = abs(median(validationStats[,"latencyQuantile"]) - median(predictedQueryLatencyQuantiles))/median(validationStats[,"latencyQuantile"])
	return(error)
}


getPredictionError2 = function(basePathValidationStats, basePathPredictedStats) {
	load(file=paste(basePathValidationStats,"/validationStats.RData",sep=""))  # => validationStats
	load(file=paste(basePathPredictedStats,"/predictedStats.RData",sep=""))   # => predictedQueryLatencyQuantiles
	
	error = abs(median(validationStats[,"latencyQuantile"]) - median(predictedQueryLatencyQuantiles))/median(validationStats[,"latencyQuantile"])
	return(error)
}


plotPredictionErrorMultipleQuantiles = function(basePathValidationStats, basePathPredictedStats, queryType, latencyQuantiles) {
	load(file=paste(basePathValidationStats, "/validationLatencyMultipleQuantiles.RData", sep="")) # => validationStats; row=>validation run, column=>quantile
	load(file=paste(basePathPredictedStats, "/predictedStats-multipleQuantiles.RData", sep="")) # => predictedQueryLatencyQuantiles; row=>sample set, column=>quantile
	
	actualMedians = matrix(nrow=1, ncol=length(latencyQuantiles))
	predictedMedians = matrix(nrow=1, ncol=length(latencyQuantiles))
	
	# Get medians for each quantile
	for (i in 1:length(latencyQuantiles)) {
		actualMedians[i] = median(validationStats[,i])
		predictedMedians[i] = median(predictedQueryLatencyQuantiles[,i])
	}
	
	save(actualMedians, predictedMedians, file=paste(basePathPredictedStats,"/actualAndPredictedQuantiles.RData",sep=""))
	
	pdf(file=paste(basePathPredictedStats, "/quantiles.pdf", sep=""), height=6, width=6)
	par(mar=c(5,5,4,2)+0.1)
	plot(latencyQuantiles, actualMedians, ylim=c(0,max(actualMedians, predictedMedians)), col=0, xlab="Latency Quantiles", ylab="Latency (ms)", main="Comparing Actual & Predicted Latency Quantiles")
	lines(latencyQuantiles, actualMedians, lw=2, col="red")
	lines(latencyQuantiles, predictedMedians, lw=2, col="blue")
	legend("topleft", legend=c("Actual", "Predicted"), col=c("red", "blue"), lwd=2)
	dev.off()
}


plotActualAndPredictedLatencyQuantiles = function(basePathValidationStats, basePathPredictedStats, queryType) {
	load(file=paste(basePathValidationStats,"/validationStats.RData",sep=""))  # => validationStats
	load(file=paste(basePathPredictedStats,"/predictedStats.RData",sep=""))   # => predictedQueryLatencyQuantiles
	
	median.actual = median(validationStats[,"latencyQuantile"])
	median.predicted = median(predictedQueryLatencyQuantiles)
	
	ub.predicted = max(predictedQueryLatencyQuantiles) - median.predicted
	lb.predicted = median.predicted - min(predictedQueryLatencyQuantiles)

	ub.actual = max(validationStats[,"latencyQuantile"]) - median.actual
	lb.actual = median.actual - min(validationStats[,"latencyQuantile"])

	pdf(file=paste(basePathPredictedStats,"/error.pdf",sep=""), height=6, width=6)
	par(mar=c(5,5,4,2)+0.1)
	x.abscissa = barplot(c(median.predicted,median.actual), ylim=c(0,1.1*max(validationStats[,"latencyQuantile"], predictedQueryLatencyQuantiles)), names.arg=c("Sampled", "Actual"), col=c("blue","cyan"), ylab="Latency (ms)", main=paste(queryType, "Query:  Sampled vs. Actual Latency"))
	superpose.eb(x.abscissa,c(median.predicted,median.actual),c(lb.predicted,lb.actual),c(ub.predicted,ub.actual),col="green",lwd=2)
	
	text()
	
	dev.off()
}


superpose.eb = function (x, y, ebl, ebu = ebl, length.arg = 0.08, col, lwd) {
	arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3, length=length.arg, col=col, lwd=lwd)
}


histWithMedianToPdf = function(histData, filename, breaks, xlab, main, col, legendLocation) {
	pdf(file=filename, width=5, height=5)
	par(mar=c(5,5,4,)1)
	hist(histData, breaks=breaks, xlab=xlab, main=main)
	medianVal = median(histData)
	abline(v=medianVal, col=col, lw=2)
	legend(legendLocation, legend=c(paste("median = ", round(medianVal, digits=2), "ms", sep="")), lwd=2, col=col)
	dev.off()
}


# Quantile latency => center of bin
# Percent of quantile => width of bin
# Return ops corresponding to queries that fall within the specified bin
# Just for actual data; not for samples
binActualDataByDifferenceFromQuantileLatency = function(data, quantileLatency, percentDifferenceFromQuantileLatency) {
	quantileValidationLatency = quantile(data$latency_ms[data$opLevel==3], probs=quantileLatency)
	targetBinDeviationFromQuantile = percentDifferenceFromQuantileLatency/100*quantileValidationLatency

	queriesWithinBin = data[which(data$opLevel==3 & abs(medianValidationLatency - data$latency_ms) < targetBinDeviationFromQuantile),]

	#print("dim:")
	#print(dim(queriesWithinBin))
	#print("range:")
	#print(range(queriesWithinBin$latency_ms))
	#print("mtx:")
	#print(queriesWithinBin[1:10,])

	# => (threadNum, queryNum) id of each query whose latency falls within this range.

	#data[data$threadNum==51 & data$queryNum==12 & data$opLevel==2,]

	# Next, get its ops and add those rows to another array.
	currentThreadNum = queriesWithinBin[1,"threadNum"]
	currentQueryNum = queriesWithinBin[1,"queryNum"]

	print(1)
	opsWithinBin = data[data$threadNum==currentThreadNum & data$queryNum==currentQueryNum & data$opLevel==2,]

	for (i in 2:nrow(queriesWithinBin)) {
		print(i)	

		currentThreadNum = queriesWithinBin[i,"threadNum"]
		currentQueryNum = queriesWithinBin[i,"queryNum"]

		ops = data[data$threadNum==currentThreadNum & data$queryNum==currentQueryNum & data$opLevel==2,]
	
		opsWithinBin = rbind(opsWithinBin, ops)
	}

	dim(opsWithinBin)
	opsWithinBin[1:10,]
	
	
	return(opsWithinBin)
}


binSamplesByDifferenceFromQuantileLatency = function(querySamples, opSamples, quantileLatency, percentDifferenceFromQuantileLatency) {
	binCenter = quantile(querySamples, probs=quantileLatency)
	targetBinDeviationFromQuantile = percentDifferenceFromQuantileLatency/100*binCenter
	
	samplesWithinBin = samples[which(abs(samples - binCenter) < targetBinDeviationFromQuantile)]
	
	return(opSamples[which(abs(samples - binCenter) < targetBinDeviationFromQuantile),])
}




