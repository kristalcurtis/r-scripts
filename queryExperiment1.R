# Goal:  use a query's own op models to predict its latency for a certain quantile.

# Arg passed to function:  basePath

# Log files should already be deinterleaved & parsed.
# ie, Expects to find the following files in "basePath":
# training-logs/Thread-*.csv
# validation+-logs/Thread-*.csv
# paramFile.RData
#   *:  startingThread to endingThread
#   +:  1 to numValidationRuns

# Values that should appear in paramFile.RData:
# startingThread
# endingThread
# numValidationRuns
# latencyQuantile:  \in (0,1)
# queryType:  string rep of query (eg, "thoughtstream")
# numSampleSets


queryExperiment1 = function(basePath) {
	source("experiment-functions.R")
	print("Loading params...")
	load(file=paste(basePath, "/paramFile.RData", sep=""))
	
	## Training Phase
	print("TRAINING PHASE:")
	print("Loading training data...")
	trainingData = getTrainingData(startingThread, endingThread, basePath)
	
	print("Creating & saving histograms...")
	if (queryType == "thoughtstream") {
		createAndSaveThoughtstreamOpHistograms(basePath)
	} else if (queryType == "userByEmail") {
		createAndSaveUserByEmailOpHistograms(basePath)
	} else if (queryType == "userByName") {
		createAndSaveUserByNameOpHistograms(basePath)
	} else if (queryType == "thoughtsByHashTag") {
		createAndSaveThoughtsByHashTagOpHistograms(basePath)
	} else {
		return("Unrecognized query type.")
	}

	# Sanity Check
	print("Sanity check the training data's dim:")
	print(dim(trainingData))
	print("Sanity check the # queries in the training data:")
	print(length(which(trainingData$opLevel==3)))


	## Validation Phase
	print("VALIDATION PHASE:")
	print("Getting validation stats...")
	getValidationStats(startingThread, endingThread, basePath, numValidationRuns, latencyQuantile)
	print("Getting predicted latency...")
	getPredictedQueryLatencyQuantiles(queryType, numSampleSets, basePath, latencyQuantile)
	error = getPredictionError(basePath)	
	print(paste("Error:", error))
	
	return(error)
}

