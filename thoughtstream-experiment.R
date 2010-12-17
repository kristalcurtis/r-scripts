# 3.2.10  
# Goal: predict thoughtstream's 99th percentile latency using its own op models.

# Initialize experiment params
startingThread=51
endingThread=100
basePath="/work/ksauer/2.12.10-thoughtstream-experiment"
numValidationRuns=10
latencyQuantile=0.99
queryType="thoughtstream"
numSampleSets=numValidationRuns


## Training Phase
trainingData = getTrainingData(startingThread, endingThread, basePath)
createAndSaveThoughtstreamOpHistograms(basePath)

# Sanity Check
dim(trainingData)
numQueries = length(which(trainingData$opLevel==3))
numQueries


## Validation Phase
getValidationStats(startingThread, endingThread, basePath, numValidationRuns, latencyQuantile)
getPredictedQueryLatencyQuantiles(queryType, numSampleSets, basePath, latencyQuantile)
getPredictionError(basePath)



## VERSION 2:  using "queryExperiment1" function
basePath="/work/ksauer/2.12.10-thoughtstream-experiment"

# Initialize experiment params
startingThread=51
endingThread=100
numValidationRuns=10
latencyQuantile=0.99
queryType="thoughtstream"
numSampleSets=numValidationRuns

# Save to paramFile
save(startingThread, endingThread, numValidationRuns, latencyQuantile, queryType, numSampleSets, file=paste(basePath, "/paramFile.RData", sep=""))

# Run experiment
#source(file="queryExperiment1.R")
error = queryExperiment1(basePath)
print(error)


# Just get error
source("/work/ksauer/scads/experiments/client/performance/logparsing/src/main/R/experiment-functions.R")
error = getPredictionError(basePath)
error