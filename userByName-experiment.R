# 3.2.10  
# Goal: predict userByName's 99th percentile latency using its own op models.

basePath="/work/ksauer/2.23.10-userByName-experiment"

# Initialize experiment params
startingThread=51
endingThread=100
numValidationRuns=10
latencyQuantile=0.99
queryType="userByName"
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
