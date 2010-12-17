# 3.9.10
# Goal: predict thoughtsByHashTag's 99th percentile latency using its own op models.

basePath="/work/ksauer/3.8.10-thoughtsByHashTag-experiment"

# Initialize experiment params
startingThread=51
endingThread=100
numValidationRuns=10
latencyQuantile=0.99
queryType="thoughtsByHashTag"
numSampleSets=numValidationRuns

# Save to paramFile
save(startingThread, endingThread, numValidationRuns, latencyQuantile, queryType, numSampleSets, file=paste(basePath, "/paramFile.RData", sep=""))


# Run experiment
source(file="queryExperiment1.R")
error = queryExperiment1(basePath)
print(error)