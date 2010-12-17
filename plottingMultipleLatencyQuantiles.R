startingThread=51
endingThread=100
latencyQuantiles=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
numValidationRuns=10
numSampleSets=numValidationRuns

# userByEmail
queryType="userByEmail"
basePathValidationStats="/work/ksauer/2.23.10-userByEmail-experiment"
basePathPredictedStats=basePathValidationStats

getPredictedQueryLatencyMultipleQuantiles(queryType, numSampleSets, basePathValidationStats, basePathPredictedStats, latencyQuantiles)

getMultipleValidationLatencyQuantiles(startingThread, endingThread, basePathValidationStats, numValidationRuns, latencyQuantiles)

plotPredictionErrorMultipleQuantiles(basePathValidationStats, basePathPredictedStats, queryType, latencyQuantiles)


# thoughtstream
queryType="thoughtstream"
basePathValidationStats="/work/ksauer/2.12.10-thoughtstream-experiment"
basePathPredictedStats=basePathValidationStats

getPredictedQueryLatencyMultipleQuantiles(queryType, numSampleSets, basePathValidationStats, basePathPredictedStats, latencyQuantiles)

getMultipleValidationLatencyQuantiles(startingThread, endingThread, basePathValidationStats, numValidationRuns, latencyQuantiles)

plotPredictionErrorMultipleQuantiles(basePathValidationStats, basePathPredictedStats, queryType, latencyQuantiles)


# thoughtsByHashTag
queryType="thoughtsByHashTag"
basePathValidationStats="/work/ksauer/3.8.10-thoughtsByHashTag-experiment"
basePathPredictedStats=basePathValidationStats

getPredictedQueryLatencyMultipleQuantiles(queryType, numSampleSets, basePathValidationStats, basePathPredictedStats, latencyQuantiles)

getMultipleValidationLatencyQuantiles(startingThread, endingThread, basePathValidationStats, numValidationRuns, latencyQuantiles)

plotPredictionErrorMultipleQuantiles(basePathValidationStats, basePathPredictedStats, queryType, latencyQuantiles)


# userByName
queryType="userByName"
basePathValidationStats="/work/ksauer/2.23.10-userByName-experiment"
basePathPredictedStats=basePathValidationStats

getPredictedQueryLatencyMultipleQuantiles(queryType, numSampleSets, basePathValidationStats, basePathPredictedStats, latencyQuantiles)

getMultipleValidationLatencyQuantiles(startingThread, endingThread, basePathValidationStats, numValidationRuns, latencyQuantiles)

plotPredictionErrorMultipleQuantiles(basePathValidationStats, basePathPredictedStats, queryType, latencyQuantiles)
