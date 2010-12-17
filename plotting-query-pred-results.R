source("/work/ksauer/scads/experiments/client/performance/logparsing/src/main/R/experiment-functions.R")

# thoughtsByHashTag
basePath="/work/ksauer/3.8.10-thoughtsByHashTag-experiment"
queryType="thoughtsByHashTag"
plotActualAndPredictedLatencyQuantiles(basePath, basePath, queryType)

# thoughtstream
basePathValidationStats="/work/ksauer/2.12.10-thoughtstream-experiment"
basePathPredictedStats=basePathValidationStats
queryType="thoughtstream"
plotActualAndPredictedLatencyQuantiles(basePathValidationStats, basePathPredictedStats, queryType)

# userByEmail1
basePath="/work/ksauer/2.23.10-userByEmail-experiment"
queryType="userByEmail"
plotActualAndPredictedLatencyQuantiles(basePath, basePath, queryType)

# userByName
basePath="/work/ksauer/2.23.10-userByName-experiment"
queryType="userByName"
plotActualAndPredictedLatencyQuantiles(basePath, basePath, queryType)

# userByEmail2
# option1
basePathValidationStats="/work/ksauer/2.23.10-userByEmail-experiment"
basePathPredictedStats="/work/ksauer/3.9.10-userByEmail-experiment-2/option1"
queryType="userByEmail"
plotActualAndPredictedLatencyQuantiles(basePathValidationStats, basePathPredictedStats, queryType)


# option2
basePathValidationStats="/work/ksauer/2.23.10-userByEmail-experiment"
basePathPredictedStats="/work/ksauer/3.9.10-userByEmail-experiment-2/option2"
queryType="userByEmail"
plotActualAndPredictedLatencyQuantiles(basePathValidationStats, basePathPredictedStats, queryType)

