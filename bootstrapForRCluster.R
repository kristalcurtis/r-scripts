# assumes it's run from scads/experiments/client/performance/logparsing/src/main/R

source("bootstrapFunctions.R")

dataPath = "/work/ksauer/7.27.10-bootstrap"

load(paste(dataPath, "/ops.RData", sep=""))
load(paste(dataPath, "/queryObs.RData", sep=""))

# create op list file
ops = getOpList(paste(dataPath, "/ops.RData", sep=""))
save(ops, file=paste(dataPath, "/opList.RData", sep=""))

# bootstrap empiricalExtreme
uBHquantileEstimates = bootstrapQueryLatencyDistr("empiricalExtreme", 10, "userByHometown", paste(dataPath, "/opList.RData", sep=""), c(0.5,0.9,0.99), c(0.025, 0.5, 0.975), 1000, "values")

# bootstrap convolution
source("convolutionForOpCombination-functions.R")

ubhConvDensity = getQueryDensityViaConvolution(paste(dataPath, "/ops.RData", sep=""), "userByHometown", 20000)


uBHquantileEstimates.conv = bootstrapQueryLatencyDistr("empiricalConvolution", 10, "userByHometown", paste(dataPath, "/opList.RData", sep=""), c(0.5,0.9,0.99), c(0.025, 0.5, 0.975), 1000, "values", opFilename=paste(dataPath, "/ops.RData", sep=""), numAlignmentPoints=20000)


# testing convolution bootstrap on laptop

convQuantiles = predictByConvolvingFullOpHist("userByHometown", c(0.5, 0.9, 0.99), ops, 20000)

dataPath = "~/Desktop"
uBHquantileEstimates.conv = bootstrapQueryLatencyDistr("empiricalConvolution", 10, "userByHometown", paste(dataPath, "/opsList.RData", sep=""), c(0.5,0.9,0.99), c(0.025, 0.5, 0.975), 1000, "values", numAlignmentPoints=20000)


uBHactualQuantiles = bootstrapActualQueryLatencyDistr(10, "userByHometown", "~/Desktop/queriesList.RData", c(0.5,0.9,0.99), c(0.025, 0.5, 0.975), "values")