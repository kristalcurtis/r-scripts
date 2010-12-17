## 6.22.10  Use GMMs that I fit to the op data to try predicting the query latency

rm(list=ls())

source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")

load("~/Desktop/ops.RData")

k=5
numRdmInits=5
pathPrefix = "~/Desktop/op3-gettingParams"

for (i in 1:numRdmInits) {
	print(paste(("rdm init", i)))
	emParams = fitEM(observationsVector=op3$latency_ms, k=k, distrType="gaussian", modelName="V", saveRdmInit=TRUE, destinationPath= pathPrefix, initNum=i)
}



# Load in em results; pick the one with the best loglik
emLogLik = vector(mode="numeric", length=numRdmInits)

for (i in 1:numRdmInits) {
	load(paste(pathPrefix, "/rdmInitAndParams-k=", k, "-init", i, ".RData", sep=""))
	emLogLik[i] = emRun$loglik
}

bestRdmInit = which.max(emLogLik) # => best rdm init

emLogLik
bestRdmInit

# Use params from best rdm init 
load(paste(pathPrefix, "/rdmInitAndParams-k=", k, "-init", bestRdmInit, ".RData", sep=""))

emParams = emRun$parameters

op3GMMSamples = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=emParams, truncateLessThanZero=TRUE)



# Turn the above param acquisition into a function
getGMMParamsGivenK = function(observationsVector, pathPrefix, k, numRdmInits=5) {
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")

	# Save the params obtained from several random inits
	for (i in 1:numRdmInits) {
		print(paste("rdm init", i))
		emParams = fitEM(observationsVector=observationsVector, k=k, distrType="gaussian", modelName="V", saveRdmInit=TRUE, destinationPath=pathPrefix, initNum=i)
	}
	
	
	# Determine which random init yielded the best param vtr
	emLogLik = vector(mode="numeric", length=numRdmInits)
	
	for (i in 1:numRdmInits) {
		load(paste(pathPrefix, "/rdmInitAndParams-k=", k, "-init", i, ".RData", sep=""))
		emLogLik[i] = emRun$loglik
	}	

	bestRdmInit = which.max(emLogLik) # => best rdm init

	
	# Return the best params
	load(paste(pathPrefix, "/rdmInitAndParams-k=", k, "-init", bestRdmInit, ".RData", sep=""))

	emParams = emRun$parameters
	return(emParams)
}



getBestGMMParamVector = function(pathPrefix, k, numRdmInits=5) {
	# Determine which random init yielded the best param vtr
	emLogLik = vector(mode="numeric", length=numRdmInits)
	
	for (i in 1:numRdmInits) {
		load(paste(pathPrefix, "/rdmInitAndParams-k=", k, "-init", i, ".RData", sep=""))
		emLogLik[i] = emRun$loglik
	}	

	bestRdmInit = which.max(emLogLik) # => best rdm init

	
	# Return the best params
	load(paste(pathPrefix, "/rdmInitAndParams-k=", k, "-init", bestRdmInit, ".RData", sep=""))

	emParams = emRun$parameters
	return(emParams)

}



# try plugging into query latency prediction
# function is in analyticalModelingMassAndTailSeparately.R

checkSingleOpModelImpactOnQueryModel(opSamples=op3GMMSamples, opNum=3, prefix="~/Desktop/checking-gmm-on-query-distr-truncate", queryType="userByHometown")
checkSingleOpModelImpactOnQueryModel(opSamples=op3GMMSamples, opNum=3, prefix="~/Desktop/checking-gmm-on-query-distr-truncate", queryType="needsApproval")


# Check log likelihood computation
#myLogLik = computeGMMLogLikelihood(op3$latency_ms, emParams)
#myLogLik
#kFoldCVToChooseNumMixtureComponentsForGMM(dataVector=op3$latency_ms, destinationPath="~/Desktop/op3-cv-loglik")



# Check out several fits for each op

checkSeveralFits = function(opData, opNum, fits=c("gamma", "lognormal", "gmm"), prefix, gmmParams) {
	dir.create(prefix)

	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/graphingAssistFunctions.R")
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/getDistrSamplers.R")
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")
	
	
	pdf(paste(prefix, "/checkFits-op", opNum, ".pdf", sep=""))
	par(mfrow=c(length(fits)+1,1), mar=c(5,5,4,2))
	
	hist(opData, breaks=100)	
	plotMed90th99thQuantilesAndLegend(opData)
	
	for (i in 1:length(fits)) {
		if (fits[i] == "gamma") {
			samples = getGammaSamples(observationsVector=opData, numSamples=1000)
		} else if (fits[i] == "lognormal") {
			samples = getLogNormalSamples(observationsVector=opData, numSamples=1000)
		} else if (fits[i] == "gmm") {
			samples = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=gmmParams, truncateLessThanZero=TRUE)
		} else {
			"Unsupported fit type."
		}

		hist(samples, breaks=100)
		plotMed90th99thQuantilesAndLegend(samples)
	}
	dev.off()
}


rm(list=ls())
load("~/Desktop/ops.RData")
load("/Users/radlab/Desktop/op3-gettingParams/rdmInitAndParams-k=5-init5.RData")
ls()


checkSeveralFits(op3$latency_ms, opNum=3, prefix="~/Desktop/fits", gmmParams=emRun$parameters)
checkSeveralFits(op3$latency_ms, opNum=3, prefix="~/Desktop/fits2", gmmParams=getGMMParamsGivenK(observationsVector=op3$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=5))

op = op9
opNum = 9
k=c(3,8,5,3,5,4,4,5,2)
checkSeveralFits(op$latency_ms, opNum=opNum, prefix="~/Desktop/fits", gmmParams=getGMMParamsGivenK(observationsVector=op$latency_ms, pathPrefix=paste("~/Desktop/op", opNum, "GMMParams", sep=""), k=k[opNum]), fits="gmm")




# Get ops' GMM samples and use to do query latency pred

rm(list=ls())
source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")
load("~/Desktop/ops.RData")
k=c(3,8,5,3,5,4,4,5,2) # from CV

op1 = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=getGMMParamsGivenK(observationsVector=op1$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=k[1]), truncateLessThanZero=TRUE)
op2 = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=getGMMParamsGivenK(observationsVector=op2$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=k[2]), truncateLessThanZero=TRUE)
op3 = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=getGMMParamsGivenK(observationsVector=op3$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=k[3]), truncateLessThanZero=TRUE)
op4 = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=getGMMParamsGivenK(observationsVector=op4$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=k[4]), truncateLessThanZero=TRUE)
op5 = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=getGMMParamsGivenK(observationsVector=op5$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=k[5]), truncateLessThanZero=TRUE)
op6 = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=getGMMParamsGivenK(observationsVector=op6$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=k[6]), truncateLessThanZero=TRUE)
op7 = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=getGMMParamsGivenK(observationsVector=op7$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=k[7]), truncateLessThanZero=TRUE)
op8 = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=getGMMParamsGivenK(observationsVector=op8$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=k[8]), truncateLessThanZero=TRUE)
op9 = sampleFromMixtureModel(numSamples=1000, distrType="gaussian", parameters=getGMMParamsGivenK(observationsVector=op9$latency_ms, pathPrefix="~/Desktop/gmmParamFcn", k=k[9]), truncateLessThanZero=TRUE)

save(op1, op2, op3, op4, op5, op6, op7, op8, op9, file="~/Desktop/opsGMM.RData")


# Oops; should save hist, not samples
op1distr = hist(op1distr, breaks=1000)
op2distr = hist(op2distr, breaks=1000)
op3distr = hist(op3distr, breaks=1000)
op4distr = hist(op4distr, breaks=1000)
op5distr = hist(op5distr, breaks=1000)
op6distr = hist(op6distr, breaks=1000)
op7distr = hist(op7distr, breaks=1000)
op8distr = hist(op8distr, breaks=1000)
op9distr = hist(op9distr, breaks=1000)
save(op1distr, op2distr, op3distr, op4distr, op5distr, op6distr, op7distr, op8distr, op9distr, file="~/Desktop/gmmOpDistr.RData")

# Checking query pred
source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/modeling-harness-analytical-distr.R")
computeAndComparePredictedQueryDistr(queryObsFile="~/Desktop/queryObs.RData", opDistrFile="~/Desktop/gmmOpDistr.RData", destinationPath="~/Desktop/gmmQueryPred-1kSamples", queryType="userByHometown", numSamples=1000)
computeAndComparePredictedQueryDistr(queryObsFile="~/Desktop/queryObs.RData", opDistrFile="~/Desktop/gmmOpDistr.RData", destinationPath="~/Desktop/gmmQueryPred-1kSamples", queryType="needsApproval", numSamples=1000)
computeAndComparePredictedQueryDistr(queryObsFile="~/Desktop/queryObs.RData", opDistrFile="~/Desktop/gmmOpDistr.RData", destinationPath="~/Desktop/gmmQueryPred-1kSamples", queryType="myFollowing", numSamples=1000)
computeAndComparePredictedQueryDistr(queryObsFile="~/Desktop/queryObs.RData", opDistrFile="~/Desktop/gmmOpDistr.RData", destinationPath="~/Desktop/gmmQueryPred-1kSamples", queryType="myThoughts", numSamples=1000)




# Save params for each op
op1params = getBestGMMParamVector(pathPrefix=paste("~/Desktop/op1GMMParams", sep=""), k=k[1])
op2params = getBestGMMParamVector(pathPrefix=paste("~/Desktop/op2GMMParams", sep=""), k=k[2])
op3params = getBestGMMParamVector(pathPrefix=paste("~/Desktop/op3GMMParams", sep=""), k=k[3])
op4params = getBestGMMParamVector(pathPrefix=paste("~/Desktop/op4GMMParams", sep=""), k=k[4])
op5params = getBestGMMParamVector(pathPrefix=paste("~/Desktop/op5GMMParams", sep=""), k=k[5])
op6params = getBestGMMParamVector(pathPrefix=paste("~/Desktop/op6GMMParams", sep=""), k=k[6])
op7params = getBestGMMParamVector(pathPrefix=paste("~/Desktop/op7GMMParams", sep=""), k=k[7])
op8params = getBestGMMParamVector(pathPrefix=paste("~/Desktop/op8GMMParams", sep=""), k=k[8])
op9params = getBestGMMParamVector(pathPrefix=paste("~/Desktop/op9GMMParams", sep=""), k=k[9])



save(op1params, op2params, op3params, op4params, op5params, op6params, op7params, op8params, op9params, file="~/Desktop/opGMMParams.RData")


computeAndComparePredictedQueryDistr(queryObsFile="~/Desktop/queryObs.RData", opDistrFile="~/Desktop/opGMMParams.RData", destinationPath="~/Desktop/gmmQueryPred-gmmSampler2", queryType="userByHometown", numSamples=10000)

