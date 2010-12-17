## 6.21.10
# Testing fitting Gaussian mixture model to op distr via EM
rm(list=ls())

load("~/Desktop/ops.RData")
ls()

hist(op3$latency_ms, breaks=100)



source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")

k=4
observationsVector=op3$latency_ms
#observationsVector=op2$latency_ms
#observationsVector=op3$latency_ms[which(op3$latency_ms > 10)]
#emRun = fitEMGaussian(observationsVector=observationsVector, k=k, modelName="V")
emParams = fitEM(observationsVector=observationsVector, k=k, distrType="gaussian", modelName="V")
emParams


par(mfrow=c(2,2))

plot(density(observationsVector))
for(i in 1:k) {
	abline(v=emRun$parameters$mean[i], lw=2, col="turquoise")
}


plot(density(observationsVector), xlim=c(0,1.1*quantile(observationsVector,0.99)))
for(i in 1:k) {
	abline(v=emRun$parameters$mean[i], lw=2, col="turquoise")
}


# Try sampling from the mixture model I obtained above
samples = sampleFromMixtureModel(numSamples=1000, parameters=emRun$parameters, distrType="gaussian")



# Compare key quantiles of actual, fitted tail
summary(observationsVector)
summary(samples)


# Visually compare actual, fitted tail
source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/graphingAssistFunctions.R")
plot(density(observationsVector)) 
plotMed90th99thQuantilesAndLegend(observationsVector)

plot(density(samples))
plotMed90th99thQuantilesAndLegend(samples)





## More systematic way to fit a Gaussian mixture model to the operator distribution

fitGMMToOperatorData = function(operatorData, kmax, nRdmInits, destinationPath, numSamples) {
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/graphingAssistFunctions.R")
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")
	
	dir.create(destinationPath)

	# Vary k from 2 to kmax
	for (k in 2:kmax) {
		
		# Do several rdm inits
		for (i in 1:nRdmInits) {
			# Fit GMM to operatorData via EM
			emParams = fitEM(observationsVector=operatorData, k=k, distrType="gaussian", modelName="V", saveRdmInit=TRUE, destinationPath=destinationPath, initNum=i)
			
			# Sample from fitted GMM
			samples = sampleFromMixtureModel(numSamples=numSamples, distrType="gaussian", parameters=emParams, truncateLessThanZero=TRUE)
			
			# Check diff b/t 99th percentile of operatorData, samples from GMM
			pdf(file=paste(destinationPath, "/compareActualAndFitted-k=", k, "-init", i, ".pdf", sep=""))
			par(mfrow=c(1,2))
			plot(density(operatorData)) 
			plotMed90th99thQuantilesAndLegend(operatorData)

			plot(density(samples))
			plotMed90th99thQuantilesAndLegend(samples)
			dev.off()
			
			# Save emParams & samples
			save(emParams, samples, file=paste(destinationPath, "/paramsAndSamples-k=", k, "-init", i, ".pdf", sep=""))			
		}
		
	}
	
}


fitGMMToOperatorData(operatorData=op3$latency_ms, kmax=10, nRdmInits=5, destinationPath="~/Desktop/op3-gmm-allrdminits-truncate", numSamples=1000)



# Check GMM likelihood computation
source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/fitEMUsingCV.R")

lkhood.op3 = computeGMMLikelihood(op3$latency_ms, emParams)
logLkhood.op3 = computeGMMLogLikelihood(op3$latency_ms, emParams)


# Check division of data into k folds
rdmAssignmentVtr = splitDataIntoKFolds(op3$latency_ms[1:55],k=10)

rdmAssignmentVtr
sort(unique(rdmAssignmentVtr))

