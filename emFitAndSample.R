# How to make complex output (like the return values used by mclust functions)
#?list

#a = list(var1="hi", var2=matrix(data=seq(from=1,to=9,by=1),nrow=3,ncol=3))

#a
#a$var1
#a$var2


# functions for FITTING mixture models via EM

# All of these functions use random initialization
# Function returns parameters of mixture model fit by EM
fitEM = function(observationsVector, k, distrType="gaussian", modelName="V", saveRdmInit=FALSE, destinationPath="", initNum=1) {
	if (destinationPath != "") {
		dir.create(destinationPath)
	}
	
	if (distrType == "gaussian") {
		emFit = fitEMGaussian(observationsVector=observationsVector, k=k, modelName=modelName, saveRdmInit=saveRdmInit, destinationPath=destinationPath, initNum=initNum)
	} else if (distrType == "gamma") {
		emFit = fitEMGamma(observationsVector=observationsVector, k=k)
	} else {
		print("Unsupported distrType.")
	}
	
	return(emFit)
}


# Gaussian
fitEMGaussian = function(observationsVector, k, modelName="V", saveRdmInit, destinationPath, initNum) {
	library(mclust)

	# Random init	
	rdmInitMtx = randomInitEM(length(observationsVector), k)
	if (saveRdmInit) {
		save(rdmInitMtx, file=paste(destinationPath, "/rdmInit-k=", k, "-init", initNum, ".RData", sep=""))
	}
	
	# Fit Gaussian mixture model with EM
	mstepRun = mstep(modelName=modelName, data=observationsVector, z=rdmInitMtx)
	emRun = em(modelName=mstepRun$modelName, data=observationsVector, parameters=mstepRun$parameters)
	
	#print(paste("init=", initNum))
	#print(emRun$parameters)

	if (saveRdmInit) {
		save(rdmInitMtx, emRun, file=paste(destinationPath, "/rdmInitAndParams-k=", k, "-init", initNum, ".RData", sep=""))
	}

	return(emRun$parameters)
}


# Gamma
fitEMGamma = function(observationsVector, k) {
}


# may need to be adjusted to fit my implementation of EM
randomInitEM = function(nObservations, k) {
	library(mclust)
	
	rdmInitVtr = matrix(nrow=nObservations, ncol=1)
	for(i in 1:nrow(rdmInitVtr)) {
		rdmInitVtr[i] = sample(x=seq(from=1, by=1, to=k), size=1, prob=rep(1/k, k))
	}

	rdmInitMtx = unmap(rdmInitVtr)
	return(rdmInitMtx)
}

# functions for SAMPLING FROM mixture models
sampleFromMixtureModel = function(numSamples, distrType, parameters, truncateLessThanZero=FALSE) {
	if (distrType == "gaussian") {
		samples = sampleFromGaussianMixtureModel(numSamples=numSamples, parameters=parameters, truncateLessThanZero=truncateLessThanZero)
	} else if (distrType == "gamma") {
		samples = sampleFromGammaMixtureModel(numSamples=numSamples, parameters=parameters, truncateLessThanZero=truncateLessThanZero)
	} else {
		print("Unsupported distrType.")
	}
}


sampleFromGaussianMixtureModel = function(numSamples, parameters, truncateLessThanZero) {
	k = length(parameters$pro)
	samples = vector(length=numSamples)
	mixComponents = vector(length=numSamples)
	
	for (i in 1:numSamples) {
		# Choose the mixing component
		mixComponents[i] = sample(x=seq(from=1,by=1,to=k), size=1, prob=parameters$pro)
		#print(paste("component=", mixComponents[i]))
	
		# Sample from that component's Gaussian
		componentMean = parameters$mean[mixComponents[i]]
		componentVariance = parameters$variance$sigmasq[mixComponents[i]]
		
		#print(paste("mean=", componentMean, sep=""))
		#print(paste("var=", componentVariance, sep=""))
		
		samples[i] = rnorm(n=1, mean=componentMean, sd=sqrt(componentVariance))
	}
	
	#print(samples)
	
	if (truncateLessThanZero) {
		if (samples < 0 & numSamples == 1) {
			return(0)
		} else {
			return(samples[samples > 0])
		}
	} else {
		return(samples)
	}
}




