#needsApprovalSampler = function(h1, h3, h4, h6, h9, numSamples) {
needsApprovalSampler = function(op1distr, op3distr, op4distr, op6distr, op9distr, numSamples) {
#needsApprovalSampler = function(opDistrFile, numSamples) {
#	load(opDistrFile)
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sum(sample(op1distr$mids, 2, replace=TRUE, prob=op1distr$density))
		samples[i] = samples[i] + sample(op3distr$mids, 1, replace=TRUE, prob=op3distr$density)
		samples[i] = samples[i] + sample(op4distr$mids, 1, replace=TRUE, prob=op4distr$density)
		samples[i] = samples[i] + sum(sample(op6distr$mids, 3, replace=TRUE, prob=op6distr$density))
		samples[i] = samples[i] + sample(op9distr$mids, 1, replace=TRUE, prob=op9distr$density)
	}

	return(samples)
}

needsApprovalSamplerFromFile = function(opDistrFile, numSamples) {
	load(opDistrFile)
	
	samples = needsApprovalSampler(op1distr, op3distr, op4distr, op6distr, op9distr, numSamples)
	
	return(samples)
}

needsApprovalGMMSampler = function(opParamFile, numSamples) {
	load(opParamFile)
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)
	
	for(i in 1:numSamples) {
		samples[i] = samples[i] + sum(sampleFromMixtureModel(numSamples=2, distrType="gaussian", parameters=op1GMMParams, truncateLessThanZero=TRUE))
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op3GMMParams, truncateLessThanZero=TRUE)
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op4GMMParams, truncateLessThanZero=TRUE)
		samples[i] = samples[i] + sum(sampleFromMixtureModel(numSamples=3, distrType="gaussian", parameters=op6GMMParams, truncateLessThanZero=TRUE))
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op9GMMParams, truncateLessThanZero=TRUE)
	}
	
	return(samples)
}

#userByHometownSampler = function(h2, h3, h6, numSamples) {
userByHometownSampler = function(op2distr, op3distr, op6distr, numSamples) {
#userByHometownSampler = function(opDistrFile, numSamples) {
#	load(opDistrFile)

	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sample(op2distr$mids, 1, replace=TRUE, prob=op2distr$density)
		samples[i] = samples[i] + sample(op3distr$mids, 1, replace=TRUE, prob=op3distr$density)
		samples[i] = samples[i] + sample(op6distr$mids, 1, replace=TRUE, prob=op6distr$density)
	}

	return(samples)
}

userByHometownSamplerFromFile = function(opDistrFile, numSamples) {
	load(opDistrFile)
	samples = userByHometownSampler(op2distr, op3distr, op6distr, numSamples)
	return(samples)
}


userByHometownGMMSampler = function(opParamFile, numSamples) {
	load(opParamFile)
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)
	
	for (i in 1:numSamples) {
		#print(i)
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op2GMMParams, truncateLessThanZero=TRUE)
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op3GMMParams, truncateLessThanZero=TRUE)
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op6GMMParams, truncateLessThanZero=TRUE)
	}
	
	return(samples)
}


#myFollowingSampler = function(h1, h4, h5, h6, numSamples) {
myFollowingSampler = function(op1distr, op4distr, op5distr, op6distr, numSamples) {
#myFollowingSampler = function(opDistrFile, numSamples) {
#	load(opDistrFile)
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sum(sample(op1distr$mids, 2, replace=TRUE, prob=op1distr$density))
		samples[i] = samples[i] + sample(op4distr$mids, 1, replace=TRUE, prob=op4distr$density)
		samples[i] = samples[i] + sample(op5distr$mids, 1, replace=TRUE, prob=op5distr$density)
		samples[i] = samples[i] + sum(sample(op6distr$mids, 4, replace=TRUE, prob=op6distr$density))
	}

	return(samples)
}

myFollowingSamplerFromFile = function(opDistrFile, numSamples) {
	load(opDistrFile)
	samples = myFollowingSampler(op1distr, op4distr, op5distr, op6distr, numSamples)
	return(samples)
}

myFollowingGMMSampler = function(opParamFile, numSamples) {
	load(opParamFile)
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)
	
	for(i in 1:numSamples) {
		samples[i] = samples[i] + sum(sampleFromMixtureModel(numSamples=2, distrType="gaussian", parameters=op1GMMParams, truncateLessThanZero=TRUE))
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op4GMMParams, truncateLessThanZero=TRUE)
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op5GMMParams, truncateLessThanZero=TRUE)
		samples[i] = samples[i] + sum(sampleFromMixtureModel(numSamples=4, distrType="gaussian", parameters=op6GMMParams, truncateLessThanZero=TRUE))
	}
	
	return(samples)
}

#myThoughtsSampler = function(h1, h4, h6, h9, numSamples) {
myThoughtsSampler = function(op1distr, op4distr, op6distr, op9distr, numSamples) {
#myThoughtsSampler = function(opDistrFile, numSamples) {
#	load(opDistrFile)
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sum(sample(op1distr$mids, 2, replace=TRUE, prob=op1distr$density))
		samples[i] = samples[i] + sample(op4distr$mids, 1, replace=TRUE, prob=op4distr$density)
		samples[i] = samples[i] + sum(sample(op6distr$mids, 3, replace=TRUE, prob=op6distr$density))
		samples[i] = samples[i] + sample(op9distr$mids, 1, replace=TRUE, prob=op9distr$density)
	}

	return(samples)
}

myThoughtsSamplerFromFile = function(opDistrFile, numSamples) {
	load(opDistrFile)
	samples = myThoughtsSampler(op1distr, op4distr, op6distr, op9distr, numSamples)
	return(samples)
}


myThoughtsGMMSampler = function(opParamFile, numSamples) {
	load(opParamFile)
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")
	
	samples=matrix(data=0, nrow=1, ncol=numSamples)
	
	for(i in 1:numSamples) {
		samples[i] = samples[i] + sum(sampleFromMixtureModel(numSamples=2, distrType="gaussian", parameters=op1GMMParams, truncateLessThanZero=TRUE))
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op4GMMParams, truncateLessThanZero=TRUE)
		samples[i] = samples[i] + sum(sampleFromMixtureModel(numSamples=3, distrType="gaussian", parameters=op6GMMParams, truncateLessThanZero=TRUE))
		samples[i] = samples[i] + sampleFromMixtureModel(numSamples=1, distrType="gaussian", parameters=op9GMMParams, truncateLessThanZero=TRUE)
	}	
	
	return(samples)
}


