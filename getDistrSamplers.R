library(moments)

getGammaSamples = function(observationsVector, numSamples) {
	s = log(1/(length(observationsVector)) * sum(observationsVector)) - 1/length(observationsVector) * sum(log(observationsVector))
	k = (3 - s + sqrt((s-3)^2 + 24*s))/(12*s)
	theta = 1/(k*length(observationsVector)) * sum(observationsVector)
	# Could improve params iteratively

	samples = rgamma(numSamples, shape=k, scale=theta)
	return(samples)
}


getGammaSamplesMomentMatching = function(observationsVector, numSamples) {
	skew = skewness(observationsVector)
	k.mm = 4/(skew^2)
	variance = var(observationsVector)
	theta.mm = sqrt(variance/k.mm)
	mu = mean(observationsVector)
	shift = mu - k.mm*theta.mm 

	samples = rgamma(numSamples, shape=k.mm, scale=theta.mm) + shift
	return(samples)
}


getNormalSamples = function(observationsVector, numSamples) {
	mu = mean(observationsVector)
	variance = var(observationsVector)

	samples = rnorm(numSamples, mean=mu, sd=sqrt(variance))
	return(samples)
}


getLogNormalSamples = function(observationsVector, numSamples) {
	n = length(observationsVector)
	mu.hat = 0
	var.hat = 0
	
	for (i in 1:n) {
		mu.hat = mu.hat + log(observationsVector[i])	}
	mu.hat = mu.hat/n
	
	for (i in 1:n) {
		var.hat = var.hat + (log(observationsVector[i]) - mu.hat)^2
	}	
	var.hat = var.hat/n

	samples = rlnorm(numSamples, meanlog=mu.hat, sdlog=sqrt(var.hat))
	return(samples)
}


getEMSamples = function(observationsVector, numSamples, k) {
	emRun = getEMRandomInit(observationsVector, k)
	
	mixComponents = matrix(nrow=numSamples, ncol=1)
	samples = matrix(nrow=numSamples, ncol=1) 

	for(i in 1:numSamples) {
		mixComponents[i] = sample(x=seq(from=1, by=1, to=k), size=1, prob=emRun$pro)
	
		componentMean = emRun$parameters$mean[, mixComponents[i]]
		componentVariance = emRun$parameters$variance$Sigma
	
		irisSamples[i,] = mvrnorm(n=1, mu=componentMean, Sigma=componentVariance)
	}

}

getEMRandomInit = function(observationsVector, k) {
	library(mclust)
	
	rdmInitVtr = matrix(nrow=length(observationsVector), ncol=1)
	for(i in 1:nrow(rdmInitVtr)) {
		rdmInitVtr[i] = sample(x=seq(from=1, by=1, to=k), size=1, prob=rep(1/k, k))
	}

	rdmInitMtx = unmap(rdmInitVtr)
	
	# Hacky way to get em to work on vtr
	#observationsMtx = matrix(data=c(observationsVector, observationsVector), nrow=length(observationsVector), ncol=2)

	msEst.rdmInit = mstep(modelName="EEE", data=as.numeric(observationsVector), z=rdmInitMtx)
	emRun.rdmInit = em(modelName=msEst.rdmInit$modelName, data=observationsVector, parameters=msEst.rdmInit$parameters)
	#msEst.rdmInit = mstep(modelName="EEE", data=observationsMtx, z=rdmInitMtx)
	#emRun.rdmInit = em(modelName=msEst.rdmInit$modelName, data=observationsMtx, parameters=msEst.rdmInit$parameters)

	return(emRun.rdmInit)
}


# testing
#emRun = getEMRandomInit(op3$latency_ms, 3)
#observationsVector=op3$latency_ms
#k=3

#str(observationsVector)
#class(observationsVector)

#str(op3$latency_ms)
#length(op3$latency_ms)
#is.vector(op3$latency_ms)
#class(op3$latency_ms)

#dim(rdmInitMtx)
#str(rdmInitMtx)
#class(rdmInitMtx)

#a = vector(mode="numeric", length=5)
#a=c(1,2,3,4,5)
#is.vector(a)
#class(a)


