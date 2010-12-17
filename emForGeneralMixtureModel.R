# Given params, returns weights
# uses dnorm, dexp, dgamma, ...
eStep = function(data, params, distrType) {
	numMixtureComponents = length(params$mixingProportions)
	numPts = length(data)
	
	# compute denom: each pt's weight = weighted sum of the pt's density under each of the mixture components
	perPointWeightedDensity = vector(mode="numeric", length=numPts)
	pointDensities = matrix(nrow=numPts, ncol=numMixtureComponents)
	
	for (m in 1:numMixtureComponents) {
		pointDensities[,m] = getPointDensity(data, params, m, distrType)
	}
	
	perPointWeightedDensity = apply(pointDensities, 1, getWeightedSum, params)
	
	# compute weights
	weights = matrix(nrow=numPts, ncol=numMixtureComponents)
	
	for (m in 1:numMixtureComponents) {
		weights[,m] = (params$mixingProportions[m] * getPointDensity(data, params, m, distrType)) / perPointWeightedDensity
	}
	
	return(weights)
}

getWeightedSum = function(data, params) {
	return(sum(data*params$mixingProportions))
}


# density of a point for a given mixture component
# m => which mixture component
getPointDensity = function(point, params, m, distrType) {
	if (distrType == "normal") {
		pointDensity = dnorm(point, mean=params$mean[m], sd=params$sd[m])
	} else if (distrType == "exponential") {
		pointDensity = dexp(point, rate=params$rate[m])
	} else if (distrType == "gamma") {
		pointDensity = dgamma(point, shape=params$shape[m], rate=params$rate[m])
	} else if (distrType == "weibull") {
		pointDensity = dweibull(point, shape=params$shape[m], scale=params$scale[m])
	} else {
		print("Unsupported distr type.")
	}

	return(pointDensity)
}



# Given weights, returns params
# uses fitdistr
# "params" rtn value should be easily compatible with dnorm, dexp, ...
mStep = function(data, weights, distrType, multiplier) {
	library(MASS)
	
	numMixtureComponents = ncol(weights)
	numPts = length(data)
	
	mixingProportions = vector(length=numMixtureComponents)
	
	# allocate space for per-mixture-component params
	if (distrType == "normal") {
		mean = vector(length=numMixtureComponents)
		sd = vector(length=numMixtureComponents)
	} else if (distrType == "exponential") {
		rate = vector(length=numMixtureComponents)
	} else if (distrType == "gamma") {
		shape = vector(length=numMixtureComponents)
		rate = vector(length=numMixtureComponents)
	} else if (distrType == "weibull") {
		shape = vector(length=numMixtureComponents)
		scale = vector(length=numMixtureComponents)
	} else {
		print("Unsupported distr type.")
	}
	
	for (m in 1:numMixtureComponents) {
		print(paste("Estimating params for mixture ", m, sep=""))
		
		# compute mixing proportions
		mixingProportions[m] = 1/numPts * sum(weights[,m])
		
		# compute distr params using weighted data
		weightedData = weightData(data, weights[,m], multiplier)
		
		if (distrType == "normal") {
			fit = fitdistr(weightedData, "normal")
			mean[m] = fit$estimate["mean"]
			sd[m] = fit$estimate["sd"]
		} else if (distrType == "exponential") {
			fit = fitdistr(weightedData, "exponential")
			rate[m] = fit$estimate["rate"]
		} else if (distrType == "gamma") {
			fit = fitdistr(weightedData, "gamma")
			shape[m] = fit$estimate["shape"]
			rate[m] = fit$estimate["rate"]
		} else if (distrType == "weibull") {
			fit = fitdistr(weightedData, "weibull")
			shape[m] = fit$estimate["shape"]
			scale[m] = fit$estimate["scale"]
		} else {
			print("Unsupported distr type.")
		}
	}
	
	# assemble params
	if (distrType == "normal") {
		params = list(mixingProportions=mixingProportions, mean=mean, sd=sd)
	} else if (distrType == "exponential") {
		params = list(mixingProportions=mixingProportions, rate=rate)
	} else if (distrType == "gamma") {
		params = list(mixingProportions=mixingProportions, shape=shape, rate=rate)
	} else if (distrType == "weibull") {
		params = list(mixingProportions=mixingProportions, shape=shape, scale=scale)
	} else {
		print("Unsupported distr type.")
	}
	
	return(params)	
}


# Given data & weights, return weighted dataset (ie, heavily-weighted data pts will appear more frequently than lightly-weighted points)
# This is done for each mixture component
# multiplier => how many times data will appear in weighted dataset
weightData = function(data, weights, multiplier=100) {
	dataAndWeights = matrix(c(data, weights), nrow=length(data), ncol=2)
	
	points = apply(dataAndWeights, 1, repeatPoint, multiplier)
	return(unlist(points))
}


repeatPoint = function(pointAndWeight, multiplier) {
	rep(pointAndWeight[1], floor(pointAndWeight[2]*multiplier))
}


# initialize by randomly assigning each point a value in {1,...,k}
# returns NxM matrix, where each row sum = 1
# N = num data pts, M = num mixture components
initEM = function(data, numMixtureComponents, initType="random", splitQuantile=0.9) {
	numPts = length(data)

	initMtx = matrix(data=0, nrow=numPts, ncol=numMixtureComponents)

	if (initType == "random") {
		for (i in 1:numPts) {
			currentMixture = sample(seq(from=1, by=1, to=numMixtureComponents), size=1, prob=rep(1/numMixtureComponents, numMixtureComponents))
			initMtx[i,currentMixture] = 1
		}
	} else if (initType == "evenlySpaced") {
		initMtx = initEvenlySpaced(data, numMixtureComponents)
	} else if (initType == "tailFocused") {
		data = sort(data)
		diffs = quantile(data, splitQuantile) - data
		splitPoint = which.min(abs(diffs))
		
		initMtx[1:splitPoint,1] = 1
		initMtx[(splitPoint+1):(length(data)), 2:numMixtureComponents] = initEvenlySpaced(data[(splitPoint+1):(length(data))], numMixtureComponents-1)
	} else if (initType == "evenlySpacedByQuantile") {
		initMtx = initEvenlySpacedByQuantile(data, numMixtureComponents)
	} else {
		print("Unsupported init type.")
	}
	
	return(initMtx)
}


initEvenlySpaced = function(data, numMixtureComponents) {
	binEndpoints = seq(from=0, to=max(data), by=max(data)/numMixtureComponents)
	return(binInit(data, numMixtureComponents, binEndpoints))
}

initEvenlySpacedByQuantile = function(data, numMixtureComponents) {
	binEndpoints = quantile(data, seq(from=0, to=1, by=1/numMixtureComponents))
	return(binInit(data, numMixtureComponents, binEndpoints))
}

binInit = function(data, numMixtureComponents, binEndpoints) {
	initMtx = matrix(data=0, nrow=length(data), ncol=numMixtureComponents)
	
	for (n in 1:length(data)) {
		binEndpointDiffs = data[n] - binEndpoints
		for (m in 1:numMixtureComponents) {
			if (binEndpointDiffs[m] > 0 & binEndpointDiffs[m+1] <= 0) {
				mixture=m
			}
		}
		
		initMtx[n,mixture] = 1
	}
	
	return(initMtx)
}



# loglik computations
# model after "computeGMMLogLikelihood" in "fitEMUsingCV.R"
computeLoglik = function(data, params, distrType) {
	numMixtureComponents = length(params$mixingProportions)
	numPts = length(data)
	
	likelihoodPerPoint = vector(mode="numeric", length=numPts)
	loglikPerPoint = vector(mode="numeric", length=numPts)
	
	for (n in 1:numPts) {
		loglikPerPoint[n] = 0
		
		for (m in 1:numMixtureComponents) {
			pointDensity = getPointDensity(data[n], params, m, distrType)
			likelihoodPerPoint[n] = likelihoodPerPoint[n] + params$mixingProportions[m] * pointDensity
		}
		
		if (likelihoodPerPoint[n] == 0) {
			loglikPerPoint[n] = NA
		} else {
			loglikPerPoint[n] = log(likelihoodPerPoint[n])
		}
	}
	
	loglik = sum(loglikPerPoint, na.rm=TRUE)
	
	return(loglik)
}



# calls eStep & mStep till convergence
# uses computeLogLikelihood to determine whether or not convergence has occurred
em = function(data, numMixtureComponents, distrType, initType, initSplitQuantile=0.9, multiplier=10) {
	# initialize
	print("Initializing...")
	weights = initEM(data, numMixtureComponents, initType, initSplitQuantile)

	prevLoglik=-1
	currentLoglik=0
		
	i=1
	# stop if hasn't converged after 20 iterations
	while(!hasConverged(prevLoglik, currentLoglik) & i<20) {
		prevLoglik=currentLoglik
		
		print(paste("M step ", i, " ...", sep=""))
		params = mStep(data, weights, distrType, multiplier)
		
		print(paste("E step ", i, " ...", sep=""))
		weights = eStep(data, params, distrType)
		
		currentLoglik = computeLoglik(data, params, distrType)
		print(paste("Loglik at ", i, "th step=", currentLoglik, sep=""))
		
		i = i+1
	}
	
	return(list(params=params, loglik=currentLoglik))
}


hasConverged = function(prevLoglik, currentLoglik) {
	return(round(prevLoglik, digits=0) == round(currentLoglik, digits=0))
}


sampleFromMixtureModel = function(distrType, params, numSamples) {
	numMixtureComponents=length(params$mixingProportions)
	samples = vector(length=numSamples)
	
	for (i in 1:numSamples) {
		# choose the mixture component
		mixtureComponent = sample(seq(from=1, by=1, to=numMixtureComponents), size=1, prob=rep(1/numMixtureComponents, numMixtureComponents))

		# sample from that mixture component's distr
		if (distrType == "normal") {
			samples[i] = rnorm(1, mean=params$mean[mixtureComponent], sd=params$sd[mixtureComponent])
		} else if (distrType == "exponential") {
			samples[i] = rexp(1, rate=params$rate[mixtureComponent])
		} else if (distrType == "gamma") {
			samples[i] = rgamma(1, shape=params$shape[mixtureComponent], rate=params$rate[mixtureComponent])
		} else if (distrType == "weibull") {
			samples[i] = rweibull(1, shape=params$shape[mixtureComponent], scale=params$scale[mixtureComponent])
		} else {
			print("Unsupported distr type.")
		}
	}
	
	return(samples)
}


# like dnorm, dexp, etc., but for general mixture models
dMixtureModel = function(x, distrType, params) {
	numMixtureComponents = length(params$mixingProportions)

	density = matrix(data=0, nrow=length(x), ncol=1)

	if (distrType == "normal") {
		for (m in 1:numMixtureComponents) {
			density = density + params$mixingProportions[m] * dnorm(x, mean=params$mean[m], sd=params$sd[m])
		}
	} else if (distrType == "exponential") {
		for (m in 1:numMixtureComponents) {
			density = density + params$mixingProportions[m] * dexp(x, rate=params$rate[m])
		}
	} else if (distrType == "gamma") {
		for (m in 1:numMixtureComponents) {
			density = density + params$mixingProportions[m] * dgamma(x, shape=params$shape[m], rate=params$rate[m])
		}		
	} else if (distrType == "weibull") {
		for (m in 1:numMixtureComponents) {
			density = density + params$mixingProportions[m] * dweibull(x, shape=params$shape[m], scale=params$scale[m])
		}
	} else {
		print("Unsupported distr type.")
	}
	
	return(list(x=x, y=density))
}
