# Get predicted op density, using mixture models

getPredictedOpDensityUsingSingleDistr = function(opData, distrType, densityDomain) {
	library(MASS)
	source("emForGeneralMixtureModel.R")

	fit = fitdistr(opData, distrType)
	
	if (distrType == "normal") {
		params=list(mixingProportions=c(1), mean=fit$estimate["mean"], sd=fit$estimate["sd"])	} else if (distrType == "exponential") {
		params=list(mixingProportions=c(1), rate=fit$estimate["rate"])
	} else if (distrType == "gamma") {
		params=list(mixingProportions=c(1), shape=fit$estimate["shape"], rate=fit$estimate["rate"])
	} else if (distrType == "weibull") {
		params=list(mixingProportions=c(1), shape=fit$estimate["shape"], scale=fit$estimate["scale"])
	} else {
		print("Unsupported distr type.")
	}
	
	return(dMixtureModel(densityDomain, distrType, params))
}


getPredictedOpDensityUsingMM = function(opData, numMixtureComponents, distrType, initType, densityDomain) {
	source("emForGeneralMixtureModel.R")
	
	emRun = em(opData, numMixtureComponents, distrType, initType)
	
	return(dMixtureModel(densityDomain, distrType, emRun$params))
}





