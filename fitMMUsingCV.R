## 6.22.10
# Use cross validation to choose the proper # of mixture components for GMM 
# GMM is fitted to data via EM

computeGMMLogLikelihood = function(dataVector, parameters) {
	numMixtureComponents = length(parameters$pro)
	
	likelihoodPerPoint = vector(mode="numeric", length=length(dataVector))
	logLikelihoodPerPoint = vector(mode="numeric", length=length(dataVector))

	for (i in 1:length(dataVector)) {
		logLikelihoodPerPoint[i] = 0
		
		for (k in 1:numMixtureComponents) {
			componentMean = parameters$mean[k]

			if (length(parameters$variance$sigmasq) == 1) {
				componentVariance = parameters$variance$sigmasq
			} else {
				componentVariance = parameters$variance$sigmasq[k]
			}
				
			likelihoodPerPoint[i] = likelihoodPerPoint[i] + parameters$pro[k] * dnorm(dataVector[i], mean=componentMean, sd=sqrt(componentVariance))
		}
		
		if (likelihoodPerPoint[i] == 0) {
			logLikelihoodPerPoint[i] = NA
		} else {
			logLikelihoodPerPoint[i] = log(likelihoodPerPoint[i])
		}	
	}	
	
	#print(summary(likelihoodPerPoint))
	#print(summary(logLikelihoodPerPoint))
	
	logLikelihood = sum(logLikelihoodPerPoint, na.rm=TRUE)
	#logLikelihood = mean(logLikelihoodPerPoint)
	
	return(logLikelihood)
}



# Randomly split data into k folds
splitDataIntoKFolds = function(dataVector, k) {
	n = length(dataVector)
	
	randomOrdering=sample(x=seq(from=1,to=n,by=1), size=n, prob=rep(1/n,n))

	numPointsPerFold = floor(n/k)
		
	foldAssignmentVector = vector(mode="numeric", length=n)
	
	for (i in 1:n) {
		positionInRandomOrdering = which(randomOrdering == i)
		foldAssignmentVector[i] = ceiling(positionInRandomOrdering/numPointsPerFold)
	}
	
	# Allocate any remaining points if n is not evenly divisible by k
	extraPoints = which(foldAssignmentVector == k+1)
	numExtraPoints = length(extraPoints)
	if (numExtraPoints > 0) {
		for (i in 1:numExtraPoints) {
			foldAssignmentVector[extraPoints[i]] = i
		}
	}

	return(foldAssignmentVector)
}



fitGMMToTrainingDataAndReturnValidationLikelihood = function(trainingDataVector, validationDataVector, numMixtureComponents, numRdmInits=1, aggregationFunction="mean") {
	#source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")
	
	likelihoodValues = vector(mode="numeric", length=numRdmInits)
	
	for (i in 1:numRdmInits) {
		emParams = fitEM(observationsVector=trainingDataVector, k=numMixtureComponents, distrType="gaussian", modelName="V")
		likelihoodValues[i] = computeGMMLogLikelihood(dataVector=validationDataVector, parameters=emParams)
	}
	
	if (aggregationFunction == "mean") {
		return(mean(likelihoodValues))
	} else if (aggregationFunction == "median") {
		return(median(likelihoodValues))
	} else if (aggregationFunction == "max") {
		return(max(likelihoodValues))
	} else {
		print("Unsupported aggregation function.")
	}
}



findAvgLogLikelihoodViaCVFixedNumMixtureComponents = function(dataVector, numFolds=10, numMixtureComponents, numRdmInits=1, destinationPath="") {
	# Partition the data into folds
	partitionVector = splitDataIntoKFolds(dataVector=dataVector, k=numFolds)
	
	# Get log likelihood for each fold
	# i => validation set
	# all other folds => training set
	logLikelihood = vector(mode="numeric", length=numFolds)
	
	for (i in 1:numFolds) {
		print(paste("Using fold ", i, " for validation data", sep=""))
		validationData = dataVector[which(partitionVector == i)]
		trainingData = dataVector[which(partitionVector != i)]
		
		logLikelihood[i] = fitGMMToTrainingDataAndReturnValidationLikelihood(trainingDataVector=trainingData, validationDataVector=validationData, numMixtureComponents=numMixtureComponents, numRdmInits=numRdmInits, aggregationFunction="median")
	}
	
	if (destinationPath != "") {
		save(logLikelihood, file=paste(destinationPath, "/cvLogLikelihood-", numMixtureComponents, "mixtureComponents.RData", sep=""))
	}
	
	# Avg the log likelihood over all folds
	return(mean(logLikelihood))
}





kFoldCVToChooseNumMixtureComponentsForGMM = function(dataVector, numFolds=10, mixtureComponentsRange=seq(from=2,by=1,to=10), numRdmInits=1, destinationPath) {
	dir.create(destinationPath)

	avgLogLikelihood = vector(mode="numeric", length=length(mixtureComponentsRange))
	
	for (i in 1:length(mixtureComponentsRange)) {
		print(paste("# mixture components =", mixtureComponentsRange[i]))
		avgLogLikelihood[i] = findAvgLogLikelihoodViaCVFixedNumMixtureComponents(dataVector=dataVector, numFolds=numFolds, numMixtureComponents=mixtureComponentsRange[i], numRdmInits=numRdmInits, destinationPath=destinationPath)
	}
	
	return(mixtureComponentsRange[which.max(avgLogLikelihood)])
}




plotCVLogLikelihoodVsNumMixtureComponents = function(path, mixtureComponentsRange=seq(from=2,by=1,to=10)) {
	pdf(file=paste(path, "/cvLogLikelihoodVsNumMixtureComponents.pdf", sep=""))
	par(mar=c(5,5,4,2)+0.1,cex.axis=1.5, cex.lab=1.2, cex.main=1.5)
	
	# Computing stats for each run
	minPerNumMixtureComponents = vector(mode="numeric", length=length(mixtureComponentsRange))
	maxPerNumMixtureComponents = vector(mode="numeric", length=length(mixtureComponentsRange))
	avgPerNumMixtureComponents = vector(mode="numeric", length=length(mixtureComponentsRange))
	medianPerNumMixtureComponents = vector(mode="numeric", length=length(mixtureComponentsRange))
	
	for (i in 1:length(mixtureComponentsRange)) {
		load(paste(path, "/cvLogLikelihood-", mixtureComponentsRange[i], "mixtureComponents.RData", sep=""))  # => logLikelihood
		minPerNumMixtureComponents[i] = min(logLikelihood)
		maxPerNumMixtureComponents[i] = max(logLikelihood)
		avgPerNumMixtureComponents[i] = mean(logLikelihood)
		medianPerNumMixtureComponents[i] = median(logLikelihood)
	}
	
	# Assumes loglikelihood values are < 0	
	ymin = 1.05*min(minPerNumMixtureComponents)
	ymax = 0.95*max(maxPerNumMixtureComponents)

	# Assumes loglikelihood values are > 0	
	#ymin = 0.95*min(minPerNumMixtureComponents)
	#ymax = 1.05*max(maxPerNumMixtureComponents)

	
	plot(x=0,y=0,xlim=c(min(mixtureComponentsRange),max(mixtureComponentsRange)),ylim=c(ymin,ymax), col=0, xlab="# Mixture Components", ylab="Log Likelihood", main="Log Likelihood vs. # Mixture Components")
	
	for (i in 1:length(mixtureComponentsRange)) {
		load(paste(path, "/cvLogLikelihood-", mixtureComponentsRange[i], "mixtureComponents.RData", sep=""))  # => logLikelihood
		points(x=rep(mixtureComponentsRange[i], length(logLikelihood)), y=logLikelihood, col="blue", lw=2)
	}
	
	lines(x=mixtureComponentsRange, y=avgPerNumMixtureComponents, lw=2, col="red")
	lines(mixtureComponentsRange, medianPerNumMixtureComponents, lw=2, col="green")
	
	legend("bottomright", legend=c("log likelihood per fold", "average", "median"), col=c("blue", "red", "green"), pch=c(1,-1,-1), lty=c(0,1,1), lwd=c(2,2,2), cex=1.2)
	
	dev.off()
}



