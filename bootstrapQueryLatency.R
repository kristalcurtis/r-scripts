rm(list=ls())

# Get the bootstrapped results

# supporting functions
resampleOp = function(op) {
	return(sample(op$latency_ms, size=nrow(op), replace=TRUE, prob=rep.int(1/nrow(op), nrow(op))))
}

getRangeForQuantileValues = function(values) {
	#return(range(values))
	return(quantile(values, probs=c(0.025,0.975)))
}


# START
#queryType="needsApproval"
queryType="userByHometown"
#queryType="myFollowing"
#queryType="myThoughts"

ylim=80

whichPred = "empirical"
#whichPred="gmm"

numResampledSets=10

path=paste("~/Desktop/bootstrap-", whichPred, sep="")
dir.create(path)


# Bootstrap the actual

load("~/Desktop/queryObs.RData")
ls()

if (queryType == "needsApproval") {
	queries = nAqueries
} else if (queryType == "userByHometown") {
	queries = uBHqueries
} else if (queryType == "myFollowing") {
	queries = mFqueries
} else if (queryType == "myThoughts") {
	queries = mTqueries
}

numSamples = nrow(queries)

actualQuantiles = matrix(nrow=numResampledSets, ncol=3)

for (i in 1:numResampledSets) {
	samples = sample(queries$latency_ms, size=numSamples, replace=TRUE, prob=rep.int(1/numSamples, numSamples))
	actualQuantiles[i,] = quantile(samples, c(0.5,0.9,0.99))	
}


#hist(quantiles, breaks=25)
#abline(v=quantile(quantiles,0.025), lw=2, col="blue")
#abline(v=quantile(quantiles,0.975), lw=2, col="blue")
#summary(quantiles)


# Bootstrap the predictions too

if (whichPred == "empirical") {
	load("~/Desktop/ops.RData")
	ls()

	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/querySamplers.R")


	numSamples=1000
	#numResampledSets=100
	sampleQuantiles = matrix(nrow=numResampledSets, ncol=3)


	for (i in 1:numResampledSets) {
		print(i)

		op1ResampleHist = hist(resampleOp(op1), breaks=1000, plot=FALSE)
		op2ResampleHist = hist(resampleOp(op2), breaks=1000, plot=FALSE)
		op3ResampleHist = hist(resampleOp(op3), breaks=1000, plot=FALSE)
		op4ResampleHist = hist(resampleOp(op4), breaks=1000, plot=FALSE)
		op5ResampleHist = hist(resampleOp(op5), breaks=1000, plot=FALSE)
		op6ResampleHist = hist(resampleOp(op6), breaks=1000, plot=FALSE)
		op7ResampleHist = hist(resampleOp(op7), breaks=1000, plot=FALSE)
		op8ResampleHist = hist(resampleOp(op8), breaks=1000, plot=FALSE)
		op9ResampleHist = hist(resampleOp(op9), breaks=1000, plot=FALSE)

		if (queryType == "needsApproval") {
			samples = needsApprovalSampler(op1ResampleHist, op3ResampleHist, op4ResampleHist, op6ResampleHist, op9ResampleHist, numSamples)
		} else if (queryType == "userByHometown") {
			samples = userByHometownSampler(op2ResampleHist, op3ResampleHist, op6ResampleHist, numSamples)
		} else if (queryType == "myFollowing") {
			samples = myFollowingSampler(op1ResampleHist, op4ResampleHist, op5ResampleHist, op6ResampleHist, numSamples)
		} else if (queryType == "myThoughts") {
			samples = myThoughtsSampler(op1ResampleHist, op4ResampleHist, op6ResampleHist, op9ResampleHist, numSamples)
		}

		sampleQuantiles[i,] = quantile(samples, c(0.5,0.9,0.99))
	}
} else if (whichPred == "gmm") {
	load("~/Desktop/ops.RData")
	ls()

	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/querySamplers.R")
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/emFitAndSample.R")

	# setup
	numSamples=1000
	sampleQuantiles = matrix(nrow=numResampledSets, ncol=3)

	k=c(3,8,5,3,5,4,4,5,2) # from CV


	for (i in 1:numResampledSets) {
		print(i)

		# resample the ops
		op1Resample = resampleOp(op1)
		op2Resample = resampleOp(op2)
		op3Resample = resampleOp(op3)
		op4Resample = resampleOp(op4)
		op5Resample = resampleOp(op5)
		op6Resample = resampleOp(op6)
		op7Resample = resampleOp(op7)
		op8Resample = resampleOp(op8)
		op9Resample = resampleOp(op9)

		# fit gmm to resampled data
		opParamFile = "~/Desktop/opGMMParams.RData"

		if (queryType == "needsApproval") {
			# fit gmms to resampled data
			op1GMMParams = fitEM(observationsVector=op1Resample, k=k[1])
			op3GMMParams = fitEM(observationsVector=op3Resample, k=k[3])
			op4GMMParams = fitEM(observationsVector=op4Resample, k=k[4])
			op6GMMParams = fitEM(observationsVector=op6Resample, k=k[6])
			op9GMMParams = fitEM(observationsVector=op9Resample, k=k[9])
		
			save(op1GMMParams, op3GMMParams, op4GMMParams, op6GMMParams, op9GMMParams, file=opParamFile)
		
			samples = needsApprovalGMMSampler(opParamFile, numSamples)
		} else if (queryType == "userByHometown") {
			op2GMMParams = fitEM(observationsVector=op2Resample, k=k[2])
			op3GMMParams = fitEM(observationsVector=op3Resample, k=k[3])
			op6GMMParams = fitEM(observationsVector=op6Resample, k=k[6])

			save(op2GMMParams, op3GMMParams, op6GMMParams, file=opParamFile)
		
			samples = userByHometownGMMSampler(opParamFile, numSamples)
		} else if (queryType == "myFollowing") {
			op1GMMParams = fitEM(observationsVector=op1Resample, k=k[1])
			op4GMMParams = fitEM(observationsVector=op4Resample, k=k[4])
			op5GMMParams = fitEM(observationsVector=op5Resample, k=k[5])
			op6GMMParams = fitEM(observationsVector=op6Resample, k=k[6])
		
			save(op1GMMParams, op4GMMParams, op5GMMParams, op6GMMParams, file=opParamFile)
		
			samples = myFollowingGMMSampler(opParamFile, numSamples)
		} else if (queryType == "myThoughts") {
			op1GMMParams = fitEM(observationsVector=op1Resample, k=k[1])
			op4GMMParams = fitEM(observationsVector=op4Resample, k=k[4])
			op6GMMParams = fitEM(observationsVector=op6Resample, k=k[6])
			op9GMMParams = fitEM(observationsVector=op9Resample, k=k[9])
		
			save(op1GMMParams, op4GMMParams, op6GMMParams, op9GMMParams, file=opParamFile)
		
			samples = myThoughtsGMMSampler(opParamFile, numSamples)
		}

		sampleQuantiles[i,] = quantile(samples, c(0.5,0.9,0.99))
	}
} else {
	print("Unsupported prediction type.")
}





# Get results

source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/graphical-compare-actual-predicted-query-distr.R")



queriesRange = c(getRangeForQuantileValues(actualQuantiles[,1]), getRangeForQuantileValues(actualQuantiles[,2]), getRangeForQuantileValues(actualQuantiles[,3]))

samplesRange = c(getRangeForQuantileValues(sampleQuantiles[,1]), getRangeForQuantileValues(sampleQuantiles[,2]), getRangeForQuantileValues(sampleQuantiles[,3]))

queriesMedians = c(median(actualQuantiles[,1]), median(actualQuantiles[,2]), median(actualQuantiles[,3]))

samplesMedians = c(median(sampleQuantiles[,1]), median(sampleQuantiles[,2]), median(sampleQuantiles[,3]))

#plotActualAndPredictedQueryQuantileBarplotWithErrorBars(path, queryType, queries, samples, queriesRange, samplesRange, c(0,ylim))
plotActualAndPredictedQueryQuantileBarplotWithErrorBars(path, queryType, queries, samples, queriesRange, samplesRange, queriesMedians, samplesMedians, c(0,ylim))
