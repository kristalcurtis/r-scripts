# compare sum(op 99th %iles) to query 99th %ile - how off is it?
rm(list=ls())

dataPath = "~/Desktop"

load(paste(dataPath, "/queriesAndTheirOps.RData", sep=""))
ls()

quantile=0.99

nAQueryQuantile = quantile(nAqueries$latency_ms, quantile)
uBHQueryQuantile = quantile(uBHqueries$latency_ms, quantile)
mFQueryQuantile = quantile(mFqueries$latency_ms, quantile)
mTQueryQuantile = quantile(mTqueries$latency_ms, quantile)




getSumOfQueryOpsQuantiles = function(queryType, opFilename, quantile) {
	load(opFilename)

	if (queryType == "needsApproval") {
		op1Quantile = quantile(op1$latency_ms, quantile)
		op3Quantile = quantile(op3$latency_ms, quantile)
		op4Quantile = quantile(op4$latency_ms, quantile)
		op6Quantile = quantile(op6$latency_ms, quantile)
		op9Quantile = quantile(op9$latency_ms, quantile)
		
		sumOfOpQuantiles = sum(op1Quantile*2, op3Quantile, op4Quantile, op6Quantile*3, op9Quantile)
	} else if (queryType == "userByHometown") {
		op2Quantile = quantile(op2$latency_ms, quantile)
		op3Quantile = quantile(op3$latency_ms, quantile)
		op6Quantile = quantile(op6$latency_ms, quantile)

		sumOfOpQuantiles = sum(op2Quantile, op3Quantile, op6Quantile)
	} else if (queryType == "myFollowing") {
		op1Quantile = quantile(op1$latency_ms, quantile)
		op4Quantile = quantile(op4$latency_ms, quantile)
		op5Quantile = quantile(op5$latency_ms, quantile)
		op6Quantile = quantile(op6$latency_ms, quantile)

		sumOfOpQuantiles = sum(op1Quantile*2, op4Quantile, op5Quantile, op6Quantile*4)
	} else if (queryType == "myThoughts") {
		op1Quantile = quantile(op1$latency_ms, quantile)
		op4Quantile = quantile(op4$latency_ms, quantile)
		op6Quantile = quantile(op6$latency_ms, quantile)
		op9Quantile = quantile(op9$latency_ms, quantile)

		sumOfOpQuantiles = sum(op1Quantile*2, op4Quantile, op6Quantile*3, op9Quantile)
	} else {
		print("Unsupported query type.")
	}
	
	return(sumOfOpQuantiles)
}


getSumOfQueryOpsMeans = function(queryType, opFilename, quantile) {
	load(opFilename)

	if (queryType == "needsApproval") {
		op1Mean = mean(op1$latency_ms)
		op3Mean = mean(op3$latency_ms)
		op4Mean = mean(op4$latency_ms)
		op6Mean = mean(op6$latency_ms)
		op9Mean = mean(op9$latency_ms)
		
		sumOfOpMeans = sum(op1Mean*2, op3Mean, op4Mean, op6Mean*3, op9Mean)
	} else if (queryType == "userByHometown") {
		op2Mean = mean(op2$latency_ms)
		op3Mean = mean(op3$latency_ms)
		op6Mean = mean(op6$latency_ms)

		sumOfOpMeans = sum(op2Mean, op3Mean, op6Mean)
	} else if (queryType == "myFollowing") {
		op1Mean = mean(op1$latency_ms)
		op4Mean = mean(op4$latency_ms)
		op5Mean = mean(op5$latency_ms)
		op6Mean = mean(op6$latency_ms)

		sumOfOpMeans = sum(op1Mean*2, op4Mean, op5Mean, op6Mean*4)
	} else if (queryType == "myThoughts") {
		op1Mean = mean(op1$latency_ms)
		op4Mean = mean(op4$latency_ms)
		op6Mean = mean(op6$latency_ms)
		op9Mean = mean(op9$latency_ms)

		sumOfOpMeans = sum(op1Mean*2, op4Mean, op6Mean*3, op9Mean)
	} else {
		print("Unsupported query type.")
	}
	
	return(sumOfOpMeans)
}



nASumOfOpsQuantile = getSumOfQueryOpsQuantiles("needsApproval", nAops, 0.99)
uBHSumOfOpsQuantile = getSumOfQueryOpsQuantiles("userByHometown", uBHops, 0.99)
mFSumOfOpsQuantile = getSumOfQueryOpsQuantiles("myFollowing", mFops, 0.99)
mTSumOfOpsQuantile = getSumOfQueryOpsQuantiles("myThoughts", mTops, 0.99)

# compare sum of ops quantiles, actual query quantiles
actualVsSumOfOpsQuantiles.99th = matrix(c(c(nAQueryQuantile, uBHQueryQuantile, mFQueryQuantile, mTQueryQuantile), c(nASumOfOpsQuantile, uBHSumOfOpsQuantile, mFSumOfOpsQuantile, mTSumOfOpsQuantile)), nrow=4, ncol=2)
colnames(actualVsSumOfOpsQuantiles.99th) = c("actual", "sumOfOps")
rownames(actualVsSumOfOpsQuantiles.99th) = c("needsApproval", "userByHometown", "myFollowing", "myThoughts")
actualVsSumOfOpsQuantiles.99th

# baseline:  can be off by up to a factor of 2


compareActualToSumOfOpsQuantiles = function(alignedQueriesOpsFilename, opsFilenames, quantile) {
	load(alignedQueriesOpsFilename)
	
	nAQueryQuantile = quantile(nAqueries$latency_ms, quantile)
	uBHQueryQuantile = quantile(uBHqueries$latency_ms, quantile)
	mFQueryQuantile = quantile(mFqueries$latency_ms, quantile)
	mTQueryQuantile = quantile(mTqueries$latency_ms, quantile)

	nASumOfOpsQuantile = getSumOfQueryOpsQuantiles("needsApproval", opsFilenames[1], quantile)
	uBHSumOfOpsQuantile = getSumOfQueryOpsQuantiles("userByHometown", opsFilenames[2], quantile)
	mFSumOfOpsQuantile = getSumOfQueryOpsQuantiles("myFollowing", opsFilenames[3], quantile)
	mTSumOfOpsQuantile = getSumOfQueryOpsQuantiles("myThoughts", opsFilenames[4], quantile)

	actualVsSumOfOpsQuantiles = matrix(c(c(nAQueryQuantile, uBHQueryQuantile, mFQueryQuantile, mTQueryQuantile), c(nASumOfOpsQuantile, uBHSumOfOpsQuantile, mFSumOfOpsQuantile, mTSumOfOpsQuantile)), nrow=4, ncol=2)
	colnames(actualVsSumOfOpsQuantiles) = c("actual", "sumOfOps")
	rownames(actualVsSumOfOpsQuantiles) = c("needsApproval", "userByHometown", "myFollowing", "myThoughts")
	return(actualVsSumOfOpsQuantiles)
}


compareActualToSumOfOpsMeans = function(alignedQueriesOpsFilename, opsFilenames) {
	load(alignedQueriesOpsFilename)

	nAQueryMean = mean(nAqueries$latency_ms)
	uBHQueryMean = mean(uBHqueries$latency_ms)
	mFQueryMean = mean(mFqueries$latency_ms)
	mTQueryMean = mean(mTqueries$latency_ms)

	nASumOfOpsMean = getSumOfQueryOpsMeans("needsApproval", opsFilenames[1])
	uBHSumOfOpsMean = getSumOfQueryOpsMeans("userByHometown", opsFilenames[2])
	mFSumOfOpsMean = getSumOfQueryOpsMeans("myFollowing", opsFilenames[3])
	mTSumOfOpsMean = getSumOfQueryOpsMeans("myThoughts", opsFilenames[4])
	
	actualVsSumOfOpsMeans = matrix(c(c(nAQueryMean, uBHQueryMean, mFQueryMean, mTQueryMean), c(nASumOfOpsMean, uBHSumOfOpsMean, mFSumOfOpsMean, mTSumOfOpsMean)), nrow=4, ncol=2)
	colnames(actualVsSumOfOpsMeans) = c("actual", "sumOfOps")
	rownames(actualVsSumOfOpsMeans) = c("needsApproval", "userByHometown", "myFollowing", "myThoughts")
	return(actualVsSumOfOpsMeans)
}



actualVsSumOfOpsQuantiles.50 = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), 0.5)
actualVsSumOfOpsQuantiles.90 = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), 0.9)
actualVsSumOfOpsQuantiles.95 = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), 0.95)
actualVsSumOfOpsQuantiles.99 = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), 0.99)


# change to load in ops from file so can try with benchmarked ops too
# nA
op1 = nAops[nAops$opType == 1,]
op3 = nAops[nAops$opType == 3,]
op4 = nAops[nAops$opType == 4,]
op6 = nAops[nAops$opType == 6,]
op9 = nAops[nAops$opType == 9,]
save(op1, op3, op4, op6, op9, file=paste(dataPath, "/nAops.RData", sep=""))

# uBH
op2 = uBHops[uBHops$opType == 2,]
op3 = uBHops[uBHops$opType == 3,]
op6 = uBHops[uBHops$opType == 6,]
save(op2, op3, op6, file=paste(dataPath, "/uBHops.RData", sep=""))

# mF
op1 = mFops[mFops$opType == 1,]
op4 = mFops[mFops$opType == 4,]
op5 = mFops[mFops$opType == 5,]
op6 = mFops[mFops$opType == 6,]
save(op1, op4, op5, op6, file=paste(dataPath, "/mFops.RData", sep=""))

# mT
op1 = mTops[mTops$opType == 1,]
op4 = mTops[mTops$opType == 4,]
op6 = mTops[mTops$opType == 6,]
op9 = mTops[mTops$opType == 9,]
save(op1, op4, op6, op9, file=paste(dataPath, "/mTops.RData", sep=""))

opsFilenames = c(paste(dataPath, "/nAops.RData", sep=""), paste(dataPath, "/uBHops.RData", sep=""), paste(dataPath, "/mFops.RData", sep=""), paste(dataPath, "/mTops.RData", sep=""))

actualVsSumOfOpsQuantiles.50.v2 = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames, 0.5)
actualVsSumOfOpsQuantiles.90.v2 = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames, 0.9)
actualVsSumOfOpsQuantiles.95.v2 = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames,0.95)
actualVsSumOfOpsQuantiles.99.v2 = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames, 0.99)


# with benchmarked ops
opsFilenames = rep(paste(dataPath, "/ops.RData", sep=""), 4)

actualVsSumOfOpsQuantiles.50.benchmarked = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames, 0.5)
actualVsSumOfOpsQuantiles.90.benchmarked = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames, 0.9)
actualVsSumOfOpsQuantiles.95.benchmarked = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames,0.95)
actualVsSumOfOpsQuantiles.99.benchmarked = compareActualToSumOfOpsQuantiles(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames, 0.99)

# mean
opsFilenames = c(paste(dataPath, "/nAops.RData", sep=""), paste(dataPath, "/uBHops.RData", sep=""), paste(dataPath, "/mFops.RData", sep=""), paste(dataPath, "/mTops.RData", sep=""))
actualVsSumOfOpsMeans = compareActualToSumOfOpsMeans(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames)

opsFilenames = rep(paste(dataPath, "/ops.RData", sep=""), 4)
actualVsSumOfOpsMeans.benchmarked = compareActualToSumOfOpsMeans(paste(dataPath, "/queriesAndTheirOps.RData", sep=""), opsFilenames)


save(actualVsSumOfOpsQuantiles.50, actualVsSumOfOpsQuantiles.90, actualVsSumOfOpsQuantiles.95, actualVsSumOfOpsQuantiles.99, actualVsSumOfOpsQuantiles.50.benchmarked, actualVsSumOfOpsQuantiles.90.benchmarked, actualVsSumOfOpsQuantiles.95.benchmarked, actualVsSumOfOpsQuantiles.99.benchmarked, actualVsSumOfOpsMeans, actualVsSumOfOpsMeans.benchmarked, file=paste(dataPath, "/actualVsSumOfOpMeansOrQuantiles.RData", sep=""))

