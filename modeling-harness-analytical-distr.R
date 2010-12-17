## 6.16.10
## Harness for testing prediction of query distributions 
## Useful so I can try a bunch of different fits for the operator distributions

computeAndComparePredictedQueryDistr = function(queryObsFile="~/Desktop/queryObs.RData", opDistrFile, destinationPath, queryType, numSamples=1000) {
	dir.create(destinationPath)

	# Load distr for query's ops
	load(opDistrFile)
	load(queryObsFile)

	# Use the op distr to create query distr
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/querySamplers.R")

	if (queryType == "needsApproval") {
		samples = needsApprovalSampler(opDistrFile, numSamples)
		queries = nAqueries
	} else if (queryType == "userByHometown") {
		samples = userByHometownSampler(opDistrFile, numSamples)
		queries = uBHqueries
	} else if (queryType == "userByHometown-GMM") {
		samples = userByHometownGMMSampler(opDistrFile, numSamples)
		queries = uBHqueries
	} else if (queryType == "myFollowing") {
		samples = myFollowingSampler(opDistrFile, numSamples)
		queries = mFqueries
	} else if (queryType == "myThoughts") {
		samples = myThoughtsSampler(opDistrFile, numSamples)
		queries = mTqueries
	} else {
		print("Incorrect value assigned to queryType.")
	}

	# Compare predicted, actual query distr
	source("/Users/radlab/Desktop/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/graphical-compare-actual-predicted-query-distr.R")

	plotActualAndPredictedQueryHists(destinationPath, queryType, queries, samples)
	plotActualAndPredictedQueryQuantileBarplot(destinationPath, queryType, queries, samples)
	plotActualAndPredictedQueryCDF(destinationPath, queryType, queries, samples)
}




## Past experimental params
#queryObsFile = "~/Desktop/queryObs.RData"	# Contains query observations for needsApproval, userByHometown, myFollowing, and myThoughts

#opDistrFile = "~/Desktop/opDistrEmpirical.RData"  # Contains op1distr, ..., op9distr (or some subset of those); these are hist
#destinationPath = "~/Desktop/opDistrEmpirical"  # Should already exist

#opDistrFile = "~/Desktop/uBH-with-op3Gamma/opDistrOp3Gamma.RData"
#destinationPath = "~/Desktop/uBH-with-op3Gamma"

#opDistrFile = "~/Desktop/opDistr-allOps-op3Gamma.RData"
#destinationPath = "~/Desktop/uBH-with-op3Gamma-massMM-tailReg"
