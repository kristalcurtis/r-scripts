rm(list=ls())

dataPath="/Users/radlab/Desktop/ksauer/Desktop/Research/MSReport/MSExperimentData"

load(paste(dataPath, "/ops.RData", sep=""))
ls()

load(paste(dataPath, "/queriesAndTheirOps.RData", sep=""))


par(mfrow=c(3,1))
xlim=c(0,300)
hist(op3$latency_ms, breaks=1000, xlab="Latency (ms)", ylab="Count", main="op3 Histogram (Benchmark)", xlim=xlim)
values=op3$latency_ms
abline(v=median(values), lw=2, col="red")
abline(v=quantile(values,0.9), lw=2, col="blue")
abline(v=quantile(values,0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(quantile(values,0.9), digits=2), "ms", sep=""), paste("99th=", round(quantile(values,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)

hist(nAops$latency_ms[nAops$opType==3], breaks=1000, xlab="Latency (ms)", ylab="Count", main="op3 Histogram (needsApproval)", xlim=xlim)
values=nAops$latency_ms[nAops$opType==3]
abline(v=median(values), lw=2, col="red")
abline(v=quantile(values,0.9), lw=2, col="blue")
abline(v=quantile(values,0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(quantile(values,0.9), digits=2), "ms", sep=""), paste("99th=", round(quantile(values,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)


hist(uBHops$latency_ms[uBHops$opType==3], breaks=1000, xlab="Latency (ms)", ylab="Count", main="op3 Histogram (userByHometown)", xlim=xlim)
values=uBHops$latency_ms[uBHops$opType==3]
abline(v=median(values), lw=2, col="red")
abline(v=quantile(values,0.9), lw=2, col="blue")
abline(v=quantile(values,0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(quantile(values,0.9), digits=2), "ms", sep=""), paste("99th=", round(quantile(values,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)


plotOpDistrFromBenchmarkAndQueries = function(dataPath, outputPath) {
	# load op data (from benchmarks) & query op data
	load(paste(dataPath, "/ops.RData", sep=""))
	load(paste(dataPath, "/queriesAndTheirOps.RData", sep=""))
	
	# for each op
	for (opNum in 1:9) {
		# figure out which queries involve this op
		opPresentInQuery = vector(length=4)
		
		opPresentInQuery[1] = (length(which(nAops$opType == opNum)) > 0)
		opPresentInQuery[2] = (length(which(uBHops$opType == opNum)) > 0)
		opPresentInQuery[3] = (length(which(mFops$opType == opNum)) > 0)
		opPresentInQuery[4] = (length(which(mTops$opType == opNum)) > 0)
	
		# set xlim
		xmax=0
		if (opPresentInQuery[1]) {
			xmax=max(xmax, nAops$latency_ms[nAops$opType == opNum])
		}
		if (opPresentInQuery[2]) {
			xmax=max(xmax, uBHops$latency_ms[uBHops$opType == opNum])
		}
		if (opPresentInQuery[3]) {
			xmax=max(xmax, mFops$latency_ms[mFops$opType == opNum])
		}
		if (opPresentInQuery[4]) {
			xmax=max(xmax, mTops$latency_ms[mTops$opType == opNum])
		}
	
		# determine # rows using "opPresentInQuery"
		numRows = length(which(opPresentInQuery)) + 1

		if (numRows > 0) {
		pdf(file=paste(outputPath, "/op", opNum, "-benchmarkVsQuery.pdf", sep=""))
		par(mfrow=c(numRows, 1), mar=c(5,5,4,2)+0.1)
		xlim=c(0,xmax)
	
		# print benchmark op (annotate with quantiles, create legend)
		if (opNum == 1) {
			op = op1
		} else if (opNum == 2) {
			op = op2
		} else if (opNum == 3) {
			op = op3
		} else if (opNum == 4) {
			op = op4
		} else if (opNum == 5) {
			op = op5
		} else if (opNum == 6) {
			op = op6
		} else if (opNum == 7) {
			op = op7
		} else if (opNum == 8) {
			op = op8
		} else if (opNum == 9) {
			op = op9
		}
		
		hist(op$latency_ms, breaks=1000, xlab="Latency (ms)", ylab="Count", main=paste("op", opNum, " Histogram (Benchmark)", sep=""), xlim=xlim)
		values=op$latency_ms
		abline(v=median(values), lw=2, col="red")
		abline(v=quantile(values,0.9), lw=2, col="blue")
		abline(v=quantile(values,0.99), lw=2, col="green")
		legend("topright", legend=c(paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(quantile(values,0.9), digits=2), "ms", sep=""), paste("99th=", round(quantile(values,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)
	
		# print op from each query that contains it (annotate with quantiles, create legend)
		if (opPresentInQuery[1]) {
			hist(nAops$latency_ms[nAops$opType==opNum], breaks=1000, xlab="Latency (ms)", ylab="Count", main=paste("op", opNum, " Histogram (needsApproval)", sep=""), xlim=xlim)
			values=nAops$latency_ms[nAops$opType==opNum]
			abline(v=median(values), lw=2, col="red")
			abline(v=quantile(values,0.9), lw=2, col="blue")
			abline(v=quantile(values,0.99), lw=2, col="green")
			legend("topright", legend=c(paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(quantile(values,0.9), digits=2), "ms", sep=""), paste("99th=", round(quantile(values,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)
		}		

		
		if (opPresentInQuery[2]) {
			hist(uBHops$latency_ms[uBHops$opType==opNum], breaks=1000, xlab="Latency (ms)", ylab="Count", main=paste("op", opNum, " Histogram (userByHometown)", sep=""), xlim=xlim)
			values=uBHops$latency_ms[uBHops$opType==opNum]
			abline(v=median(values), lw=2, col="red")
			abline(v=quantile(values,0.9), lw=2, col="blue")
			abline(v=quantile(values,0.99), lw=2, col="green")
			legend("topright", legend=c(paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(quantile(values,0.9), digits=2), "ms", sep=""), paste("99th=", round(quantile(values,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)
		}
		
		if (opPresentInQuery[3]) {
			hist(mFops$latency_ms[mFops$opType==opNum], breaks=1000, xlab="Latency (ms)", ylab="Count", main=paste("op", opNum, " Histogram (myFollowing)", sep=""), xlim=xlim)
			values=mFops$latency_ms[mFops$opType==opNum]
			abline(v=median(values), lw=2, col="red")
			abline(v=quantile(values,0.9), lw=2, col="blue")
			abline(v=quantile(values,0.99), lw=2, col="green")
			legend("topright", legend=c(paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(quantile(values,0.9), digits=2), "ms", sep=""), paste("99th=", round(quantile(values,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)
		}

		if (opPresentInQuery[4]) {
			hist(mTops$latency_ms[mTops$opType==opNum], breaks=1000, xlab="Latency (ms)", ylab="Count", main=paste("op", opNum, " Histogram (myThoughts)", sep=""), xlim=xlim)
			values=mTops$latency_ms[mTops$opType==opNum]
			abline(v=median(values), lw=2, col="red")
			abline(v=quantile(values,0.9), lw=2, col="blue")
			abline(v=quantile(values,0.99), lw=2, col="green")
			legend("topright", legend=c(paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(quantile(values,0.9), digits=2), "ms", sep=""), paste("99th=", round(quantile(values,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)
		}
		
		dev.off()
		}

	}
	
}


plotOpDistrFromBenchmarkAndQueries(dataPath, "~/Desktop/opPlotsBenchmarkVsQuery")