deinterleaveLogsMultiThreads = function(logMatrix) {
	startingThread=min(logMatrix$threadNum)
	endingThread=max(logMatrix$threadNum)

	print(startingThread)
	mtx.threadSorted = logMatrix[which(logMatrix$threadNum == startingThread),]

	for (i in (startingThread+1):endingThread) {
		print(i)
		currentThreadData = logMatrix[which(logMatrix$threadNum == i),]
		mtx.threadSorted = rbind(mtx.threadSorted, currentThreadData)
	}

	return(mtx.threadSorted)
}


getOpsForGivenQuery = function(ops, queryNum, queryType) {
	numOps = getNumOpsForQueryType(queryType)
	
	return(ops[((queryNum-1)*numOps+1):(queryNum*numOps),])
}


getNumOpsForQueryType = function(queryType) {
	if (queryType == "needsApproval") {
		numOps=8
	} else if (queryType == "userByHometown") {
		numOps=3
	} else if (queryType == "myFollowing") {
		numOps=8
	} else if (queryType == "myThoughts") {
		numOps=7
	} else {
		print("Unsupported query type.")
	}

	return(numOps)
}

getGapsBetweenOpsAndQueryLatency = function(ops, queries, queryType) {
	gaps = vector(length=nrow(queries))
	
	for (i in 1:nrow(queries)) {
		perQueryOps = getOpsForGivenQuery(ops, i, queryType)
		gaps[i] = queries$latency_ms[i] - sum(perQueryOps$latency_ms)
	}
	
	return(gaps)
}


getQueriesByLatencyRange = function(queries, minLatency, maxLatency) {
	return(queries[which(queries$latency_ms >= minLatency & queries$latency_ms <= maxLatency), ])
}


getWhichQueriesByLatencyRange = function(queries, minLatency, maxLatency) {
	return(which(queries$latency_ms >= minLatency & queries$latency_ms <= maxLatency))
}


getOpsForQueriesInLatencyRange = function(queries, ops, minLatency, maxLatency, queryType) {
	whichQueries = getWhichQueriesByLatencyRange(queries, minLatency, maxLatency)
	
	numOps = getNumOpsForQueryType(queryType)
	queryOps = matrix(nrow=length(whichQueries), ncol=numOps)

	# just to set up colnames
	currentOps = getOpsForGivenQuery(ops, 1, queryType)
	colnames(queryOps) = currentOps$opType
	
	for (i in 1:length(whichQueries)) {
		currentOps = getOpsForGivenQuery(ops, whichQueries[i], queryType)
		queryOps[i,] = currentOps$latency_ms
	}
	
	return(queryOps)
}

plotOpsForQueriesInLatencyRange = function(path, queries, ops, minLatency, maxLatency, queryType) {
	dir.create(path)

	queryOps = getOpsForQueriesInLatencyRange(queries, ops, minLatency, maxLatency, queryType)
	
	opNames = colnames(queryOps)
	
	for (i in sort(unique(opNames))) {
		pdf(file=paste(path, "/op", i, ".pdf", sep=""))
		par(mar=c(5,5,4,2)+0.1)
		par(mfrow=c(2,1))
		
		xmax=1.1*max(quantile(ops[which(ops$opType==i),"latency_ms"], 0.99), quantile(queryOps[,which(opNames==paste(i))], 0.99))
		
		hist(ops[which(ops$opType==i), "latency_ms"], breaks=500, xlab="Latency (ms)", ylab="Count", xlim=c(0, xmax), main=paste(queryType, " (all): op", i, sep=""))
		abline(v=quantile(ops[which(ops$opType==i), "latency_ms"], 0.9), lw=2, col="blue")
		legend("topright", legend="90th %ile", col="blue", lw=2)
		
		hist(queryOps[,which(opNames==paste(i))], breaks=50, xlab="Latency (ms)", ylab="Count", xlim=c(0,xmax), main=paste(queryType, " (", round(minLatency, digits=0), " ms, ", round(maxLatency, digits=0), " ms): op ", i, sep=""))
		
		dev.off()
	}
}



