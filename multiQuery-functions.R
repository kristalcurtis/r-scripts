annotatePlotWithQuantiles = function(values) {
	mean = mean(values)
	median = median(values)
	q90th = quantile(values, 0.9)
	q99th = quantile(values, 0.99)

	abline(v=mean, lw=2, col="purple")
	abline(v=median, lw=2, col="red")
	abline(v=q90th, lw=2, col="blue")
	abline(v=q99th, lw=2, col="green")
	legend("topright", legend=c(paste("mean=", round(mean, digits=2), "ms", sep=""), paste("median=", round(median, digits=2), "ms", sep=""), paste("90th=", round(q90th, digits=2), "ms", sep=""), paste("99th=", round(q99th, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)
}


## Automate the processing done in multiQuery.R
## Given a list of directories (one directory => one query mix (wrt queries)), produce a plot for each of the two queries, where each query's plot file has |directories list| plots, one for its latency distribution under each mix.

plotQueryMixLatencyForEachQueryUnderEachMix = function(baseDir, relativeDirList, queries) {
	setwd(baseDir)
	
	# preload the data
	queryMixList = loadQueryMixData(relativeDirList)
	#xmax = getXmax(queryMixList)

	# find which query types are available
	queryTypes = getQueryTypes(queryMixList)

	# make plots
	# for each query
	for (queryType in queryTypes) {
		print(queryType)
		pdf(file=paste(baseDir, "/", queries[queryType], ".pdf", sep=""))
		
		par(mfrow=c(length(relativeDirList)-1,1))
		
		# for each mix
		for (mix in 1:length(relativeDirList)) {
			print(mix)
			if (length(getQueryLatencyForOneQueryTypeUnderOneMix(queryMixList, queryType, mix)) > 1) {
				plotQueryLatencyForOneQueryTypeUnderOneMix(queryMixList, queryType, mix, makePlotTitle(queries[queryType], relativeDirList[mix])) 
			}
		}
		
		dev.off()
	}
}

loadQueryMixData = function(directoriesList) {
	queryLatencyPerMix = replicate(length(directoriesList), list())
	queryTypePerMix = replicate(length(directoriesList), list())

	# loading
	for (i in 1:length(directoriesList)) {
		queryData = as.data.frame(read.csv(paste(directoriesList[i], "/queries.csv", sep="")))
		
		queryLatencyPerMix[[i]] = queryData$latency_ms
		queryTypePerMix[[i]] = queryData$opType
	}
	
	queryMixList = list(latency=queryLatencyPerMix, queryType=queryTypePerMix)
	
	return(queryMixList)
}


getXmax = function(queryMixList, queryType) {
	max = 0
	
	for (i in 1:length(queryMixList$latency)) {
		possibleNewMax = max(getQueryLatencyForOneQueryTypeUnderOneMix(queryMixList, queryType, i), na.rm=TRUE)
		if (possibleNewMax > max) {
			max = possibleNewMax
		}
	}
	
	return(max)
}

getQueryTypes = function(queryMixList) {
	for (i in 1:length(queryMixList$queryType)) {
		if (length(unique(queryMixList$queryType[[i]])) == 2) {
			queryTypes = sort(unique(queryMixList$queryType[[i]]))
		}
	}
	
	return(queryTypes)
}

plotQueryLatencyForOneQueryTypeUnderOneMix = function(queryMixList, queryType, mix, plotTitle) {
	values = getQueryLatencyForOneQueryTypeUnderOneMix(queryMixList, queryType, mix)
	hist(values, breaks=100, xlab="Latency (ms)", ylab="Count", main=plotTitle, xlim=c(0,getXmax(queryMixList, queryType)))
	annotatePlotWithQuantiles(values)
}

makePlotTitle = function(query, directory) {
	return(paste(query, " (", directory, ")", sep=""))
}

getQueryLatencyForOneQueryTypeUnderOneMix = function(queryMixList, queryType, mix) {
	values = queryMixList$latency[[mix]][which(queryMixList$queryType[[mix]] == queryType)]
	if (length(values) > 0) {
		return(values)
	} else {
		return(NA)
	}
}


