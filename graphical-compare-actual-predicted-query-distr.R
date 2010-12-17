plotActualAndPredictedQueryHists = function(destinationPath, queryType, queries, samples) {
	pdf(paste(destinationPath, "/", queryType, "-distr.pdf", sep=""))
	par(mfrow=c(2,1), mar=c(5,5,4,2)+0.1)
	xmax=max(queries$latency_ms, samples)
	latencyDistr = queries$latency_ms
	hist(latencyDistr, xlim=c(0,xmax), breaks=20, xlab="Latency (ms)", ylab="Count", main=paste(queryType, "Latency Distribution, Actual"))
	abline(v=median(latencyDistr), lw=2, col="red")
	abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
	abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
	legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
	paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
	paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)

	latencyDistr = samples
	hist(latencyDistr, xlim=c(0,xmax), breaks=50, xlab="Latency (ms)", ylab="Count", main=paste(queryType, "Latency Distribution, Predicted"))
	abline(v=median(latencyDistr), lw=2, col="red")
	abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
	abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
	legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
	paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
	paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)
	dev.off()
}


plotActualAndPredictedQueryQuantileBarplot = function(destinationPath, queryType, queries, samples) {
	pdf(paste(destinationPath, "/", queryType, "-barplot.pdf", sep=""))
	par(mar=c(5,5,4,2)+0.1)
	b = barplot(c(quantile(queries$latency_ms,0.5), quantile(samples,0.5), quantile(queries$latency_ms,0.9), quantile(samples,0.9), quantile(queries$latency_ms,0.99), quantile(samples,0.99)), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), xlab="Quantile", ylab="Latency (ms)", main=queryType, ylim=c(0,1.1*max(quantile(queries$latency_ms,0.99), quantile(samples, 0.99))))

	numDigits=2
	offset = 0.2*quantile(queries$latency_ms,0.5)
	text(b, c(offset+quantile(queries$latency_ms,0.5), offset+quantile(samples,0.5), offset+quantile(queries$latency_ms,0.9), offset+quantile(samples,0.9), offset+quantile(queries$latency_ms,0.99), offset+quantile(samples,0.99)), labels=c(round(quantile(queries$latency_ms,0.5), digits=numDigits), round(quantile(samples,0.5), digits=numDigits), round(quantile(queries$latency_ms,0.9),digits=numDigits), round(quantile(samples,0.9),digits=numDigits), round(quantile(queries$latency_ms,0.99),digits=numDigits), round(quantile(samples,0.99),digits=numDigits)))

	legend("topleft", c("Actual median", "Predicted median", "Actual 90th", "Predicted 90th", "Actual 99th", "Predicted 99th"), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), lwd=10)
	dev.off()
}


#superposeErrorBars = function(x, y, ebl, ebu = ebl, length = 0.08, ...) {
superposeErrorBars = function(x, lowerBound, upperBound, length = 0.08, ...) {
	arrows(x, upperBound, x, lowerBound, angle = 90, code = 3, length = length, ...)
}


superposeErrorBarsWithMedian = function(x, y, lowerBound, upperBound, length = 0.08, ...) {
	arrows(x, upperBound, x, y, angle = 90, code = 3, length = length, ...)

	arrows(x, y, x, lowerBound, angle = 90, code = 3, length = length, ...)
}


plotActualAndPredictedQueryQuantileBarplotWithErrorBars = function(destinationPath, queryType, queries, samples, queriesRange, samplesRange, queriesMedians, samplesMedians, ylim) {
	pdf(paste(destinationPath, "/", queryType, "-barplot.pdf", sep=""))
	par(mar=c(5,5,4,2)+0.1)
	#b = barplot(c(quantile(queries$latency_ms,0.5), quantile(samples,0.5), quantile(queries$latency_ms,0.9), quantile(samples,0.9), quantile(queries$latency_ms,0.99), quantile(samples,0.99)), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), xlab="Quantile", ylab="Latency (ms)", main=queryType, ylim=c(0,1.1*max(quantile(queries$latency_ms,0.99), quantile(samples, 0.99))))
	#b = barplot(c(quantile(queries$latency_ms,0.5), quantile(samples,0.5), quantile(queries$latency_ms,0.9), quantile(samples,0.9), quantile(queries$latency_ms,0.99), quantile(samples,0.99)), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), xlab="Quantile", ylab="Latency (ms)", main=queryType, ylim=c(0,1.1*max(queriesRange[6], samplesRange[6])))
	#b = barplot(c(quantile(queries$latency_ms,0.5), quantile(samples,0.5), quantile(queries$latency_ms,0.9), quantile(samples,0.9), quantile(queries$latency_ms,0.99), quantile(samples,0.99)), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), xlab="Quantile", ylab="Latency (ms)", main=queryType, ylim=ylim)
	b = barplot(c(quantile(queries$latency_ms,0.5), quantile(samples,0.5), quantile(queries$latency_ms,0.9), quantile(samples,0.9), quantile(queries$latency_ms,0.99), quantile(samples,0.99)), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), xlab="Quantile", ylab="Latency (ms)", main=queryType, ylim=c(0,1.1*max(queriesRange[6], samplesRange[6], ylim)))

	# Error bars
	#superposeErrorBars(b[1], queriesRange[1], queriesRange[2], lw=2)
	#superposeErrorBars(b[3], queriesRange[3], queriesRange[4], lw=2)
	#superposeErrorBars(b[5], queriesRange[5], queriesRange[6], lw=2)

	superposeErrorBarsWithMedian(b[1], queriesMedians[1], queriesRange[1], queriesRange[2], lw=2)
	superposeErrorBarsWithMedian(b[3], queriesMedians[2], queriesRange[3], queriesRange[4], lw=2)
	superposeErrorBarsWithMedian(b[5], queriesMedians[3], queriesRange[5], queriesRange[6], lw=2)


	#superposeErrorBars(b[2], samplesRange[1], samplesRange[2], lw=2)
	#superposeErrorBars(b[4], samplesRange[3], samplesRange[4], lw=2)
	#superposeErrorBars(b[6], samplesRange[5], samplesRange[6], lw=2)
	
	superposeErrorBarsWithMedian(b[2], samplesMedians[1], samplesRange[1], samplesRange[2], lw=2)
	superposeErrorBarsWithMedian(b[4], samplesMedians[2], samplesRange[3], samplesRange[4], lw=2)
	superposeErrorBarsWithMedian(b[6], samplesMedians[3], samplesRange[5], samplesRange[6], lw=2)
		

	legend("topleft", c("Actual median", "Predicted median", "Actual 90th", "Predicted 90th", "Actual 99th", "Predicted 99th"), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), lwd=10)
	dev.off()
}


plotActualAndPredictedQueryCDF = function(destinationPath, queryType, queries, samples) {
	pdf(paste(destinationPath, "/", queryType, "-cdf.pdf", sep=""))
	par(mar=c(5,5,4,2)+0.1)
	cdf = ecdf(queries$latency_ms)
	samplesCdf = ecdf(samples)
	cdfSeq = seq(min(queries$latency_ms), max(queries$latency_ms))
	plot(cdfSeq, cdf(cdfSeq), log="x", col=0, xlab="Latency (ms)", ylab="Quantile", main=queryType)
	lines(cdfSeq, cdf(cdfSeq), col="red", lw=2, log="x")
	lines(cdfSeq, samplesCdf(cdfSeq), col="blue", lw=2, log="x")
	legend("bottomright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lwd=2, cex=1.2)
	dev.off()
}


# predictionsMatrix:  rows => resample-based quantile estimates, cols => results derived via diff estimation methods
# colnames of predictionsMatrix => estimation methods
plotActualAndPredictedBootstrapBoxAndWhisker = function(predictionsMatrix, predictedQuantile, queryType, path) {
	dir.create(path)
	pdf(paste(path, "/", queryType, "-bootstrap.pdf", sep=""))

	par(mar=c(5,5,4,2)+0.1)
	
	b=boxplot(predictionsMatrix,ylim=c(0,max(predictionsMatrix)), names=colnames(predictionsMatrix), boxwex=0.5, col="turquoise", xlab="Estimation Method", ylab="Latency (ms)", main=paste(queryType, ": bootstrap estimates of ", predictedQuantile*100, "th %ile latency", sep=""))

	# mark median prediction from each estimation method
	for (i in 1:ncol(predictionsMatrix)) {
		text(0.6+(i-1),b$stats[3,i], round(b$stats[3,i], digits=1))
	}

	legend("bottomright", legend="median", lw=3)
	dev.off()
}


