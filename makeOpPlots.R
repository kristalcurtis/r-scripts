# 7.28.10
# want to show a bunch of diff fits for each op

rm(list=ls())
dataPath="~/Desktop"

load(paste(dataPath, "/ops.RData", sep=""))

library(MASS)
?fitdistr

# checking it out

op3.normal = fitdistr(op3$latency_ms, "normal")
op3.exponential = fitdistr(op3$latency_ms, "exponential")
op3.gamma = fitdistr(op3$latency_ms, "gamma")
op3.weibull = fitdistr(op3$latency_ms, "weibull")

op3.normalSamples = rnorm(10000, mean=op3.normal$estimate["mean"], sd=op3.normal$estimate["sd"])
op3.exponentialSamples = rexp(10000, rate=op3.exponential$estimate["rate"])
op3.gammaSamples = rgamma(10000, shape=op3.gamma$estimate["shape"], rate=op3.gamma$estimate["rate"])
op3.weibullSamples = rweibull(10000, shape=op3.weibull$estimate["shape"], scale=op3.weibull$estimate["scale"])


par(mfrow=c(3,2), mar=c(5,5,4,2)+0.1)
xlim=c(-20,100)
plot(density(op3$latency_ms), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Actual op3", col=0)
lines(density(op3$latency_ms), lw=2)
values = op3$latency_ms
abline(v=mean(values), col="purple", lw=2)
abline(v=median(values), col="red", lw=2)
q90 = quantile(values,0.9)
abline(v=q90, col="blue", lw=2)
q99 = quantile(values,0.99)
abline(v=quantile(values,0.99), col="green", lw=2)

legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)

values = op3.normalSamples
plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Normal", col=0)
lines(density(values), lw=2)
abline(v=mean(values), col="purple", lw=2)
abline(v=median(values), col="red", lw=2)
q90 = quantile(values,0.9)
abline(v=q90, col="blue", lw=2)
q99 = quantile(values,0.99)
abline(v=quantile(values,0.99), col="green", lw=2)

legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


values = op3.exponentialSamples
plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Exponential", col=0)
lines(density(values), lw=2)
abline(v=mean(values), col="purple", lw=2)
abline(v=median(values), col="red", lw=2)
q90 = quantile(values,0.9)
abline(v=q90, col="blue", lw=2)
q99 = quantile(values,0.99)
abline(v=quantile(values,0.99), col="green", lw=2)

legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


values = op3.gammaSamples
plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Gamma", col=0)
lines(density(values), lw=2)
abline(v=mean(values), col="purple", lw=2)
abline(v=median(values), col="red", lw=2)
q90 = quantile(values,0.9)
abline(v=q90, col="blue", lw=2)
q99 = quantile(values,0.99)
abline(v=quantile(values,0.99), col="green", lw=2)

legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


values = op3.weibullSamples
plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Weibull", col=0)
lines(density(values), lw=2)
abline(v=mean(values), col="purple", lw=2)
abline(v=median(values), col="red", lw=2)
q90 = quantile(values,0.9)
abline(v=q90, col="blue", lw=2)
q99 = quantile(values,0.99)
abline(v=quantile(values,0.99), col="green", lw=2)

legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


# cdfs
op3cdf.actual = ecdf(op3$latency_ms)
op3cdf.seq = seq(min(op3$latency_ms), max(op3$latency_ms))

op3cdf.normal = ecdf(op3.normalSamples)
op3cdf.exponential = ecdf(op3.exponentialSamples)
op3cdf.gamma = ecdf(op3.gammaSamples)
op3cdf.weibull = ecdf(op3.weibullSamples)


plot(op3cdf.seq, op3cdf.actual(op3cdf.seq), col=0, log="x", xlab="Latency (ms)", ylab="Quantile", main="CDF for op3 Latency")
lines(op3cdf.seq, op3cdf.actual(op3cdf.seq), col="black", lw=2, log="x")
lines(op3cdf.seq, op3cdf.normal(op3cdf.seq), col="red", lw=2, log="x")
lines(op3cdf.seq, op3cdf.exponential(op3cdf.seq), col="green", lw=2, log="x")
lines(op3cdf.seq, op3cdf.gamma(op3cdf.seq), col="blue", lw=2, log="x")
lines(op3cdf.seq, op3cdf.weibull(op3cdf.seq), col="purple", lw=2, log="x")

legend("bottomright", legend=c("actual", "normal", "exponential", "gamma", "weibull"), col=c("black", "red", "green", "blue", "purple"), lw=2)



plotOpSingleDistrFits(op1$latency_ms, 1, "~/Desktop/opPlots")
plotOpSingleDistrFits(op2$latency_ms, 2, "~/Desktop/opPlots")
plotOpSingleDistrFits(op3$latency_ms, 3, "~/Desktop/opPlots")
plotOpSingleDistrFits(op4$latency_ms, 4, "~/Desktop/opPlots")
plotOpSingleDistrFits(op5$latency_ms, 5, "~/Desktop/opPlots")
plotOpSingleDistrFits(op6$latency_ms, 6, "~/Desktop/opPlots")
plotOpSingleDistrFits(op7$latency_ms, 7, "~/Desktop/opPlots")
plotOpSingleDistrFits(op8$latency_ms, 8, "~/Desktop/opPlots")
plotOpSingleDistrFits(op9$latency_ms, 9, "~/Desktop/opPlots")


plotOpSingleDistrFits = function(opData, opNum, outputPath) {
	library(MASS)

	# fit diff types of single distr to op data
	op.normal = fitdistr(opData, "normal")
	op.exponential = fitdistr(opData, "exponential")
	op.gamma = fitdistr(opData, "gamma")
	op.weibull = fitdistr(opData, "weibull")


	op.normalSamples = rnorm(10000, mean=op.normal$estimate["mean"], sd=op.normal$estimate["sd"])
	op.exponentialSamples = rexp(10000, rate=op.exponential$estimate["rate"])
	op.gammaSamples = rgamma(10000, shape=op.gamma$estimate["shape"], rate=op.gamma$estimate["rate"])
	op.weibullSamples = rweibull(10000, shape=op.weibull$estimate["shape"], scale=op.weibull$estimate["scale"])
	#op.normalDensity = getPredictedOpDensityUsingSingleDistr(opData, "normal", densityDomain)
	#op.exponentialDensity = getPredictedOpDensityUsingSingleDistr(opData, "exponential", densityDomain)
	#op.gammaDensity = getPredictedOpDensityUsingSingleDistr(opData, "gamma", densityDomain)
	#op.weibullDensity = getPredictedOpDensityUsingSingleDistr(opData, "weibull", densityDomain)
	
	
	# make plot
	dir.create(outputPath)
	pdf(paste(outputPath, "/op", opNum, "-singleDistrFits.pdf", sep=""))

	par(mfrow=c(3,2), mar=c(5,5,4,2)+0.1)
	xlim=c(((-2)*mean(opData)),(1.1*quantile(opData,0.99)))
	#xlim=c(-20,100)
	print(xlim)

	plot(density(opData), xlim=xlim, xlab="Latency (ms)", ylab="Density", main=paste("Actual op", opNum, sep=""), col=0)
	lines(density(opData), lw=2)
	values = opData
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)

	values = op.normalSamples
	plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Normal", col=0)
	lines(density(values), lw=2)
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


	values = op.exponentialSamples
	plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Exponential", col=0)
	lines(density(values), lw=2)
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


	values = op.gammaSamples
	plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Gamma", col=0)
	lines(density(values), lw=2)
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


	values = op.weibullSamples
	plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Weibull", col=0)
	lines(density(values), lw=2)
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


	# cdfs
	opcdf.actual = ecdf(opData)
	opcdf.seq = seq(min(opData), max(opData))

	opcdf.normal = ecdf(op.normalSamples)
	opcdf.exponential = ecdf(op.exponentialSamples)
	opcdf.gamma = ecdf(op.gammaSamples)
	opcdf.weibull = ecdf(op.weibullSamples)


	plot(opcdf.seq, opcdf.actual(opcdf.seq), col=0, log="x", xlab="Latency (ms)", ylab="Quantile", main=paste("CDF for op", opNum, " Latency", sep=""))
	lines(opcdf.seq, opcdf.actual(opcdf.seq), col="black", lw=2, log="x")
	lines(opcdf.seq, opcdf.normal(opcdf.seq), col="red", lw=2, log="x")
	lines(opcdf.seq, opcdf.exponential(opcdf.seq), col="green", lw=2, log="x")
	lines(opcdf.seq, opcdf.gamma(opcdf.seq), col="blue", lw=2, log="x")
	lines(opcdf.seq, opcdf.weibull(opcdf.seq), col="purple", lw=2, log="x")

	legend("bottomright", legend=c("actual", "normal", "exponential", "gamma", "weibull"), col=c("black", "red", "green", "blue", "purple"), lw=2)	
	
	dev.off()
}



plotOpSingleDistrFitsUsingDensity = function(opData, opNum, outputPath, densityDomain) {
	source("convolutionForOpCombination-functions.R")
	source("useOpDensitiesForQueryDensityPrediction.R")

	#library(MASS)

	# fit diff types of single distr to op data
	#op.normal = fitdistr(opData, "normal")
	#op.exponential = fitdistr(opData, "exponential")
	#op.gamma = fitdistr(opData, "gamma")
	#op.weibull = fitdistr(opData, "weibull")


	#op.normalSamples = rnorm(10000, mean=op.normal$estimate["mean"], sd=op.normal$estimate["sd"])
	#op.exponentialSamples = rexp(10000, rate=op.exponential$estimate["rate"])
	#op.gammaSamples = rgamma(10000, shape=op.gamma$estimate["shape"], rate=op.gamma$estimate["rate"])
	#op.weibullSamples = rweibull(10000, shape=op.weibull$estimate["shape"], scale=op.weibull$estimate["scale"])
	op.normalDensity = getPredictedOpDensityUsingSingleDistr(opData, "normal", densityDomain)
	op.exponentialDensity = getPredictedOpDensityUsingSingleDistr(opData, "exponential", densityDomain)
	op.gammaDensity = getPredictedOpDensityUsingSingleDistr(opData, "gamma", densityDomain)
	op.weibullDensity = getPredictedOpDensityUsingSingleDistr(opData, "weibull", densityDomain)
	
	
	# make plot
	dir.create(outputPath)
	pdf(paste(outputPath, "/op", opNum, "-singleDistrFits.pdf", sep=""))

	par(mfrow=c(3,2), mar=c(5,5,4,2)+0.1)
	xlim=c(((-2)*mean(opData)),(1.1*quantile(opData,0.99)))
	#xlim=c(-20,100)
	print(xlim)

	plot(density(opData), xlim=xlim, xlab="Latency (ms)", ylab="Density", main=paste("Actual op", opNum, sep=""), col=0)
	lines(density(opData), lw=2)
	values = opData
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


	print("plotting normal density...")
	density=op.normalDensity
	median=getQuantileFromDensity(density,0.5)
	q90=getQuantileFromDensity(density,0.9)
	q99=getQuantileFromDensity(density,0.99)
	plot(density, xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Normal", col=0)
	lines(density, lw=2)
	abline(v=median, lw=2, col="red")
	abline(v=q90, lw=2, col="blue")
	abline(v=q99, lw=2, col="green")

	legend("topright", legend=c(paste("median=", round(median, digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)


	print("plotting exponential density...")
	density=op.exponentialDensity
	median=getQuantileFromDensity(density,0.5)
	q90=getQuantileFromDensity(density,0.9)
	q99=getQuantileFromDensity(density,0.99)
	plot(density, xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Exponential", col=0)
	lines(density, lw=2)
	abline(v=median, lw=2, col="red")
	abline(v=q90, lw=2, col="blue")
	abline(v=q99, lw=2, col="green")

	legend("topright", legend=c(paste("median=", round(median, digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)


	print("plotting gamma density...")
	density=op.gammaDensity
	median=getQuantileFromDensity(density,0.5)
	q90=getQuantileFromDensity(density,0.9)
	q99=getQuantileFromDensity(density,0.99)
	plot(density, xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Gamma", col=0)
	lines(density, lw=2)
	abline(v=median, lw=2, col="red")
	abline(v=q90, lw=2, col="blue")
	abline(v=q99, lw=2, col="green")

	legend("topright", legend=c(paste("median=", round(median, digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)


	print("plotting weibull density...")
	density=op.weibullDensity
	median=getQuantileFromDensity(density,0.5)
	q90=getQuantileFromDensity(density,0.9)
	q99=getQuantileFromDensity(density,0.99)
	plot(density, xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Weibull", col=0)
	lines(density, lw=2)
	abline(v=median, lw=2, col="red")
	abline(v=q90, lw=2, col="blue")
	abline(v=q99, lw=2, col="green")

	legend("topright", legend=c(paste("median=", round(median, digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("red", "blue", "green"), lw=2)

	
	dev.off()
}




plotOpMMFits = function(opData, opNum, numMixtures, outputPath) {
	source("emForGeneralMixtureModel.R")

	# fit MM
	print("running em...")
	op.normalMM = em(opData, numMixtures[1], "normal", "evenlySpacedByQuantile")
	op.exponentialMM = em(opData, numMixtures[2], "exponential", "evenlySpacedByQuantile")
	op.gammaMM = em(opData, numMixtures[3], "gamma", "evenlySpacedByQuantile")
	op.weibullMM = em(opData, numMixtures[4], "weibull", "evenlySpacedByQuantile")
	

	#op.normalSamples = rnorm(10000, mean=op.normal$estimate["mean"], sd=op.normal$estimate["sd"])
	#op.exponentialSamples = rexp(10000, rate=op.exponential$estimate["rate"])
	#op.gammaSamples = rgamma(10000, shape=op.gamma$estimate["shape"], rate=op.gamma$estimate["rate"])
	#op.weibullSamples = rweibull(10000, shape=op.weibull$estimate["shape"], scale=op.weibull$estimate["scale"])
	#op.normalDensity = getPredictedOpDensityUsingSingleDistr(opData, "normal", densityDomain)
	#op.exponentialDensity = getPredictedOpDensityUsingSingleDistr(opData, "exponential", densityDomain)
	#op.gammaDensity = getPredictedOpDensityUsingSingleDistr(opData, "gamma", densityDomain)
	#op.weibullDensity = getPredictedOpDensityUsingSingleDistr(opData, "weibull", densityDomain)
	
	op.normalSamples = sampleFromMixtureModel("normal", op.normalMM$params, 10000)
	op.exponentialSamples = sampleFromMixtureModel("exponential", op.exponentialMM$params, 10000)
	op.gammaSamples = sampleFromMixtureModel("gamma", op.gammaMM$params, 10000)
	op.weibullSamples = sampleFromMixtureModel("weibull", op.weibullMM$params, 10000)
	
	
	# make plot
	dir.create(outputPath)
	pdf(paste(outputPath, "/op", opNum, "-MMFits.pdf", sep=""))

	par(mfrow=c(3,2), mar=c(5,5,4,2)+0.1)
	xlim=c(((-2)*mean(opData)),(1.1*quantile(opData,0.99)))
	#xlim=c(-20,100)
	print(xlim)

	plot(density(opData), xlim=xlim, xlab="Latency (ms)", ylab="Density", main=paste("Actual op", opNum, sep=""), col=0)
	lines(density(opData), lw=2)
	values = opData
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)

	values = op.normalSamples
	plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Normal", col=0)
	lines(density(values), lw=2)
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


	values = op.exponentialSamples
	plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Exponential", col=0)
	lines(density(values), lw=2)
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


	values = op.gammaSamples
	plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Gamma", col=0)
	lines(density(values), lw=2)
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


	values = op.weibullSamples
	plot(density(values), xlim=xlim, xlab="Latency (ms)", ylab="Density", main="Weibull", col=0)
	lines(density(values), lw=2)
	abline(v=mean(values), col="purple", lw=2)
	abline(v=median(values), col="red", lw=2)
	q90 = quantile(values,0.9)
	abline(v=q90, col="blue", lw=2)
	q99 = quantile(values,0.99)
	abline(v=quantile(values,0.99), col="green", lw=2)

	legend("topright", legend=c(paste("mean=", round(mean(values), digits=2), "ms", sep=""), paste("median=", round(median(values), digits=2), "ms", sep=""), paste("90th=", round(q90, digits=2), "ms", sep=""), paste("99th=", round(q99, digits=2), "ms", sep="")), col=c("purple", "red", "blue", "green"), lw=2)


	# cdfs
	opcdf.actual = ecdf(opData)
	opcdf.seq = seq(min(opData), max(opData))

	opcdf.normal = ecdf(op.normalSamples)
	opcdf.exponential = ecdf(op.exponentialSamples)
	opcdf.gamma = ecdf(op.gammaSamples)
	opcdf.weibull = ecdf(op.weibullSamples)


	plot(opcdf.seq, opcdf.actual(opcdf.seq), col=0, log="x", xlab="Latency (ms)", ylab="Quantile", main=paste("CDF for op", opNum, " Latency", sep=""))
	lines(opcdf.seq, opcdf.actual(opcdf.seq), col="black", lw=2, log="x")
	lines(opcdf.seq, opcdf.normal(opcdf.seq), col="red", lw=2, log="x")
	lines(opcdf.seq, opcdf.exponential(opcdf.seq), col="green", lw=2, log="x")
	lines(opcdf.seq, opcdf.gamma(opcdf.seq), col="blue", lw=2, log="x")
	lines(opcdf.seq, opcdf.weibull(opcdf.seq), col="purple", lw=2, log="x")

	legend("bottomright", legend=c("actual", "normal", "exponential", "gamma", "weibull"), col=c("black", "red", "green", "blue", "purple"), lw=2)	
	
	dev.off()
}