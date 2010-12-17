myThoughtsSampler = function(h1, h4, h6, h9, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sum(sample(h1$mids, 2, replace=TRUE, prob=h1$density))
		samples[i] = samples[i] + sample(h4$mids, 1, replace=TRUE, prob=h4$density)
		samples[i] = samples[i] + sum(sample(h6$mids, 3, replace=TRUE, prob=h6$density))
		samples[i] = samples[i] + sample(h9$mids, 1, replace=TRUE, prob=h9$density)
	}

	return(samples)
}


# On EC2
mTqueries = as.data.frame(read.csv("~/myThoughts/queries.csv"))

mTops = as.data.frame(read.csv("~/myThoughts/ops.csv"))

save(mTqueries, mTops, file="~/myThoughts/myThoughts.RData")


# On laptop
load("~/Desktop/myThoughts.RData")

numSamples=1000
mTsamples = myThoughtsSampler(h1, h4, h6, h9, numSamples)

# comparing actual vs. predicted latency distr
pdf("~/Desktop/Retreat-Figs/myThoughts.pdf")
par(mfrow=c(2,1), mar=c(5,5,4,2)+0.1)
xmax=max(mTqueries$latency_ms, samples)
latencyDistr = mTqueries$latency_ms
hist(latencyDistr, xlim=c(0,xmax), breaks=40, xlab="Latency (ms)", ylab="Count", main="myThoughts Latency Distribution, Actual")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)

latencyDistr = samples
hist(latencyDistr, xlim=c(0,xmax), breaks=50, xlab="Latency (ms)", ylab="Count", main="myThoughts Latency Distribution, Predicted")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)
dev.off()
