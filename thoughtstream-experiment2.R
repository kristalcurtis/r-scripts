thoughtstreamSampler = function(h1, h3, h4, h5, h6, h7, h8, h9, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sum(sample(h1$mids, 2, replace=TRUE, prob=h1$density))
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sum(sample(h4$mids, 2, replace=TRUE, prob=h4$density))
		samples[i] = samples[i] + sample(h5$mids, 1, replace=TRUE, prob=h5$density)
		samples[i] = samples[i] + sum(sample(h6$mids, 5, replace=TRUE, prob=h6$density))
		samples[i] = samples[i] + sample(h7$mids, 1, replace=TRUE, prob=h7$density)
		samples[i] = samples[i] + sample(h8$mids, 1, replace=TRUE, prob=h8$density)
		samples[i] = samples[i] + sample(h9$mids, 1, replace=TRUE, prob=h9$density)
	}

	return(samples)
}


ls()


op3.100 = as.data.frame(read.csv("~/Desktop/op3ops.csv"))
dim(op3.100)

h3.100 = hist(op3.100$latency_ms, breaks=100)

numSamples=1000
tsSamples = thoughtstreamSampler(h1, h3.100, h4, h5, h6, h7, h8, h9, numSamples)

# On EC2
tsqueries = as.data.frame(read.csv("~/ts/queries.csv"))

tsops = as.data.frame(read.csv("~/ts/ops.csv"))

save(tsqueries, tsops, file="~/ts/ts.RData")


# On laptop
load("~/Desktop/ts.RData")



# Compare
par(mfrow=c(2,1), mar=c(5,5,4,2)+0.1)
xmax=max(tsqueries$latency_ms, tsSamples)
latencyDistr = tsqueries$latency_ms
hist(latencyDistr, xlim=c(0,xmax), breaks=20, xlab="Latency (ms)", ylab="Count", main="myFollowing Latency Distribution, Actual")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)

latencyDistr = tsSamples
hist(latencyDistr, xlim=c(0,xmax), breaks=50, xlab="Latency (ms)", ylab="Count", main="myFollowing Latency Distribution, Predicted")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)

