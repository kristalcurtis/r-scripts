ls()


userByHometownSampler = function(h2, h3, h6, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sample(h2$mids, 1, replace=TRUE, prob=h2$density)
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sample(h6$mids, 1, replace=TRUE, prob=h6$density)
	}

	return(samples)
}


uBHsamples = userByHometownSampler(h2, h3, h6, numSamples)
hist(uBHsamples, breaks=25)

# On EC2
uBHqueries = as.data.frame(read.csv("queries.csv"))
uBHops = as.data.frame(read.csv("ops.csv"))
save(uBHqueries, uBHops, file="uBH.RData")

# On laptop
load("~/Desktop/uBH.RData")
ls()

median(uBHsamples)
median(uBHqueries$latency_ms)

pdf("~/Desktop/Retreat-Figs/userByHometown.pdf")
par(mfrow=c(2,1), mar=c(5,5,4,2)+0.1)
xmax=max(uBHqueries$latency_ms, uBHsamples)
latencyDistr = uBHqueries$latency_ms
hist(latencyDistr, xlim=c(0,xmax), breaks=40, xlab="Latency (ms)", ylab="Count", main="userByHometown Latency Distribution, Actual")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)

latencyDistr = uBHsamples
hist(latencyDistr, xlim=c(0,xmax), breaks=25, xlab="Latency (ms)", ylab="Count", main="userByHometown Latency Distribution, Predicted")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)
dev.off()
