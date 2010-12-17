needsApprovalSampler = function(h1, h3, h4, h6, h9, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sum(sample(h1$mids, 2, replace=TRUE, prob=h1$density))
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sample(h4$mids, 1, replace=TRUE, prob=h4$density)
		samples[i] = samples[i] + sum(sample(h6$mids, 3, replace=TRUE, prob=h6$density))
		samples[i] = samples[i] + sample(h9$mids, 1, replace=TRUE, prob=h9$density)
	}

	return(samples)
}


# On EC2
nAqueries = as.data.frame(read.csv("~/needsApproval/queries.csv"))

nAops = as.data.frame(read.csv("~/needsApproval/ops.csv"))

save(nAqueries, nAops, file="~/needsApproval/needsApproval.RData")


# On laptop
load("~/Desktop/needsApproval.RData")

numSamples=1000
nAsamples = needsApprovalSampler(h1, h3, h4, h6, h9, numSamples)

pdf("~/Desktop/Retreat-Figs/needsApproval.pdf")
par(mfrow=c(2,1), mar=c(5,5,4,2)+0.1)
xmax=max(nAqueries$latency_ms, samples)
latencyDistr = nAqueries$latency_ms
hist(latencyDistr, xlim=c(0,xmax), breaks=40, xlab="Latency (ms)", ylab="Count", main="needsApproval Latency Distribution, Actual")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)

latencyDistr = samples
hist(latencyDistr, xlim=c(0,xmax), breaks=25, xlab="Latency (ms)", ylab="Count", main="needsApproval Latency Distribution, Predicted")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)
dev.off()
