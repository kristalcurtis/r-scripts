# 6.1.10
# attempting to model myFollowing

# On EC2
queries = as.data.frame(read.csv("~/myFollowing/queries.csv"))
ls()
dim(queries)
queries[1:10,]

ops = as.data.frame(read.csv("~/myFollowing/ops.csv"))
ls()
dim(ops)
ops[1:10,]
ops.thread5=ops[ops$threadNum==5,]
ops.thread5[1:15,]
unique(ops$threadNum)
unique(ops$opType)

prims = as.data.frame(read.csv("~/myFollowing/prims.csv"))
bins = as.data.frame(read.delim("~/myFollowing/primitive/bins/part-00000", header=FALSE, row.names=1))
dim(bins)
summary(bins)

save(queries, ops, prims, bins, file="~/myFollowing/myFollowing.RData")

## On laptop
rm(list=ls())
load("~/Desktop/Retreat-Modeling/myFollowing.RData")
ls()



myFollowingSampler = function(h1, h4, h5, h6, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sum(sample(h1$mids, 2, replace=TRUE, prob=h1$density))
		samples[i] = samples[i] + sample(h4$mids, 1, replace=TRUE, prob=h4$density)
		samples[i] = samples[i] + sample(h5$mids, 1, replace=TRUE, prob=h5$density)
		samples[i] = samples[i] + sum(sample(h6$mids, 4, replace=TRUE, prob=h6$density))
	}

	return(samples)
}


# Getting op histograms
numBreaks=100
op=1
op1 = as.data.frame(read.csv(file=paste("~/Desktop/opsAll/op", op, "ops.csv", sep="")))
dim(op1)
op1[1:10,]
unique(op1$aSize)
h1=hist(op1$latency_ms, breaks=numBreaks)

op=2
op2 = as.data.frame(read.csv(file=paste("~/Desktop/opsAll/op", op, "ops.csv", sep="")))
dim(op2)
op2[1:10,]
unique(op2$aSize)
h2=hist(op2$latency_ms, breaks=numBreaks)

op=3
op3 = as.data.frame(read.csv(file=paste("~/Desktop/opsAll/op", op, "ops.csv", sep="")))
dim(op3)
op3[1:10,]
unique(op3$aSize)
h3=hist(op3$latency_ms, breaks=numBreaks)

op=4
op4 = as.data.frame(read.csv(file=paste("~/Desktop/opsAll/op", op, "ops.csv", sep="")))
dim(op4)
op4[1:10,]
unique(op4$aSize)
h4=hist(op4$latency_ms, breaks=numBreaks)

op=5
op5 = as.data.frame(read.csv(file=paste("~/Desktop/opsAll/op", op, "ops.csv", sep="")))
dim(op5)
op5[1:10,]
unique(op5$aSize)
h5=hist(op5$latency_ms, breaks=numBreaks)

op=6
op6 = as.data.frame(read.csv(file=paste("~/Desktop/opsAll/op", op, "ops.csv", sep="")))
dim(op6)
op6[1:10,]
unique(op6$aSize)
h6=hist(op6$latency_ms, breaks=numBreaks)

op=7
op7 = as.data.frame(read.csv(file=paste("~/Desktop/opsAll/op", op, "ops.csv", sep="")))
dim(op7)
op7[1:10,]
unique(op7$aSize)
h7=hist(op7$latency_ms, breaks=numBreaks)

op=8
op8 = as.data.frame(read.csv(file=paste("~/Desktop/opsAll/op", op, "ops.csv", sep="")))
dim(op8)
op8[1:10,]
unique(op8$aSize)
h8=hist(op8$latency_ms, breaks=numBreaks)

op=9
op9 = as.data.frame(read.csv(file=paste("~/Desktop/opsAll/op", op, "ops.csv", sep="")))
dim(op9)
op9[1:10,]
unique(op9$aSize)
h9=hist(op9$latency_ms, breaks=numBreaks)


numSamples=1000
mFsamples = myFollowingSampler(h1, h4, h5, h6, numSamples)

# comparing actual vs. predicted latency distr
pdf("~/Desktop/myFollowing.pdf")
par(mfrow=c(2,1), mar=c(5,5,4,2)+0.1)
xmax=max(queries$latency_ms, samples)
latencyDistr = queries$latency_ms
hist(latencyDistr, xlim=c(0,xmax), breaks=20, xlab="Latency (ms)", ylab="Count", main="myFollowing Latency Distribution, Actual")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)

latencyDistr = samples
hist(latencyDistr, xlim=c(0,xmax), breaks=50, xlab="Latency (ms)", ylab="Count", main="myFollowing Latency Distribution, Predicted")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)
dev.off()

# looking op-by-op
par(mfrow=c(2,1))
opMtx = op4
op = 4
#xmax = max(ops$latency_ms[ops$opType==op], opMtx$latency_ms)
xmax=300
hist(ops$latency_ms[ops$opType==op], xlim=c(0,xmax))
hist(opMtx$latency_ms, xlim=c(0,xmax), breaks=50)
summary(ops$latency_ms[ops$opType==op])
summary(opMtx$latency_ms)

hist(ops$latency_ms[ops$opType==op], breaks=100, xlim=c(0,10))



load("~/Desktop/opsAll/op4.RData")	# overwrites op4 from above
ls()
op4[1:10,]
unique(op4$aSize)
h4=hist(op4$latency_ms, breaks=100)


save(list=ls(), file="~/Desktop/myFollowing.RData")