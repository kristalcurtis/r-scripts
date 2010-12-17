# towards modeling thoughtsByHashTag
queryPrims = as.data.frame(read.delim(file="~/thoughtsByHashTag/primitive/bins/part-00000", header=FALSE, row.names=1))



op=2
op2 = as.data.frame(read.csv(file=paste("~/op", op,"-5-threads/op", op, "ops.csv", sep="")))
op2prims = as.data.frame(read.delim(file=paste("~/op", op, "-5-threads/primitive/bins/part-00000", sep=""), header=FALSE, row.names=1))

dim(op2)
op2[1:10,]

op2prims[1:10,1]
dim(op2prims)
summary(op2prims)
summary(op2prims[301:361,1])



op=5
op5 = as.data.frame(read.csv(file=paste("~/op", op,"-5-threads/op", op, "ops.csv", sep="")))
op5prims = as.data.frame(read.delim(file=paste("~/op", op, "-5-threads/primitive/bins/part-00000", sep=""), header=FALSE, row.names=1))

dim(op5)
op5[1:10,]

op5prims[1:10,1]
summary(op5prims)
summary(op5prims[301:361,])




op=6
op6 = as.data.frame(read.csv(file=paste("~/op", op,"-5-threads/op", op, "ops.csv", sep="")))

dim(op6)
op6[1:10,]



op=8
op8 = as.data.frame(read.csv(file=paste("~/op", op,"-5-threads/op", op, "ops.csv", sep="")))

dim(op8)
op8[1:10,]



h2 = hist(op2$latency_ms, breaks=100)
h5 = hist(op5$latency_ms, breaks=100)
h6 = hist(op6$latency_ms, breaks=100)
h8 = hist(op8$latency_ms, breaks=100)

median(samples)
quantile(samples, 0.75)
quantile(samples, 0.9)
quantile(samples, 0.99)

save(samples, file="~/thoughtsByHashTagSamples1msThinkTime.RData")


load(file="~/Desktop/thoughtsByHashTag.RData")
ls()
query[1:10,]
summary(query$latency_ms)
quantile(query$latency_ms, 0.99)

xmax=200
hist(query$latency_ms, breaks=30, xlim=c(0,xmax))
abline(v=median(query$latency_ms), lw=2, col="red")
abline(v=quantile(query$latency_ms, 0.6), lw=2, col="orange")
abline(v=quantile(query$latency_ms, 0.7), lw=2, col="green")
abline(v=quantile(query$latency_ms, 0.8), lw=2, col="blue")
abline(v=quantile(query$latency_ms, 0.9), lw=2, col="purple")
abline(v=quantile(query$latency_ms, 0.91), lw=2, col="red")
abline(v=quantile(query$latency_ms, 0.92), lw=2, col="orange")
abline(v=quantile(query$latency_ms, 0.93), lw=2, col="yellow")
abline(v=quantile(query$latency_ms, 0.94), lw=2, col="green")
abline(v=quantile(query$latency_ms, 0.95), lw=2, col="blue")
abline(v=quantile(query$latency_ms, 0.96), lw=2, col="purple")
abline(v=quantile(query$latency_ms, 0.97), lw=2, col="magenta")
abline(v=quantile(query$latency_ms, 0.98), lw=2, col="cyan")
abline(v=quantile(query$latency_ms, 0.99), lw=2, col="black")


which(query$latency_ms > 400)
which(query$latency_ms > 300)
which(query$latency_ms > 200)
which(query$latency_ms > 100)


ls()
rm(samples)

load(file="~/Desktop/thoughtsByHashTagSamples1msThinkTime.RData")
ls()

par(mfrow=c(2,1))
xmax=200
hist(samples, xlim=c(0,xmax), breaks=50)
abline(v=median(samples), lw=2, col="red")
abline(v=quantile(samples, 0.6), lw=2, col="orange")
abline(v=quantile(samples, 0.7), lw=2, col="green")
abline(v=quantile(samples, 0.8), lw=2, col="blue")
abline(v=quantile(samples, 0.9), lw=2, col="purple")
abline(v=quantile(samples, 0.91), lw=2, col="red")
abline(v=quantile(samples, 0.92), lw=2, col="orange")
abline(v=quantile(samples, 0.93), lw=2, col="yellow")
abline(v=quantile(samples, 0.94), lw=2, col="green")
abline(v=quantile(samples, 0.95), lw=2, col="blue")
abline(v=quantile(samples, 0.96), lw=2, col="purple")
abline(v=quantile(samples, 0.97), lw=2, col="magenta")
abline(v=quantile(samples, 0.98), lw=2, col="cyan")
abline(v=quantile(samples, 0.99), lw=2, col="black")


hist(query$latency_ms, breaks=100, xlim=c(0,xmax))
abline(v=median(query$latency_ms), lw=2, col="red")
abline(v=quantile(query$latency_ms, 0.6), lw=2, col="orange")
abline(v=quantile(query$latency_ms, 0.7), lw=2, col="green")
abline(v=quantile(query$latency_ms, 0.8), lw=2, col="blue")
abline(v=quantile(query$latency_ms, 0.9), lw=2, col="purple")
abline(v=quantile(query$latency_ms, 0.91), lw=2, col="red")
abline(v=quantile(query$latency_ms, 0.92), lw=2, col="orange")
abline(v=quantile(query$latency_ms, 0.93), lw=2, col="yellow")
abline(v=quantile(query$latency_ms, 0.94), lw=2, col="green")
abline(v=quantile(query$latency_ms, 0.95), lw=2, col="blue")
abline(v=quantile(query$latency_ms, 0.96), lw=2, col="purple")
abline(v=quantile(query$latency_ms, 0.97), lw=2, col="magenta")
abline(v=quantile(query$latency_ms, 0.98), lw=2, col="cyan")
abline(v=quantile(query$latency_ms, 0.99), lw=2, col="black")





# looking at the actual ops
actualOps = as.data.frame(read.csv(file="~/thoughtsByHashTag/ops.csv"))

dim(actualOps)
actualOps[1:10,]

actualOp2 = actualOps[actualOps$opType==2,]
actualOp5 = actualOps[actualOps$opType==5,]
actualOp6 = actualOps[actualOps$opType==6,]
actualOp8 = actualOps[actualOps$opType==8,]

save(actualOp2, actualOp5, actualOp6, actualOp8, file="~/thoughtsByHashTag/ops.RData")

load(file="~/Desktop/ops.RData")
ls()

# Nice plotting
op=2
actual = actualOp2
benchmark = op2

actual = actual[which(actual$latency_ms < 2*quantile(actual$latency_ms, 0.99)),]  # extreme outliers are more common in the ops-from-queries case
benchmark = benchmark[which(benchmark$latency_ms < 2*quantile(benchmark$latency_ms, 0.99)),]

baseNumBreaks = 100
pdf(paste("~/Desktop/op", op, "-thoughtsByHashTag.pdf", sep=""))
par(mfrow=c(2,1), mar=c(5,5,4,2)+0.1)
xmax = 1.1*(max(quantile(actual$latency_ms, 0.99), quantile(benchmark$latency_ms, 0.99)))
distr = actual$latency_ms
#numBreaks = round(length(distr)/50)
numBreaks=round(nrow(benchmark)/nrow(actual)*baseNumBreaks)
hist(distr, xlim=c(0,xmax), breaks=numBreaks, xlab="Latency (ms)", main=paste("Actual op", op, " (thoughtsByHashTag)", sep=""))
abline(v=median(distr), lw=2, col="red")
abline(v=quantile(distr, 0.6), lw=2, col="orange")
abline(v=quantile(distr, 0.7), lw=2, col="green")
abline(v=quantile(distr, 0.8), lw=2, col="blue")
abline(v=quantile(distr, 0.9), lw=2, col="purple")
abline(v=quantile(distr, 0.99), lw=2, col="red")
distr.quantiles = quantile(distr, c(0.5, 0.6, 0.7, 0.8, 0.9, 0.99))
legend("topright", 
	legend=c(paste("median=", round(distr.quantiles[1], 2)),
		paste("60th=", round(distr.quantiles[2], 2)),
		paste("70th=", round(distr.quantiles[3], 2)),
		paste("80th=", round(distr.quantiles[4], 2)),
		paste("90th=", round(distr.quantiles[5], 2)),
		paste("99th=", round(distr.quantiles[6], 2))
		),
	col=c("red", "orange", "green", "blue", "purple", "red"),
	lwd=2
)


distr = benchmark$latency_ms
#numBreaks = round(length(distr)/50)
#numBreaks=500
numBreaks=round(nrow(actual)/nrow(benchmark)*baseNumBreaks)
hist(distr, xlim=c(0,xmax), breaks=numBreaks, xlab="Latency (ms)", main=paste("Benchmarked op", op, sep=""))
abline(v=median(distr), lw=2, col="red")
abline(v=quantile(distr, 0.6), lw=2, col="orange")
abline(v=quantile(distr, 0.7), lw=2, col="green")
abline(v=quantile(distr, 0.8), lw=2, col="blue")
abline(v=quantile(distr, 0.9), lw=2, col="purple")
abline(v=quantile(distr, 0.99), lw=2, col="red")
distr.quantiles = quantile(distr, c(0.5, 0.6, 0.7, 0.8, 0.9, 0.99))
legend("topright", 
	legend=c(paste("median=", round(distr.quantiles[1], 2)),
		paste("60th=", round(distr.quantiles[2], 2)),
		paste("70th=", round(distr.quantiles[3], 2)),
		paste("80th=", round(distr.quantiles[4], 2)),
		paste("90th=", round(distr.quantiles[5], 2)),
		paste("99th=", round(distr.quantiles[6], 2))
		),
	col=c("red", "orange", "green", "blue", "purple", "red"),
	lwd=2
)
dev.off()
