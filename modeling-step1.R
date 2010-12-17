# Try to match the userByEmail latency distribution using the op hists from my op benchmarking

rm(list=ls())
load("~/Desktop/op2-hists/i=10,j=10,k=10,l=10.RData")
ls()
h2 = h
op2Bin = bin
op2Bin[1:10,]

load("~/Desktop/op3-hists/i=10,j=10,k=10,l=10.RData")
h3 = h
op3Bin = bin
ls()

load("~/Desktop/op6-hists/i=10,j=10,k=10,l=10.RData")
h6 = h
op6Bin = bin
ls()


userByEmailSampler = function(h2, h3, h6, sampleID, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sample(h2$mids, 1, replace=TRUE, prob=h2$density)
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sample(h6$mids, 1, replace=TRUE, prob=h6$density)
	}

	save(samples, file=paste("~/Desktop/userByEmailSamples/sample", sampleID,".RData",sep=""))

	return(samples)
}


numSamples = 1000
for (i in 1:10) {
	samples = userByEmailSampler(h2, h3, h6, i, numSamples)
	print(quantile(samples, 0.99))
}


load("~/Desktop/vdata1.RData")
ls()
opData = vdata1[vdata1$opLevel==2,]
opData[1:10,]

# Op 2
par(mfrow=c(2,1))
xmax = max(opData$latency_ms[opData$opType==2], op2Bin$latency_ms)
validationOp2 = hist(opData$latency_ms[opData$opType==2], breaks=25, xlim=c(0,xmax))
hist(op2Bin$latency_ms, breaks=25, xlim=c(0,xmax))


# Op 3
par(mfrow=c(2,1))
xmax = max(opData$latency_ms[opData$opType==3], op3Bin$latency_ms)
validationOp3 = hist(opData$latency_ms[opData$opType==3], breaks=25, xlim=c(0,xmax))
hist(op3Bin$latency_ms, breaks=25, xlim=c(0,xmax))


# Op 6
par(mfrow=c(2,1))
#xmax = max(opData$latency_ms[opData$opType==6], op6Bin$latency_ms)
xmax=1000
validationOp6 = hist(opData$latency_ms[opData$opType==6], breaks=25, xlim=c(0,xmax))
hist(op6Bin$latency_ms, breaks=200, xlim=c(0,xmax))


# Query
par(mfrow=c(2,1))
xmax = max(vdata1$latency_ms[vdata1$opLevel==3], samples)
hist(vdata1$latency_ms[vdata1$opLevel==3], breaks=25, xlim=c(0,xmax))
load("~/Desktop/userByEmailSamples/sample1.RData")
hist(samples, breaks=25, xlim=c(0,xmax))



# Look at query on EC2
pdf(file="~/Desktop/sampled-actualR-actualEC2-userByEmail.pdf")

queryData = as.data.frame(read.csv(file="~/Desktop/userByEmail-actual/query/out.csv"))

par(mar=c(5,5,4,2)+0.1)
par(mfrow=c(3,1))
xmax = max(vdata1$latency_ms[vdata1$opLevel==3], samples, queryData$latency_ms[queryData$threadNum > 50])
hist(samples, breaks=25, xlim=c(0,xmax), main="Sampled latency", xlab="Latency (ms)")
abline(v=quantile(samples, 0.99), col="red", lw=2)
legend("topright", legend=(paste("99th Percentile =", round(quantile(samples, 0.99)), "ms")), lwd=2, col="red")

hist(vdata1$latency_ms[vdata1$opLevel==3], breaks=25, xlim=c(0,xmax), main="Actual (R Cluster)", xlab="Latency (ms)")
abline(v=quantile(vdata1$latency_ms[vdata1$opLevel==3], 0.99), col="red", lw=2)
legend("topright", legend=(paste("99th Percentile =", round(quantile(vdata1$latency_ms[vdata1$opLevel==3], 0.99)), "ms")), lwd=2, col="red")

error = abs(actual99th-sampled99th)/actual99th
hist(queryData$latency_ms[queryData$threadNum > 50], breaks=25, xlim=c(0,xmax), main="Actual (EC2)", xlab="Latency (ms)")
abline(v=quantile(queryData$latency_ms[queryData$threadNum > 50], 0.99), col="red", lw=2)
legend("topright", legend=(paste("99th Percentile =", round(quantile(queryData$latency_ms[queryData$threadNum > 50], 0.99)), "ms, error =", round(error, digits=2))), lwd=2, col="red")

actual99th=quantile(queryData$latency_ms[queryData$threadNum > 50], 0.99)
sampled99th=quantile(samples, 0.99)

dev.off()

## COMPARING OPERATOR HISTS -- actual & training
# Operator hists (training data)
rm(list=ls())
load("~/Desktop/op2-hists/i=10,j=10,k=10,l=10.RData")
ls()
h2 = h
op2Bin = bin
op2Bin[1:10,]

load("~/Desktop/op3-hists/i=10,j=10,k=10,l=10.RData")
h3 = h
op3Bin = bin
ls()

load("~/Desktop/op6-hists/i=10,j=10,k=10,l=10.RData")
h6 = h
op6Bin = bin
ls()

par(mfrow=c(3,1))
hist(op2Bin$latency_ms, breaks=25)
hist(op3Bin$latency_ms, breaks=25)  # gamma will still work well here (like k=2 example on wikipedia)
hist(op6Bin$latency_ms[op6Bin$latency_ms < 5], breaks=25)




# Validation data
vOps = as.data.frame(read.csv("~/Desktop/userByEmail/operator/ops.csv"))
vOps[1:10,]
dim(vOps)

vOpsRun = vOps[vOps$threadNum > 50,]
dim(vOpsRun)

pdf("~/Desktop/ops-training-and-actual.pdf")

par(mfrow=c(3,2))
par(mar=c(5,5,4,2)+0.1)
xmax = max(op2Bin$latency_ms, vOpsRun$latency_ms[vOpsRun$opType==2])
hist(op2Bin$latency_ms, breaks=25, xlim=c(0,xmax), main="Op2-Training", xlab="Latency (ms)")
hist(vOpsRun$latency_ms[vOpsRun$opType==2], breaks=50, xlim=c(0,xmax), main="Op2-Actual", xlab="Latency (ms)")

xmax = max(op3Bin$latency_ms, vOpsRun$latency_ms[vOpsRun$opType==3])
hist(op3Bin$latency_ms, breaks=25, xlim=c(0,xmax), main="Op3-Training", xlab="Latency (ms)")
hist(vOpsRun$latency_ms[vOpsRun$opType==3], breaks=50, xlim=c(0,xmax), main="Op3-Actual", xlab="Latency (ms)")

hist(op6Bin$latency_ms[op6Bin$latency_ms < 5], breaks=25, main="Op6-Training", xlab="Latency (ms)")
hist(vOpsRun$latency_ms[vOpsRun$opType==6 & vOpsRun$latency_ms < 5], breaks=25, main="Op6-Actual", xlab="Latency (ms)")

dev.off()


# Try bigger data sz for op3 b/c the scadr users' email field is more like 20 chars
load("~/Desktop/op3-hists/i=40,j=10,k=10,l=10.RData")
h3.40 = h
op3Bin.40 = bin
ls()

par(mfrow=c(2,1))
hist(op3Bin$latency_ms)
hist(op3Bin.40$latency_ms)
# almost same



## 5.12.10
## Using op csv from run with only 5 threads


rm(list=ls())
op3 = as.data.frame(read.csv(file="~/Desktop/op3ops.csv"))
dim(op3)
op3[1:10,]

hist(op3$latency_ms, breaks=50, xlim=c(0,100))

# looking at op3 from uBE w/ only 5 threads (skip warmup)
rm(list=ls())
uBEops = as.data.frame(read.csv(file="~/Desktop/uBEops.csv"))
dim(uBEops)
uBEops[1:10,]

range(uBEops$threadNum)

uBErun = uBEops[uBEops$threadNum >= 5,]
dim(uBErun)
range(uBErun$threadNum)

unique(uBErun$opType)

uBEop3 = uBErun[uBErun$opType==3,]
dim(uBEop3)

hist(uBEop3$latency_ms)


# compare micro, macro bm op3
op3 = as.data.frame(read.csv(file="~/Desktop/op3ops.csv"))

pdf(file="~/Desktop/op3-5threads.pdf")

par(mfrow=c(2,1))
par(mar=c(5,5,4,2)+0.1)

xmax=100
op3onebin = op3[op3$aSize==10 & op3$bSize==10 & op3$numA==10 & op3$numB==10,]
dim(op3onebin)
dim(op3)

unique(op3$aSize)
unique(op3$bSize)
unique(op3$numA)
unique(op3$numB)


#hist(op3$latency_ms, breaks=50, xlim=c(0,xmax), xlab="Latency (ms)", main="op3 Latency Histogram (microbenchmark)")
#microMedian = median(op3$latency_ms)
#micro90th = quantile(op3$latency_ms, 0.9)
#abline(v= microMedian, col="red", lw=2)
#abline(v=micro90th, col="blue", lw=2)
#legend("topright", legend=c(paste("median=", microMedian, "ms"), paste("90th=", micro90th, "ms")), lwd=2, col=c("red", "blue"))


hist(op3onebin$latency_ms, breaks=50, xlim=c(0,xmax), xlab="Latency (ms)", main="op3 Latency Histogram (microbenchmark)")
microMedian = median(op3onebin$latency_ms)
micro90th = quantile(op3onebin$latency_ms, 0.9)
abline(v= microMedian, col="red", lw=2)
abline(v=micro90th, col="blue", lw=2)
legend("topright", legend=c(paste("median=", microMedian, "ms"), paste("90th=", micro90th, "ms")), lwd=2, col=c("red", "blue"))


hist(uBEop3$latency_ms, breaks=100, xlim=c(0,xmax), xlab="Latency (ms)", main="op3 Latency Histogram (macrobenchmark - userByEmail)")
macroMedian = median(uBEop3$latency_ms)
macro90th = quantile(uBEop3$latency_ms, 0.9)
abline(v=macroMedian, col="red", lw=2)
abline(v=macro90th, col="blue", lw=2)
legend("topright", legend=c(paste("median=", macroMedian, "ms"), paste("90th=", macro90th, "ms")), lwd=2, col=c("red", "blue"))

dev.off()

# look at the overall query latency
uBEqueries = as.data.frame(read.csv("~/Desktop/uBEqueries.csv"))
dim(uBEqueries)
uBEqueries[1:10,]

uBEqueriesRun = uBEqueries[uBEqueries$threadNum >= 5,]
dim(uBEqueriesRun)
uBEqueriesRun[1:10,]


hist(uBEqueriesRun$latency_ms, xlim=c(0,100), breaks=100)
abline(v=quantile(uBEqueriesRun$latency_ms, 0.99), lw=2, col="green")
legend("topright", legend=paste("99th=", quantile(uBEqueriesRun$latency_ms, 0.99), "ms"), col="green", lwd=2)



# try using the microbenchmark ops to predict the query's latency distr

op2 = as.data.frame(read.csv(file="~/Desktop/op2/op2ops.csv"))
dim(op2)
op2[1:10,]

op2onebin = op2[op2$aSize==10 & op2$bSize==10 & op2$numA==10 & op2$numB==10,]
dim(op2onebin)

op6 = as.data.frame(read.csv(file="~/Desktop/op6/op6ops.csv"))
dim(op6)
op6[1:10,]

op6onebin = op6[op6$aSize==10 & op6$bSize==10 & op6$numA==10 & op6$numB==10,]
dim(op6onebin)


h2 = hist(op2onebin$latency_ms, breaks=50)
h3 = hist(op3onebin$latency_ms, breaks=50)
h6 = hist(op6onebin$latency_ms, breaks=50)



userByEmailSampler = function(h2, h3, h6, sampleID, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sample(h2$mids, 1, replace=TRUE, prob=h2$density)
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sample(h6$mids, 1, replace=TRUE, prob=h6$density)
	}

	save(samples, file=paste("~/Desktop/userByEmailSamples/sample", sampleID,".RData",sep=""))

	return(samples)
}


numSamples = 1000
for (i in 1:10) {
	samples = userByEmailSampler(h2, h3, h6, i, numSamples)
	print(quantile(samples, 0.99))
}

load("~/Desktop/userByEmailSamples/sample1.RData")
ls()



estimates=c(46, 56, 56, 41.05, 51, 51, 41, 51, 46, 46)
median(estimates)

actual=55.8
error = abs(actual - median(estimates))/actual
error



par(mfrow=c(2,1))
xmax=100
hist(uBEqueriesRun$latency_ms, xlim=c(0,xmax), breaks=200)
abline(v=quantile(uBEqueriesRun$latency_ms, 0.99), lw=2, col="green")
legend("topright", legend=paste("99th=", quantile(uBEqueriesRun$latency_ms, 0.99), "ms"), col="green", lwd=2)

hist(samples, breaks=100, xlim=c(0,xmax))
abline(v=quantile(samples, 0.99), lw=2, col="green")
legend("topright", legend=paste("99th=", quantile(samples, 0.99), "ms"), col="green", lwd=2)



## 5.13.10
## plotting med latency for op3 vs. # threads

medianLatency = matrix(nrow=1, ncol=10)

for (i in 1:10) {
	print(i)
	if (i != 6) {
		op3 = as.data.frame(read.csv(paste("~/", i*5, "/op3ops.csv", sep="")))
		medianLatency[i] = median(op3$latency_ms)
	}
}

medianLatency

medianLatency = c(5.032, 7.677, 13.6625, 23.094, 32.51, NA, 56.474, 66.397, 73.07, 91.276)
numThreads = seq(from=5, to=50, by=5)

par(mar=c(5,5,4,2)+0.1)
plot(numThreads, medianLatency, xlab="# client threads", ylab="Median latency (ms)", main="Op3 Median Latency vs. # of Client Threads")




## 5.14.10
# Trying to predict latency of my 4 query types
op2 = as.data.frame(read.csv("~/op2/op2ops.csv"))
dim(op2)
op2[1:10,]

op3 = as.data.frame(read.csv("~/op3/op3ops.csv"))
dim(op3)
op3[1:10,]

op6 = as.data.frame(read.csv("~/op6/op6ops.csv"))
dim(op6)
op6[1:10,]



h2 = hist(op2$latency_ms, breaks=50)
h3 = hist(op3$latency_ms, breaks=50)
h6 = hist(op6$latency_ms, breaks=50)



userByEmailSampler = function(h2, h3, h6, sampleID, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sample(h2$mids, 1, replace=TRUE, prob=h2$density)
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sample(h6$mids, 1, replace=TRUE, prob=h6$density)
	}

	#save(samples, file=paste("~/Desktop/userByEmailSamples/sample", sampleID,".RData",sep=""))

	return(samples)
}


numSamples = 10000
quantiles = matrix(nrow=1, ncol=10)
for (i in 1:10) {
	samples = userByEmailSampler(h2, h3, h6, i, numSamples)
	#print(quantile(samples, 0.99))
	quantiles[i] = quantile(samples, 0.99)
}
quantiles


# actual
query = as.data.frame(read.csv("~/userByEmail/queries.csv"))
dim(query)
query[1:10,]

quantile(query$latency_ms,0.99)
save(list=ls(), file="~/userByEmail.RData")



load(file="~/Desktop/userByEmail.RData")
ls()

pdf(file="~/Desktop/MSReport/MSFigs/userByHometown.pdf")

par(mfrow=c(2,1))
par(mar=c(5,5,4,2)+0.1)
xmax=100
hist(query$latency_ms, breaks=100, xlim=c(0,xmax), xlab="Latency (ms)", main="userByHometown: latency distribution, actual")
abline(v=median(query$latency_ms), col="red", lw=2)
abline(v=quantile(query$latency_ms,0.9), col="green", lw=2)
abline(v=quantile(query$latency_ms,0.99), col="blue", lw=2)
legend("topright", legend=c(paste("median=", round(median(query$latency_ms)), "ms"), paste("90th=", round(quantile(query$latency_ms, 0.9)), "ms"), paste("99th=", round(quantile(query$latency_ms, 0.99)), "ms")), lwd=2, col=c("red", "green", "blue"))

hist(samples, breaks=100, xlim=c(0,xmax), xlab="Latency (ms)", main="userByHometown: latency distribution, predicted")
abline(v=median(samples), col="red", lw=2)
abline(v=quantile(samples,0.9), col="green", lw=2)
abline(v=quantile(samples,0.99), col="blue", lw=2)
legend("topright", legend=c(paste("median=", round(median(samples)), "ms"), paste("90th=", round(quantile(samples, 0.9)), "ms"), paste("99th=", round(quantile(samples, 0.99)), "ms")), lwd=2, col=c("red", "green", "blue"))

dev.off()

numSamples = 10000
quantiles = matrix(nrow=1, ncol=10)
for (i in 1:10) {
	samples = userByEmailSampler(h2, h3, h6, i, numSamples)
	#print(quantile(samples, 0.99))
	quantiles[i] = quantile(samples, 0.99)
}
quantiles


ls()

h2 = hist(op2$latency_ms, breaks=100)
h3 = hist(op3$latency_ms, breaks=100)
h6 = hist(op6$latency_ms, breaks=100)

## trying out the gamma... not too good yet

gammaParams = function(op) {
	s = log(1/(nrow(op)) * sum(op$latency_ms)) - 1/nrow(op) * sum(log(op$latency_ms))
	k = (3 - s + sqrt((s-3)^2 + 24*s))/(12*s)
	theta = 1/(k*nrow(op)) * sum(op$latency_ms)
	
	return(c(k,theta))
}


op2gammaParams = gammaParams(op2)
op3gammaParams = gammaParams(op3)
op6gammaParams = gammaParams(op6)




userByEmailGammaSampler = function(op2gammaParams, op3gammaParams, op6gammaParams, sampleID, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		# shape=k, scale=theta
		samples[i] = samples[i] + rgamma(1, shape=op2gammaParams[1], scale=op2gammaParams[2])
		samples[i] = samples[i] + rgamma(1, shape=op3gammaParams[1], scale=op3gammaParams[2])
		samples[i] = samples[i] + rgamma(1, shape=op6gammaParams[1], scale=op6gammaParams[2])
	}

	return(samples)
}

numSamples=1000
gammaSamples = userByEmailGammaSampler(op2gammaParams, op3gammaParams, op6gammaParams, 1, numSamples)

par(mfrow=c(2,1))
par(mar=c(5,5,4,2)+0.1)
xmax=100
hist(gammaSamples, breaks=25, xlim=c(0,xmax), xlab="Latency (ms)", main="userByEmail: latency distribution, predicted")
abline(v=median(gammaSamples), col="red", lw=2)
abline(v=quantile(gammaSamples,0.9), col="green", lw=2)
abline(v=quantile(gammaSamples,0.99), col="blue", lw=2)
legend("topright", legend=c(paste("median=", round(median(gammaSamples)), "ms"), paste("90th=", round(quantile(gammaSamples, 0.9)), "ms"), paste("99th=", round(quantile(gammaSamples, 0.99)), "ms")), lwd=2, col=c("red", "green", "blue"))

hist(query$latency_ms, breaks=100, xlim=c(0,xmax), xlab="Latency (ms)", main="userByEmail: latency distribution, actual")
abline(v=median(query$latency_ms), col="red", lw=2)
abline(v=quantile(query$latency_ms,0.9), col="green", lw=2)
abline(v=quantile(query$latency_ms,0.99), col="blue", lw=2)
legend("topright", legend=c(paste("median=", round(median(query$latency_ms)), "ms"), paste("90th=", round(quantile(query$latency_ms, 0.9)), "ms"), paste("99th=", round(quantile(query$latency_ms, 0.99)), "ms")), lwd=2, col=c("red", "green", "blue"))




# thoughts by hash tag

rm(list=ls())

query = as.data.frame(read.csv("~/thoughtsByHashTag/queries.csv"))
dim(query)

op2 = as.data.frame(read.csv("~/op2/op2ops.csv"))
dim(op2)
op2[1:10,]

op5 = as.data.frame(read.csv("~/op5/op5ops.csv"))
dim(op5)
op5[1:10,]

op6 = as.data.frame(read.csv("~/op6/op6ops.csv"))
dim(op6)
op6[1:10,]

op8 = as.data.frame(read.csv("~/op8/op8ops.csv"))
dim(op8)
op8[1:10,]


h2 = hist(op2$latency_ms, breaks=100)
h5 = hist(op5$latency_ms, breaks=100)
h6 = hist(op6$latency_ms, breaks=100)
h8 = hist(op8$latency_ms, breaks=100)


thoughtsByHashTagSampler = function(h2, h5, h6, h8, sampleID, numSamples) {
	samples=matrix(data=0, nrow=1, ncol=numSamples)

	for (i in 1:numSamples) {
		samples[i] = samples[i] + sample(h2$mids, 1, replace=TRUE, prob=h2$density)
		samples[i] = samples[i] + sample(h5$mids, 1, replace=TRUE, prob=h5$density)
		samples[i] = samples[i] + sum(sample(h6$mids, 2, replace=TRUE, prob=h6$density))
		samples[i] = samples[i] + sample(h8$mids, 1, replace=TRUE, prob=h8$density)
	}

	return(samples)
}


numSamples = 5000
quantiles = matrix(nrow=1, ncol=10)
for (i in 1:10) {
	samples = thoughtsByHashTagSampler(h2, h5, h6, h8, i, numSamples)
	#print(quantile(samples, 0.99))
	quantiles[i] = quantile(samples, 0.99)
}
quantiles


save(list=ls(), file="~/thoughtsByHashTag.RData")
rm(list=ls())
load(file="~/Desktop/thoughtsByHashTag.RData")


par(mfrow=c(2,1))
par(mar=c(5,5,4,2)+0.1)
xmax=200
hist(samples, breaks=50, xlim=c(0,xmax), xlab="Latency (ms)", main="thoughtsByHashTag: latency distribution, predicted")
abline(v=median(samples), col="red", lw=2)
abline(v=quantile(samples,0.9), col="green", lw=2)
abline(v=quantile(samples,0.99), col="blue", lw=2)
legend("topright", legend=c(paste("median=", round(median(samples)), "ms"), paste("90th=", round(quantile(samples, 0.9)), "ms"), paste("99th=", round(quantile(samples, 0.99)), "ms")), lwd=2, col=c("red", "green", "blue"))

hist(query$latency_ms, breaks=100, xlim=c(0,xmax), xlab="Latency (ms)", main="thoughtsByHashTag: latency distribution, actual")
abline(v=median(query$latency_ms), col="red", lw=2)
abline(v=quantile(query$latency_ms,0.9), col="green", lw=2)
abline(v=quantile(query$latency_ms,0.99), col="blue", lw=2)
legend("topright", legend=c(paste("median=", round(median(query$latency_ms)), "ms"), paste("90th=", round(quantile(query$latency_ms, 0.9)), "ms"), paste("99th=", round(quantile(query$latency_ms, 0.99)), "ms")), lwd=2, col=c("red", "green", "blue"))



## looking at the ops for thoughtsByHashTag
ls()
ops = as.data.frame(read.csv("~/Desktop/thoughtsByHashTag-ops.csv"))
dim(ops)
ops[1:10,]

unique(ops$opType)

# op2 -- really off
par(mfrow=c(2,1))
xmax=20
hist(ops$latency_ms[ops$opType==2], breaks=500, xlim=c(0,xmax), main="actual")
hist(op2$latency_ms, breaks=250, xlim=c(0,xmax), main="micro")


# op5 -- really off
par(mfrow=c(2,1))
xmax=100
hist(ops$latency_ms[ops$opType==5], breaks=100, xlim=c(0,xmax), main="actual")
hist(op5$latency_ms, breaks=50, xlim=c(0,xmax), main="micro")


# op6 -- really off
par(mfrow=c(2,1))
xmax=4
hist(ops$latency_ms[ops$opType==6], breaks=2000, xlim=c(0,xmax), main="actual")
hist(op6$latency_ms, breaks=20, xlim=c(0,xmax), main="micro")


# op8 -- ok (so short, doesn't make much difference)
par(mfrow=c(2,1))
xmax=2
hist(ops$latency_ms[ops$opType==8], breaks=500, xlim=c(0,xmax), main="actual")
hist(op8$latency_ms, breaks=50, xlim=c(0,xmax), main="micro")
