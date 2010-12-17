# 7.21.10
# figure out how to use convolution for combining the op distr => query distr

# start with a simple example
# obtained from http://www.ece.unm.edu/signals/signals/Discrete_Convolution/discrete_convolution.html
?convolve

x = c(1,1,1,1,1)
h = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1)
out = convolve(x, h, type="open")

?plot
plot(out, type="h", ylim=c(0,6))


# figure out how to control points in density
rm(list=ls())
load("~/Desktop/ops.RData")

?density

d3 = density(op3$latency_ms)
plot(d3)
plot(d3, xlim=c(0,50))

names(d3)
d3$n

# try w/o aligning
d2 = density(op2$latency_ms)
d6 = density(op6$latency_ms)

conv = convolve(d2$y, d3$y, type="open")
convUBH = convolve(conv, d6$y, type="open")

plot(convUBH)
# definitely doesn't work!

# try to align
d2$x[1:10]
d3$x[1:10]
d6$x[1:10]

from=min(op2$latency_ms, op3$latency_ms, op6$latency_ms)
to=max(op2$latency_ms, op3$latency_ms, op6$latency_ms)

d2.aligned = density(op2$latency_ms, from=from, to=to, n=20000)
d3.aligned = density(op3$latency_ms, from=from, to=to, n=20000)
d6.aligned = density(op6$latency_ms, from=from, to=to, n=20000)

# compare aligned, unaligned versions
d2$n
d2.aligned$n
d2$x[1:10]
d2.aligned$x[1:10]
length(d2.aligned$x)
range(d2$x)
range(d2.aligned$x)

d3$n
d3.aligned$n
d3.aligned$x[1:10]
length(d3.aligned$x)
range(d3$x)
range(d3.aligned$x)

d6$n
d6.aligned$n
d6.aligned$x[1:10]
length(d6.aligned$x)
length(d6.aligned$y)
range(d6$x)
range(d6.aligned$x)

conv.aligned = convolve(d2.aligned$y, d3.aligned$y, type="circular", conj=FALSE)
plot(conv.aligned)
length(conv.aligned)

convUBH.aligned = convolve(conv.aligned, d6.aligned$y, type="circular", conj=FALSE)
dim(convUBH.aligned)
length(convUBH.aligned)
plot(convUBH.aligned, type="l")


# compare convolution to real uBH
load("~/Desktop/queryObs.RData")
par(mfrow=c(2,1))
dUBH=density(uBHqueries$latency_ms)
plot(density(uBHqueries$latency_ms), xlim=c(0,300))
plot(d2.aligned$x, convUBH.aligned/normalization, type="l")

normalization=sum(rep.int(d$x[2]-d$x[1],times=length(d$x))*convUBH.aligned)

convDensityUBH = list(x=d2.aligned$x, y=convUBH.aligned/normalization)

?quantile
quantile(dUBH, 0.99)

# check to make sure I aligned them correctly
par(mfrow=c(3,1))
plot(d2.aligned)
plot(d3.aligned)
plot(d6.aligned)

# try to compute the quantile from the density

# need dot product
x = c(1,3,-5)
y = c(4,-2,-1)

sum(x*y)


totalArea = sum(d2.aligned$x*d2.aligned$y)

maxBin=length(d2.aligned$x)
bin=4000
leftOfBin = sum(d2.aligned$x[1:bin]*d2.aligned$y[1:bin])
rightOfBin = sum(d2.aligned$x[(bin+1):maxBin]*d2.aligned$y[(bin+1):maxBin])

quantile=leftOfBin/totalArea
d2.aligned$x[bin]



d=d2.aligned

totalArea = sum(rep.int(d$x[2]-d$x[1],times=length(d$x))*d$y)

quantiles = vector(length=maxBin)

for (bin in 1:maxBin) {
	#leftOfBin = sum(d$x[1:bin]*d$y[1:bin])
	leftOfBin = sum(rep.int(d$x[2]-d$x[1],times=bin)*d$y[1:bin])
	#rightOfBin = sum(d$x[(bin+1):maxBin]*d$y[(bin+1):maxBin])
	
	quantiles[bin] = leftOfBin/totalArea
}

summary(quantiles)

plot(d$x, quantiles, type="l")
abline(h=0.99)

diffFromDesiredQuantile = abs(quantiles - 0.99)
plot(diffFromDesiredQuantile)
bestBin=which.min(diffFromDesiredQuantile)
return(d$x[bestBin])

quantiles[15000]
op2.99th=quantile(op2$latency_ms, 0.99)
summary(op2$latency_ms)
length(op2$latency_ms)

# find bin that's closest to real 99th %ile
binDiff = abs(d$x - op2.99th)
which.min(binDiff)
plot(binDiff)

diffFromDesiredQuantile[1712]


plot(ecdf(op2$latency_ms))


getQuantileFromDensity(d2.aligned, 0.99)
getQuantileFromDensity(dUBH, 0.99)

getQuantileFromDensity(convDensityUBH, 0.99)
quantile(uBHqueries$latency_ms,0.99)


# check getUserByHometownDensityViaConvolution function

ubhConvDensity = getQueryDensityViaConvolution("~/Desktop/ops.RData", "userByHometown", 20000)

path="~/Desktop/convPlots"
dir.create(path)

pdf(file=paste(path, "/ubh.pdf", sep=""))

par(mfrow=c(2,1))
plot(density(uBHqueries$latency_ms), xlab="Latency (ms)", ylab="Density", main="userByHometown actual")
abline(v=quantile(uBHqueries$latency_ms,0.99), lw=2, col="blue")
legend("topright", col="blue", legend=paste("99th = ", round(quantile(uBHqueries$latency_ms,0.99), digits=2), "ms", sep=""), lw=2)

plot(ubhConvDensity, type="l", xlab="Latency (ms)", ylab="Density", main="userByHometown prediction")
#ubh99th=getQuantileFromDensity(ubhConvDensity,0.99)
abline(v=ubh99th, lw=2, col="blue")
legend("topright", col="blue", legend=paste("99th = ", round(ubh99th, digits=2), "ms", sep=""), lw=2)
dev.off()

# check convolution for nA
nAConvDensity = getQueryDensityViaConvolution("~/Desktop/ops.RData", "needsApproval", 20000)
nA99th=getQuantileFromDensity(nAConvDensity,0.99)

pdf(file=paste(path, "/na.pdf", sep=""))
par(mfrow=c(2,1))
plot(density(nAqueries$latency_ms), xlab="Latency (ms)", ylab="Density", main="needsApproval actual")
abline(v=quantile(nAqueries$latency_ms,0.99), lw=2, col="blue")
legend("topright", col="blue", legend=paste("99th = ", round(quantile(nAqueries$latency_ms,0.99), digits=2), "ms", sep=""), lw=2)


plot(nAConvDensity, type="l", xlab="Latency (ms)", ylab="Density", main="needsApproval prediction")
abline(v=nA99th, lw=2, col="blue")
legend("topright", col="blue", legend=paste("99th = ", round(nA99th, digits=2), "ms", sep=""), lw=2)
dev.off()

# check conv for mF
mFConvDensity = getQueryDensityViaConvolution("~/Desktop/ops.RData", "myFollowing", 20000)
mF99th=getQuantileFromDensity(mFConvDensity,0.99)


pdf(file=paste(path, "/mf.pdf", sep=""))
par(mfrow=c(2,1))
plot(density(mFqueries$latency_ms), xlab="Latency (ms)", ylab="Density", main="myFollowing actual")
abline(v=quantile(mFqueries$latency_ms,0.99), lw=2, col="blue")
legend("topright", col="blue", legend=paste("99th = ", round(quantile(mFqueries$latency_ms,0.99), digits=2), "ms", sep=""), lw=2)


plot(mFConvDensity, type="l", xlab="Latency (ms)", ylab="Density", main="myFollowing prediction")
abline(v=mF99th, lw=2, col="blue")
legend("topright", col="blue", legend=paste("99th = ", round(mF99th, digits=2), "ms", sep=""), lw=2)

dev.off()

# check conv for mT
mTConvDensity = getQueryDensityViaConvolution("~/Desktop/ops.RData", "myThoughts", 20000)
mT99th=getQuantileFromDensity(mTConvDensity,0.99)

pdf(file=paste(path, "/mt.pdf", sep=""))
par(mfrow=c(2,1))
plot(density(mTqueries$latency_ms), xlab="Latency (ms)", ylab="Density", main="myThoughts actual")
abline(v=quantile(mTqueries$latency_ms,0.99), lw=2, col="blue")
legend("topright", col="blue", legend=paste("99th = ", round(quantile(mTqueries$latency_ms,0.99), digits=2), "ms", sep=""), lw=2)

plot(mTConvDensity, type="l", xlab="Latency (ms)", ylab="Density", main="myThoughts prediction")
abline(v=mT99th, lw=2, col="blue")
legend("topright", col="blue", legend=paste("99th = ", round(mT99th, digits=2), "ms", sep=""), lw=2)

dev.off()


