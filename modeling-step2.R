# Try to fit an analytical distribution to the operator hists
#... kinda morphed to trying to get insight into why the 

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


# trying to fit a gamma to op2Bin$latency_ms
k=1
theta = 1/(k*nrow(op2Bin)) * sum(op2Bin$latency_ms)

par(mfrow=c(2,1))
gammaSamples = rgamma(10000, shape=1, scale=theta)
xmax = max(op2Bin$latency_ms, gammaSamples)
hist(op2Bin$latency_ms, breaks=25, xlim=c(0,xmax))
hist(gammaSamples, breaks=15, xlim=c(0,xmax))

# matches well, but how to set k?
# use initial value from wikipedia -- w/in 1.5% of correct value
s = log(1/(nrow(op2Bin)) * sum(op2Bin$latency_ms)) - 1/nrow(op2Bin) * sum(log(op2Bin$latency_ms))
k = (3 - s + sqrt((s-3)^2 + 24*s))/(12*s)

theta = 1/(k*nrow(op2Bin)) * sum(op2Bin$latency_ms)

par(mfrow=c(2,1))
gammaSamples = rgamma(10000, shape=k, scale=theta)
xmax = max(op2Bin$latency_ms, gammaSamples)
hist(op2Bin$latency_ms, breaks=25, xlim=c(0,xmax))
hist(gammaSamples, breaks=15, xlim=c(0,xmax))

# perfect!  

# now try op3
opBin = op3Bin
s = log(1/(nrow(opBin)) * sum(opBin$latency_ms)) - 1/nrow(opBin) * sum(log(opBin$latency_ms))
k = (3 - s + sqrt((s-3)^2 + 24*s))/(12*s)

theta = 1/(k*nrow(opBin)) * sum(opBin$latency_ms)




par(mfrow=c(2,1))
gammaSamples = rgamma(10000, shape=k, scale=theta)
xmax = max(opBin$latency_ms, gammaSamples)
hist(opBin$latency_ms, breaks=25, xlim=c(0,xmax))
abline(v=median(opBin$latency_ms), col="green", lw=2)
hist(gammaSamples, breaks=25, xlim=c(0,xmax))
abline(v=median(gammaSamples), col="green", lw=2)







