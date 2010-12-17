# Fitting analytical distributions to the op histograms
save(h1.v2, h2.v2, h3.v2, h4.v2, h5.v2, h6.v2, h7.v2, h8.v2, h9.v2, file="~/Desktop/hists.RData")

save(mFqueries, mFsamples.v2, mTqueries, mTsamples.v2, nAqueries, nAsamples.v2, uBHqueries, uBHsamples.v2, file="~/Desktop/queriesAndSamples.RData")

save(list=ls(), file="~/Desktop/retreat-data.RData")

rm(list=ls())

load("~/Desktop/hists.RData")
ls()

rm(list=ls())
load("~/Desktop/retreat-data.RData")
ls()
save(op1, op2, op3, op4, op5, op6, op7, op8, op9, file="~/Desktop/ops.RData")

load("~/Desktop/ops.RData")

plot(h1.v2, xlim=c(0,5), col="turquoise")

# fit a gamma to op
op=op3
#op$latency_ms = op$latency_ms - 0.99*min(op$latency_ms)
#op=op[which(op$latency_ms < quantile(op$latency_ms, 0.9)),]
op=op[which(op$latency_ms > 10),]
s = log(1/(nrow(op)) * sum(op$latency_ms)) - 1/nrow(op) * sum(log(op$latency_ms))
k = (3 - s + sqrt((s-3)^2 + 24*s))/(12*s)
theta = 1/(k*nrow(op)) * sum(op$latency_ms)
gammaSamples = rgamma(10000, shape=k, scale=theta)
k
theta

# fit a log-normal to op
mean.hat = sum(log(op$latency_ms))/nrow(op)

var.numerator = 0
for (i in 1:nrow(op)) {
	var.numerator = var.numerator + (log(op$latency_ms[i]) - mean.hat)^2
}
var.hat = var.numerator/nrow(op)

logNormalSamples = rlnorm(10000, meanlog=mean.hat, sdlog=sqrt(var.hat))

# fit a gamma to op by moment matching
library(moments)
skew = skewness(op$latency_ms)
k.mm = 4/(skew^2)
variance = var(op$latency_ms)
theta.mm = sqrt(variance/k.mm)
mu = mean(op$latency_ms)
shift = mu - k.mm*theta.mm 

gammaSamplesMomentMatching = rgamma(10000, shape=k.mm, scale=theta.mm) + shift

# plot
#pdf("~/Desktop/op.pdf")
par(mfrow=c(4,1), mar=c(5,5,4,2)+0.1)
#xmax=max(gammaSamples, logNormalSamples)
xmax=60
#plot(h1.v2, xlim=c(0,xmax), col="turquoise")
hist(op$latency_ms, xlim=c(0,xmax), breaks=2000, col="turquoise")
#hist(gammaSamples)
hist(gammaSamples, breaks=100, xlim=c(0,xmax), col="turquoise")
hist(logNormalSamples, breaks=100, xlim=c(0,xmax), col="turquoise")
hist(gammaSamplesMomentMatching, breaks=2000, xlim=c(0,xmax), col="turquoise")
#dev.off()


# Compare gamma params derived by param estimation on wp, moment matching
library(moments)
gammaParams = matrix(nrow=9, ncol=4)
colnames(gammaParams) = c("k", "theta", "k.mm", "theta.mm")
for (i in 1:9) {
	if (i == 1) {
		op = op1
	} else if (i == 2) {
		op = op2
	} else if (i == 3) {
		op = op3
	} else if (i == 4) {
		op = op4
	} else if (i == 5) {
		op = op5
	} else if (i == 6) {
		op = op6
	} else if (i == 7) {
		op = op7
	} else if (i == 8) {
		op = op8
	} else if (i == 9) {
		op = op9
	}
		
	s = log(1/(nrow(op)) * sum(op$latency_ms)) - 1/nrow(op) * sum(log(op$latency_ms))
	k = (3 - s + sqrt((s-3)^2 + 24*s))/(12*s)
	theta = 1/(k*nrow(op)) * sum(op$latency_ms)

	skew = skewness(op$latency_ms)
	k.mm = 4/(skew^2)
	variance = var(op$latency_ms)
	theta.mm = sqrt(variance/k.mm)
	
	gammaParams[i,"k"] = k
	gammaParams[i,"theta"] = theta
	gammaParams[i,"k.mm"] = k.mm
	gammaParams[i,"theta.mm"] = theta.mm
}