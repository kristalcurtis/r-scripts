# making figs for MS report

load("~/Desktop/ops.RData")

pdf("~/Desktop/op-densities.pdf")

par(mfrow=c(3,3), mar=c(5,5,5,4)+0.1)

plot(density(op1$latency_ms, bw="nrd"), xlim=c(min(op1$latency_ms),1.1*quantile(op1$latency_ms, 0.99)), xlab="Latency (ms)", ylab="Density", main="op1: singleGet")
plot(density(op2$latency_ms, bw="nrd"), xlim=c(min(op2$latency_ms),1.1*quantile(op2$latency_ms, 0.99)), xlab="Latency (ms)", ylab="Density", main="op2: prefixGet")
plot(density(op3$latency_ms, bw="nrd"), xlim=c(min(op3$latency_ms),1.1*quantile(op3$latency_ms, 0.99)), xlab="Latency (ms)", ylab="Density", main="op3: sequentialDereferenceIndex")
plot(density(op4$latency_ms, bw="nrd"), xlim=c(min(op4$latency_ms),1.1*quantile(op4$latency_ms, 0.99)), xlab="Latency (ms)", ylab="Density", main="op4: prefixJoin")
plot(density(op5$latency_ms, bw="nrd"), xlim=c(min(op5$latency_ms),1.1*quantile(op5$latency_ms, 0.99)), xlab="Latency (ms)", ylab="Density", main="op5: pointerJoin")
plot(density(op6$latency_ms, bw="nrd"), xlim=c(min(op6$latency_ms),5*quantile(op6$latency_ms, 0.99)), xlab="Latency (ms)", ylab="Density", main="op6: materialize")
plot(density(op7$latency_ms, bw="nrd"), xlim=c(min(op7$latency_ms),1.1*quantile(op7$latency_ms, 0.99)), xlab="Latency (ms)", ylab="Density", main="op7: selection")
plot(density(op8$latency_ms, bw="nrd"), xlim=c(min(op8$latency_ms),5*quantile(op8$latency_ms, 0.99)), xlab="Latency (ms)", ylab="Density", main="op8: sort")
plot(density(op9$latency_ms, bw="nrd"), xlim=c(min(op9$latency_ms),50*quantile(op9$latency_ms, 0.99)), xlab="Latency (ms)", ylab="Density", main="op9: topK")


dev.off()




# show how 99th %ile changes when using raw data, hist, or density

op99th = matrix(nrow=9, ncol=3)
colnames(op99th) = c("actual", "hist", "density")

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
	} else {
		print("error")
	}

	op99th[i,1] = quantile(op$latency_ms, 0.99)

	h = hist(op$latency_ms, breaks=1000)
	histDensity = list(x=h$mids, y=h$density)
	op99th[i,2] = getQuantileFromDensity(histDensity,0.99)

	op99th[i,3] = getQuantileFromDensity(density(op$latency_ms, bw="nrd"),0.99)
}





# show how # bins impacts 99th %ile latency
bins = c(10,100,1000,10000,100000)
op3.99th = vector(length=length(bins))

op=op3

for (i in 1:length(bins)) {
	print(i)
	h = hist(op$latency_ms, breaks=bins[i], plot=FALSE)
	histDensity = list(x=h$mids, y=h$density)
	op3.99th[i] = getQuantileFromDensity(histDensity,0.99)
}


op3.99th

pdf("~/Desktop/choose-bins.pdf")
par(mar=c(5,5,4,2)+0.1,cex.axis=1.5, cex.lab=1.2, cex.main=1.5)
plot(bins, op3.99th, log="x", ylim=c(0,65), col="blue", xlab="Number of Bins", ylab="99th-Percentile Latency (ms)", main="op3 99th-Percentile Latency vs. Number of Bins")
lines(bins, op3.99th, col="blue", lw=2, log="x")
abline(h=quantile(op3$latency_ms,0.99), col="red", lw=2)
legend("bottomright", legend=c("actual", "predicted"), lw=2, col=c("red", "blue"), cex=1.2)
dev.off()


# redoing op3 single distr plot
from=min(op1$latency_ms, op2$latency_ms, op3$latency_ms, op4$latency_ms, op5$latency_ms, op6$latency_ms, op7$latency_ms, op8$latency_ms, op9$latency_ms)
to=max(op1$latency_ms, op2$latency_ms, op3$latency_ms, op4$latency_ms, op5$latency_ms, op6$latency_ms, op7$latency_ms, op8$latency_ms, op9$latency_ms)
length.out=20000
densityDomain=seq(from=from, to=to, length.out=length.out)
length(densityDomain)


plotOpSingleDistrFitsUsingDensity(op3$latency_ms, 3, "~/Desktop", densityDomain)
# shelved for now - hard to do cdf


# make plot like single op plot for MM fits
# fair b/c both are using sampling rather than density
plotOpMMFits(op3$latency_ms, 3, c(3,4,6,5), "~/Desktop")


# fix cv plot -- need bigger text

plotCVLogLikelihoodVsNumMixtureComponents("~/Desktop/8.3.10-cv/op3Weibull", seq(from=2,to=9,by=1))


# make query pred plots
source("convolutionForOpCombination-functions.R")
load("~/Desktop/queryObs.RData")
load("~/Desktop/ops.RData")
load("~/Desktop/opsList.RData")

h1 = hist(op1$latency_ms, plot=FALSE, breaks=1000)
h2 = hist(op2$latency_ms, plot=FALSE, breaks=1000)
h3 = hist(op3$latency_ms, plot=FALSE, breaks=1000)
h4 = hist(op4$latency_ms, plot=FALSE, breaks=1000)
h5 = hist(op5$latency_ms, plot=FALSE, breaks=1000)
h6 = hist(op6$latency_ms, plot=FALSE, breaks=1000)
h7 = hist(op7$latency_ms, plot=FALSE, breaks=1000)
h8 = hist(op8$latency_ms, plot=FALSE, breaks=1000)
h9 = hist(op9$latency_ms, plot=FALSE, breaks=1000)

h1.density = list(x=h1$mids, y=h1$density)
h2.density = list(x=h2$mids, y=h2$density)
h3.density = list(x=h3$mids, y=h3$density)
h4.density = list(x=h4$mids, y=h4$density)
h5.density = list(x=h5$mids, y=h5$density)
h6.density = list(x=h6$mids, y=h6$density)
h7.density = list(x=h7$mids, y=h7$density)
h8.density = list(x=h8$mids, y=h8$density)
h9.density = list(x=h9$mids, y=h9$density)

opDensityList.hist = list(d1=h1.density, d2=h2.density, d3=h3.density, d4=h4.density, d5=h5.density, d6=h6.density, d7=h7.density, d8=h8.density, d9=h9.density)

source("useOpDensitiesForQueryDensityPrediction.R")

from=min(op1$latency_ms, op2$latency_ms, op3$latency_ms, op4$latency_ms, op5$latency_ms, op6$latency_ms, op7$latency_ms, op8$latency_ms, op9$latency_ms)
to=max(op1$latency_ms, op2$latency_ms, op3$latency_ms, op4$latency_ms, op5$latency_ms, op6$latency_ms, op7$latency_ms, op8$latency_ms, op9$latency_ms)
length.out=20000
densityDomain=seq(from=from, to=to, length.out=length.out)
length(densityDomain)

d1.exp = getPredictedOpDensityUsingSingleDistr(op1$latency_ms, "exponential", densityDomain)
d2.exp = getPredictedOpDensityUsingSingleDistr(op2$latency_ms, "exponential", densityDomain)
d3.exp = getPredictedOpDensityUsingSingleDistr(op3$latency_ms, "exponential", densityDomain)
d4.exp = getPredictedOpDensityUsingSingleDistr(op4$latency_ms, "exponential", densityDomain)
d5.exp = getPredictedOpDensityUsingSingleDistr(op5$latency_ms, "exponential", densityDomain)
d6.exp = getPredictedOpDensityUsingSingleDistr(op6$latency_ms, "exponential", densityDomain)
d7.exp = getPredictedOpDensityUsingSingleDistr(op7$latency_ms, "exponential", densityDomain)
d8.exp = getPredictedOpDensityUsingSingleDistr(op8$latency_ms, "exponential", densityDomain)
d9.exp = getPredictedOpDensityUsingSingleDistr(op9$latency_ms, "exponential", densityDomain)

opDensityList.exp = list(d1=d1.exp, d2=d2.exp, d3=d3.exp, d4=d4.exp, d5=d5.exp, d6=d6.exp, d7=d7.exp, d8=d8.exp, d9=d9.exp)

# nA

pdf("~/Desktop/queryCDFs.pdf")
par(mfrow=c(2,2), mar=c(5,5,4,2)+0.1)

queries=nAqueries
queryType = "needsApproval"


# op hist
#nApredicted.hist = getNeedsApprovalDensityViaConvolution(opDensityList.hist) # won't work, b/c hists aren't aligned



# op density
nApredicted.opDensity = getNeedsApprovalDensityViaConvolutionGivenOps(ops, 20000)

# single exponential
nApredicted.singleExp = getNeedsApprovalDensityViaConvolution(opDensityList.exp)

# mixture model
load("~/Desktop/opDensityList.RData")
nApredicted.mm = getNeedsApprovalDensityViaConvolution(opDensityList)


nAsamples.opDensity = sample(nApredicted.opDensity$x, 10000, replace=TRUE, prob=nApredicted.opDensity$y)
nAsamples.singleExp = sample(nApredicted.singleExp$x, 10000, replace=TRUE, prob=nApredicted.singleExp$y)
nAsamples.mm = sample(nApredicted.mm$x, 10000, replace=TRUE, prob=nApredicted.mm$y)

# make cdf
cdf.actual=ecdf(queries$latency_ms)
cdf.seq=seq(min(queries$latency_ms), max(queries$latency_ms))

cdf.opDensity=ecdf(nAsamples.opDensity)
cdf.singleExp=ecdf(nAsamples.singleExp)
cdf.mm=ecdf(nAsamples.mm)

plot(cdf.seq, cdf.actual(cdf.seq), col=0, log="x", xlab="Latency (ms)", ylab="Quantile", main=paste("CDF for ", queryType, " Latency", sep=""))
lines(cdf.seq, cdf.actual(cdf.seq), col="black", lw=2, log="x")
lines(cdf.seq, cdf.opDensity(cdf.seq), col="red", lw=2, log="x")
lines(cdf.seq, cdf.singleExp(cdf.seq), col="blue", lw=2, log="x")
lines(cdf.seq, cdf.mm(cdf.seq), col="purple", lw=2, log="x")

legend("bottomright", legend=c("actual", "empirical", "exponential", "mixtures"), col=c("black", "red", "blue", "purple"), lw=2)


# uBH
queries=uBHqueries
queryType = "userByHometown"

uBHpredicted.opDensity = getUserByHometownDensityViaConvolutionGivenOps(ops, 20000)
uBHpredicted.singleExp = getUserByHometownDensityViaConvolution(opDensityList.exp)
uBHpredicted.mm = getUserByHometownDensityViaConvolution(opDensityList)

uBHsamples.opDensity = sample(uBHpredicted.opDensity$x, 10000, replace=TRUE, prob=uBHpredicted.opDensity$y)
uBHsamples.singleExp = sample(uBHpredicted.singleExp$x, 10000, replace=TRUE, prob=uBHpredicted.singleExp$y)
uBHsamples.mm = sample(uBHpredicted.mm$x, 10000, replace=TRUE, prob=uBHpredicted.mm$y)

cdf.actual=ecdf(queries$latency_ms)
cdf.seq=seq(min(queries$latency_ms), max(queries$latency_ms))

cdf.opDensity=ecdf(uBHsamples.opDensity)
cdf.singleExp=ecdf(uBHsamples.singleExp)
cdf.mm=ecdf(uBHsamples.mm)

plot(cdf.seq, cdf.actual(cdf.seq), col=0, log="x", xlab="Latency (ms)", ylab="Quantile", main=paste("CDF for ", queryType, " Latency", sep=""))
lines(cdf.seq, cdf.actual(cdf.seq), col="black", lw=2, log="x")
lines(cdf.seq, cdf.opDensity(cdf.seq), col="red", lw=2, log="x")
lines(cdf.seq, cdf.singleExp(cdf.seq), col="blue", lw=2, log="x")
lines(cdf.seq, cdf.mm(cdf.seq), col="purple", lw=2, log="x")
legend("bottomright", legend=c("actual", "empirical", "exponential", "mixtures"), col=c("black", "red", "blue", "purple"), lw=2)


# mF
queries=mFqueries
queryType = "myFollowing"

mFpredicted.opDensity = getMyFollowingDensityViaConvolutionGivenOps(ops, 20000)
mFpredicted.singleExp = getMyFollowingDensityViaConvolution(opDensityList.exp)
mFpredicted.mm = getMyFollowingDensityViaConvolution(opDensityList)

mFsamples.opDensity = sample(mFpredicted.opDensity$x, 10000, replace=TRUE, prob=mFpredicted.opDensity$y)
mFsamples.singleExp = sample(mFpredicted.singleExp$x, 10000, replace=TRUE, prob=mFpredicted.singleExp$y)
mFsamples.mm = sample(mFpredicted.mm$x, 10000, replace=TRUE, prob=mFpredicted.mm$y)

cdf.actual=ecdf(queries$latency_ms)
cdf.seq=seq(min(queries$latency_ms), max(queries$latency_ms))

cdf.opDensity=ecdf(mFsamples.opDensity)
cdf.singleExp=ecdf(mFsamples.singleExp)
cdf.mm=ecdf(mFsamples.mm)

plot(cdf.seq, cdf.actual(cdf.seq), col=0, log="x", xlab="Latency (ms)", ylab="Quantile", main=paste("CDF for ", queryType, " Latency", sep=""))
lines(cdf.seq, cdf.actual(cdf.seq), col="black", lw=2, log="x")
lines(cdf.seq, cdf.opDensity(cdf.seq), col="red", lw=2, log="x")
lines(cdf.seq, cdf.singleExp(cdf.seq), col="blue", lw=2, log="x")
lines(cdf.seq, cdf.mm(cdf.seq), col="purple", lw=2, log="x")
legend("bottomright", legend=c("actual", "empirical", "exponential", "mixtures"), col=c("black", "red", "blue", "purple"), lw=2)


# mT
queries=mTqueries
queryType = "myThoughts"

mTpredicted.opDensity = getMyThoughtsDensityViaConvolutionGivenOps(ops, 20000)
mTpredicted.singleExp = getMyThoughtsDensityViaConvolution(opDensityList.exp)
mTpredicted.mm = getMyThoughtsDensityViaConvolution(opDensityList)

mTsamples.opDensity = sample(mTpredicted.opDensity$x, 10000, replace=TRUE, prob=mTpredicted.opDensity$y)
mTsamples.singleExp = sample(mTpredicted.singleExp$x, 10000, replace=TRUE, prob=mTpredicted.singleExp$y)
mTsamples.mm = sample(mTpredicted.mm$x, 10000, replace=TRUE, prob=mTpredicted.mm$y)

cdf.actual=ecdf(queries$latency_ms)
cdf.seq=seq(min(queries$latency_ms), max(queries$latency_ms))

cdf.opDensity=ecdf(mTsamples.opDensity)
cdf.singleExp=ecdf(mTsamples.singleExp)
cdf.mm=ecdf(mTsamples.mm)

plot(cdf.seq, cdf.actual(cdf.seq), col=0, log="x", xlab="Latency (ms)", ylab="Quantile", main=paste("CDF for ", queryType, " Latency", sep=""))
lines(cdf.seq, cdf.actual(cdf.seq), col="black", lw=2, log="x")
lines(cdf.seq, cdf.opDensity(cdf.seq), col="red", lw=2, log="x")
lines(cdf.seq, cdf.singleExp(cdf.seq), col="blue", lw=2, log="x")
lines(cdf.seq, cdf.mm(cdf.seq), col="purple", lw=2, log="x")
legend("bottomright", legend=c("actual", "empirical", "exponential", "mixtures"), col=c("black", "red", "blue", "purple"), lw=2)

dev.off()




# table of errors

nA.actual=round(quantile(nAqueries$latency_ms, c(0.5,0.9,0.99)), digits=2)
uBH.actual=round(quantile(uBHqueries$latency_ms, c(0.5,0.9,0.99)), digits=2)
mF.actual=round(quantile(mFqueries$latency_ms, c(0.5,0.9,0.99)), digits=2)
mT.actual=round(quantile(mTqueries$latency_ms, c(0.5,0.9,0.99)), digits=2)


# empirical
source("convolutionForOpCombination-functions.R")
nA.empirical = getQuantileFromDensity(nApredicted.opDensity, c(0.5,0.9,0.99))
uBH.empirical = getQuantileFromDensity(uBHpredicted.opDensity, c(0.5,0.9,0.99))
mF.empirical = getQuantileFromDensity(mFpredicted.opDensity, c(0.5,0.9,0.99))
mT.empirical = getQuantileFromDensity(mTpredicted.opDensity, c(0.5,0.9,0.99))


# exp
nA.exp = getQuantileFromDensity(nApredicted.singleExp, c(0.5,0.9,0.99))
uBH.exp = getQuantileFromDensity(uBHpredicted.singleExp, c(0.5,0.9,0.99))
mF.exp = getQuantileFromDensity(mFpredicted.singleExp, c(0.5,0.9,0.99))
mT.exp = getQuantileFromDensity(mTpredicted.singleExp, c(0.5,0.9,0.99))


# mm
nA.mm = getQuantileFromDensity(nApredicted.mm, c(0.5,0.9,0.99))
uBH.mm = getQuantileFromDensity(uBHpredicted.mm, c(0.5,0.9,0.99))
mF.mm = getQuantileFromDensity(mFpredicted.mm, c(0.5,0.9,0.99))
mT.mm = getQuantileFromDensity(mTpredicted.mm, c(0.5,0.9,0.99))



round(nA.empirical, digits=2)
round(nA.exp, digits=2)
round(nA.mm, digits=2)

# get percentages
actual=round(quantile(nAqueries$latency_ms, c(0.5,0.9,0.99)), digits=2)
empirical=nA.empirical
exp=nA.exp
mm=nA.mm

actual=round(quantile(uBHqueries$latency_ms, c(0.5,0.9,0.99)), digits=2)
empirical=uBH.empirical
exp=uBH.exp
mm=uBH.mm

actual=round(quantile(mFqueries$latency_ms, c(0.5,0.9,0.99)), digits=2)
empirical=mF.empirical
exp=mF.exp
mm=mF.mm

actual=round(quantile(mTqueries$latency_ms, c(0.5,0.9,0.99)), digits=2)
empirical=mT.empirical
exp=mT.exp
mm=mT.mm


for (i in 1:3) {
	if (i == 1) {
		print("median")
	} else if (i == 2) {
		print("90th")
	} else if (i == 3) {
		print("99th")
	}
	print(paste("  actual=", round(actual[i]/actual[i]*100, digits=2)))
	print(paste("  empirical=", round(empirical[i]/actual[i]*100, digits=2)))
	print(paste("  exp=", round(exp[i]/actual[i]*100, digits=2)))
	print(paste("  mm=", round(mm[i]/actual[i]*100, digits=2)))
}


round(uBH.empirical, digits=2)
round(uBH.exp, digits=2)
round(uBH.mm, digits=2)

round(mF.empirical, digits=2)
round(mF.exp, digits=2)
round(mF.mm, digits=2)

round(mT.empirical, digits=2)
round(mT.exp, digits=2)
round(mT.mm, digits=2)





# get average error for each method at each quantile of interest
for (i in 1:3) {
	if (i == 1) {
		print("median")
	} else if (i == 2) {
		print("90th")
	} else if (i == 3) {
		print("99th")
	}

	#print(paste("  empirical avg error=", round(mean(nA.empirical[i]/nA.actual[i]*100, uBH.empirical[i]/uBH.actual[i]*100, mF.empirical[i]/mF.actual[i]*100, mT.empirical[i]/mT.actual[i]*100), digits=2)-100))
	#print(paste("  exp avg error=", round(mean(nA.exp[i]/nA.actual[i]*100, uBH.exp[i]/uBH.actual[i]*100, mF.exp[i]/mF.actual[i]*100, mT.exp[i]/mT.actual[i]*100), digits=2)-100))
	#print(paste("  mm avg error=", round(mean(nA.mm[i]/nA.actual[i]*100, uBH.mm[i]/uBH.actual[i]*100, mF.mm[i]/mF.actual[i]*100, mT.mm[i]/mT.actual[i]*100), digits=2)-100))

	print(paste("  empirical avg error=", round(mean(c(abs(nA.empirical[i]-nA.actual[i])/nA.actual[i]*100, abs(uBH.empirical[i]-uBH.actual[i])/uBH.actual[i]*100, abs(mF.empirical[i]-mF.actual[i])/mF.actual[i]*100, abs(mT.empirical[i]-mT.actual[i])/mT.actual[i]*100)), digits=2)))
	print(paste("  exp avg error=", round(mean(c(abs(nA.exp[i]-nA.actual[i])/nA.actual[i]*100, abs(uBH.exp[i]-uBH.actual[i])/uBH.actual[i]*100, abs(mF.exp[i]-mF.actual[i])/mF.actual[i]*100, abs(mT.exp[i]-mT.actual[i])/mT.actual[i]*100)), digits=2)))
	print(paste("  mm avg error=", round(mean(c(abs(nA.mm[i]-nA.actual[i])/nA.actual[i]*100, abs(uBH.mm[i]-uBH.actual[i])/uBH.actual[i]*100, abs(mF.mm[i]-mF.actual[i])/mF.actual[i]*100, abs(mT.mm[i]-mT.actual[i])/mT.actual[i]*100)), digits=2)))

}









