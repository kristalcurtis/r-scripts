# Q-Q Plots for needsApproval, userByHometown, myFollowing, myThoughts
?qqplot


pdf("~/Desktop/Retreat-Figs/qqplots-legend-v2-largetext.pdf")
par(mfrow=c(2,2), mar=c(5,5,4,2)+0.1, cex.axis=1.5, cex.lab=1.2, cex.main=1.5)

queries = nAqueries
#samples = nAsamples
samples = nAsamples.v2
qqplot(queries$latency_ms, samples, xlim=c(0,200), ylim=c(0,200), xlab="Actual Latency Values (ms)", ylab="Predicted Latency Values (ms)", main="needsApproval")
abline(a=1,b=1, lw=2, col="green")
legend("topleft", legend="Perfect Prediction", lwd=2, col="green")
num.outliers = length(which(queries$latency_ms > 100))

queries = uBHqueries
#samples = uBHsamples
samples = uBHsamples.v2
qqplot(queries$latency_ms, samples, xlim=c(0,250), ylim=c(0,250), xlab="Actual Latency Values (ms)", ylab="Predicted Latency Values (ms)", main="userByHometown")
abline(a=1,b=1, lw=2, col="green")


queries = mFqueries
#samples = mFsamples
samples = mFsamples.v2
qqplot(queries$latency_ms, samples, xlim=c(0,250), ylim=c(0,250), xlab="Actual Latency Values (ms)", ylab="Predicted Latency Values (ms)", main="myFollowing")
abline(a=1,b=1, lw=2, col="green")


queries = mTqueries
#samples = mTsamples
samples = mTsamples.v2
xymax = max(queries$latency_ms, samples)
qqplot(mTqueries$latency_ms, mTsamples, xlim=c(0,xymax), ylim=c(0,xymax), xlab="Actual Latency Values (ms)", ylab="Predicted Latency Values (ms)", main="myThoughts")
abline(a=1,b=1, lw=2, col="green")
dev.off()



# bar graph for retreat
?barplot
colors()

pdf("~/Desktop/Retreat-Figs/barplots-no-legend-v2-largetext.pdf", height=10, width=10)
par(mfrow=c(2,2), mar=c(5,5,4,2)+0.1, cex.axis=2, cex.lab=2, cex.main=2)

queries = nAqueries
#samples = nAsamples
samples = nAsamples.v2
barplot(c(quantile(queries$latency_ms,0.5), quantile(samples,0.5), quantile(queries$latency_ms,0.9), quantile(samples,0.9), quantile(queries$latency_ms,0.99), quantile(samples,0.99)), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), xlab="Quantile", ylab="Latency (ms)", main="needsApproval", ylim=c(0,1.1*max(quantile(queries$latency_ms,0.99), quantile(samples, 0.99))))
#legend("topleft", c("Actual median", "Predicted median", "Actual 90th", "Predicted 90th", "Actual 99th", "Predicted 99th"), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), lwd=10)
actual=quantile(queries$latency_ms,0.5)
predicted=quantile(samples,0.5)
print(paste("nA med=", abs(actual-predicted)/actual))
print(paste("nA med(ms)=", abs(actual-predicted)))

actual=quantile(queries$latency_ms,0.9)
predicted=quantile(samples,0.9)
print(paste("nA 90th=", abs(actual-predicted)/actual))
print(paste("nA 90th(ms)=", abs(actual-predicted)))

actual=quantile(queries$latency_ms,0.99)
predicted=quantile(samples,0.99)
print(paste("nA 99th=", abs(actual-predicted)/actual))
print(paste("nA 99th(ms)=", abs(actual-predicted)))


queries = uBHqueries
#samples = uBHsamples
samples = uBHsamples.v2
barplot(c(quantile(queries$latency_ms,0.5), quantile(samples,0.5), quantile(queries$latency_ms,0.9), quantile(samples,0.9), quantile(queries$latency_ms,0.99), quantile(samples,0.99)), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), xlab="Quantile", ylab="Latency (ms)", main="userByHometown", ylim=c(0,1.1*max(quantile(queries$latency_ms,0.99), quantile(samples, 0.99))))

actual=quantile(queries$latency_ms,0.5)
predicted=quantile(samples,0.5)
print(paste("uBH med=", abs(actual-predicted)/actual))
print(paste("uBH med(ms)=", abs(actual-predicted)))

actual=quantile(queries$latency_ms,0.9)
predicted=quantile(samples,0.9)
print(paste("uBH 90th=", abs(actual-predicted)/actual))
print(paste("uBH 90th(ms)=", abs(actual-predicted)))

actual=quantile(queries$latency_ms,0.99)
predicted=quantile(samples,0.99)
print(paste("uBH 99th=", abs(actual-predicted)/actual))
print(paste("uBH 99th(ms)=", abs(actual-predicted)))

queries = mFqueries
#samples = mFsamples
samples = mFsamples.v2
barplot(c(quantile(queries$latency_ms,0.5), quantile(samples,0.5), quantile(queries$latency_ms,0.9), quantile(samples,0.9), quantile(queries$latency_ms,0.99), quantile(samples,0.99)), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), xlab="Quantile", ylab="Latency (ms)", main="myFollowing", ylim=c(0,1.1*max(quantile(queries$latency_ms,0.99), quantile(samples, 0.99))))

actual=quantile(queries$latency_ms,0.5)
predicted=quantile(samples,0.5)
print(paste("mF med=", abs(actual-predicted)/actual))
print(paste("mF med(ms)=", abs(actual-predicted)))

actual=quantile(queries$latency_ms,0.9)
predicted=quantile(samples,0.9)
print(paste("mF 90th=", abs(actual-predicted)/actual))
print(paste("mF 90th(ms)=", abs(actual-predicted)))

actual=quantile(queries$latency_ms,0.99)
predicted=quantile(samples,0.99)
print(paste("mF 99th=", abs(actual-predicted)/actual))
print(paste("mF 99th(ms)=", abs(actual-predicted)))

queries = mTqueries
#samples = mTsamples
samples = mTsamples.v2
barplot(c(quantile(queries$latency_ms,0.5), quantile(samples,0.5), quantile(queries$latency_ms,0.9), quantile(samples,0.9), quantile(queries$latency_ms,0.99), quantile(samples,0.99)), col=c("red1", "red4", "blue1", "blue4", "green1", "green4"), xlab="Quantile", ylab="Latency (ms)", main="myThoughts", ylim=c(0,1.1*max(quantile(queries$latency_ms,0.99), quantile(samples, 0.99))))

actual=quantile(queries$latency_ms,0.5)
predicted=quantile(samples,0.5)
print(paste("mT med=", abs(actual-predicted)/actual))
print(paste("mT med(ms)=", abs(actual-predicted)))

actual=quantile(queries$latency_ms,0.9)
predicted=quantile(samples,0.9)
print(paste("mT 90th=", abs(actual-predicted)/actual))
print(paste("mT 90th(ms)=", abs(actual-predicted)))

actual=quantile(queries$latency_ms,0.99)
predicted=quantile(samples,0.99)
print(paste("mT 99th=", abs(actual-predicted)/actual))
print(paste("mT 99th(ms)=", abs(actual-predicted)))

dev.off()


save(list=ls(), file="~/Desktop/retreat-modeling.RData")


# cdfs

# my Thoughts

pdf("~/Desktop/Retreat-Figs/cdfs-v2-largetext.pdf")
par(mfrow=c(2,2), mar=c(5,5,4,2)+0.1, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
#par(mar=c(5,5,4,2)+0.1, cex=2)

queries = nAqueries
#samples = nAsamples
#samples = needsApprovalSampler(h1.v2, h3.v2, h4.v2, h6.v2, h9.v2, 1000)
samples = nAsamples.v2
cdf = ecdf(queries$latency_ms)
samplesCdf = ecdf(samples)
cdfSeq = seq(min(queries$latency_ms), max(queries$latency_ms))
plot(cdfSeq, cdf(cdfSeq), log="x", col=0, xlab="Latency (ms)", ylab="Quantile", main="needsApproval")
lines(cdfSeq, cdf(cdfSeq), col="red", lw=2, log="x")
lines(cdfSeq, samplesCdf(cdfSeq), col="blue", lw=2, log="x")
legend("bottomright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lwd=2, cex=1.2)

queries = uBHqueries
#samples = uBHsamples
#samples = userByHometownSampler(h2.v2, h3.v2, h6.v2, 1000)
samples = uBHsamples.v2
cdf = ecdf(queries$latency_ms)
samplesCdf = ecdf(samples)
cdfSeq = seq(min(queries$latency_ms), max(queries$latency_ms))
plot(cdfSeq, cdf(cdfSeq), log="x", col=0, xlab="Latency (ms)", ylab="Quantile", main="userByHometown")
lines(cdfSeq, cdf(cdfSeq), col="red", lw=2, log="x")
lines(cdfSeq, samplesCdf(cdfSeq), col="blue", lw=2, log="x")
#legend("bottomright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lwd=2)

queries = mFqueries
#samples = mFsamples
#samples = myFollowingSampler(h1.v2, h4.v2, h5.v2, h6.v2, 1000)
samples = mFsamples.v2
cdf = ecdf(queries$latency_ms)
samplesCdf = ecdf(samples)
cdfSeq = seq(min(queries$latency_ms), max(queries$latency_ms))
plot(cdfSeq, cdf(cdfSeq), log="x", col=0, xlab="Latency (ms)", ylab="Quantile", main="myFollowing")
lines(cdfSeq, cdf(cdfSeq), col="red", lw=2, log="x")
lines(cdfSeq, samplesCdf(cdfSeq), col="blue", lw=2, log="x")
#legend("bottomright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lwd=2)

queries = mTqueries
#samples = mTsamples
#samples = myThoughtsSampler(h1.v2, h4.v2, h6.v2, h9.v2, 1000)
samples = mTsamples.v2
cdf = ecdf(queries$latency_ms)
samplesCdf = ecdf(samples)
cdfSeq = seq(min(queries$latency_ms), max(queries$latency_ms))
plot(cdfSeq, cdf(cdfSeq), log="x", col=0, xlab="Latency (ms)", ylab="Quantile", main="myThoughts")
lines(cdfSeq, cdf(cdfSeq), col="red", lw=2, log="x")
lines(cdfSeq, samplesCdf(cdfSeq), col="blue", lw=2, log="x")
#legend("bottomright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lwd=2)
dev.off()



# try to smooth the above
h1.v2 = hist(op1$latency_ms, breaks=1000)
h2.v2 = hist(op2$latency_ms, breaks=1000)
h3.v2 = hist(op3$latency_ms, breaks=1000)
h4.v2 = hist(op4$latency_ms, breaks=1000)
h5.v2 = hist(op5$latency_ms, breaks=1000)
h6.v2 = hist(op6$latency_ms, breaks=1000)
h7.v2 = hist(op7$latency_ms, breaks=1000)
h8.v2 = hist(op8$latency_ms, breaks=1000)
h9.v2 = hist(op9$latency_ms, breaks=1000)

nAsamples.v2 = needsApprovalSampler(h1.v2, h3.v2, h4.v2, h6.v2, h9.v2, 1000)
uBHsamples.v2 = userByHometownSampler(h2.v2, h3.v2, h6.v2, 1000)
mFsamples.v2 = myFollowingSampler(h1.v2, h4.v2, h5.v2, h6.v2, 1000)
mTsamples.v2 = myThoughtsSampler(h1.v2, h4.v2, h6.v2, h9.v2, 1000)



# looking at histograms

par(mfrow=c(2,1), mar=c(5,5,4,2)+0.1)

queries=mTqueries
samples=mTsamples.v2

xmax=max(queries$latency_ms, samples)
latencyDistr = queries$latency_ms
hist(latencyDistr, xlim=c(0,xmax), breaks=20, xlab="Latency (ms)", ylab="Count", main="Latency Distribution, Actual")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)

latencyDistr = samples
hist(latencyDistr, xlim=c(0,xmax), breaks=50, xlab="Latency (ms)", ylab="Count", main="Latency Distribution, Predicted")
abline(v=median(latencyDistr), lw=2, col="red")
abline(v=quantile(latencyDistr, 0.9), lw=2, col="blue")
abline(v=quantile(latencyDistr, 0.99), lw=2, col="green")
legend("topright", legend=c(paste("median=", round(median(latencyDistr), digits=2), "ms", sep=""), 
paste("90th=", round(quantile(latencyDistr,0.9), digits=2), "ms", sep=""),
paste("99th=", round(quantile(latencyDistr,0.99), digits=2), "ms", sep="")), col=c("red", "blue", "green"), lwd=2)

