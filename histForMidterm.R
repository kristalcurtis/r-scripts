bins = c(20,19,10,5,4,2.5,4,3.5,2.5,0.6,0.5,0.2,0.1,0.2,0.1,0.2,0.1,0.1)
binsLots = bins*100
sum(bins)
#plot(bins)

latencyData = matrix(nrow=1, ncol=bins[1]*1000,data=rep(5,bins[1]*1000))
dim(latencyData)
#latencyData


for (i in 2:length(bins)) {
	latencyData = c(latencyData, rep(i*10-5, bins[i]*1000))
}


dim(rep(i*10-5, bins[i]*1000))

pdf("~/Desktop/midtermHist.pdf")

hist(latencyData, xlab="Latency (ms)", ylab="Count", main="Web Request Latency Histogram", xlim=c(0,200))
#median=median(latencyData)
#q95=quantile(latencyData, 0.95)
#q99=quantile(latencyData, 0.99)

median=20
q95=90
q99=130

#abline(v=median, col="red", lw=2, lty="solid")
abline(v=median, col="red", lw=2, lty="dotdash")
abline(v=q95, col="blue", lw=2, lty="dashed")
abline(v=q99, col="purple", lw=2, lty="solid")
legend("topright", legend=c(paste("median=", round(median), "ms", sep=""), paste("95th %ile=", round(q95), "ms", sep=""), paste("99th %ile=", round(q99), "ms", sep="")), col=c("red", "blue", "purple"), lwd=2, lty=c("dotdash", "dashed", "solid"))

dev.off()