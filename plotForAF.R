rm(list=ls())
load("/Users/ksauer/Desktop/Research/MSReport/MSExperimentData/queriesAndTheirOps.RData")

pdf("~/Desktop/slo.pdf")

par(mar=c(5,5,4,2)+0.1)
plot(density(nAqueries$latency_ms), xlab="Latency (ms)", ylab="Density", main="Latency Distribution for SCADS Query")

source("/Users/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/multiQuery-functions.R")

annotatePlotWithQuantiles(nAqueries$latency_ms)

dev.off()



# for midterm review

rm(list=ls())
load("/Users/ksauer/Desktop/Research/MSReport/MSExperimentData/queriesAndTheirOps.RData")

pdf("~/Desktop/slo.pdf")

par(mar=c(5,5,4,2)+0.1)
hist(nAqueries$latency_ms, breaks=100, xlab="Latency (ms)", ylab="Count", main="Latency Distribution for SCADS Query", xlim=c(0,100))

source("/Users/ksauer/Desktop/scads/experiments/client/performance/logparsing/src/main/R/multiQuery-functions.R")

annotatePlotWithQuantiles(nAqueries$latency_ms)

dev.off()