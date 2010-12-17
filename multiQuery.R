# nA = query type 8
queries.uBH0.nA1 = as.data.frame(read.csv("/Users/ksauer/Desktop/run-1285360286054/UserByHT0_NeedsApp1-threads5-take0/queries.csv"))
dim(queries.uBH0.nA1)

summary(queries.uBH0.nA1$latency_ms[which(queries.uBH0.nA1$opType == 6)])
quantile(queries.uBH0.nA1$latency_ms[which(queries.uBH0.nA1$opType == 6)], c(0.25, 0.5, 0.75, 0.9, 0.99))

summary(queries.uBH0.nA1$latency_ms[which(queries.uBH0.nA1$opType == 8)])
quantile(queries.uBH0.nA1$latency_ms[which(queries.uBH0.nA1$opType == 8)], c(0.25, 0.5, 0.75, 0.9, 0.99))

#25/75
queries.uBH0.25.nA0.75 = as.data.frame(read.csv("/Users/ksauer/Desktop/run-1285360286054/UserByHT25_NeedsApp75-threads5-take1/queries.csv"))
dim(queries.uBH0.25.nA0.75)

summary(queries.uBH0.25.nA0.75$latency_ms[which(queries.uBH0.25.nA0.75$opType == 6)])
quantile(queries.uBH0.25.nA0.75$latency_ms[which(queries.uBH0.25.nA0.75$opType == 6)], c(0.25, 0.5, 0.75, 0.9, 0.99))

summary(queries.uBH0.25.nA0.75$latency_ms[which(queries.uBH0.25.nA0.75$opType == 8)])
quantile(queries.uBH0.25.nA0.75$latency_ms[which(queries.uBH0.25.nA0.75$opType == 8)], c(0.25, 0.5, 0.75, 0.9, 0.99))


# 50/50
queries.uBH0.5.nA0.5 = as.data.frame(read.csv("/Users/ksauer/Desktop/run-1285360286054/UserByHT50_NeedsApp50-threads5-take2/queries.csv"))
dim(queries.uBH0.5.nA0.5)

summary(queries.uBH0.5.nA0.5$latency_ms[which(queries.uBH0.5.nA0.5$opType == 6)])
quantile(queries.uBH0.5.nA0.5$latency_ms[which(queries.uBH0.5.nA0.5$opType == 6)], c(0.25, 0.5, 0.75, 0.9, 0.99))

summary(queries.uBH0.5.nA0.5$latency_ms[which(queries.uBH0.5.nA0.5$opType == 8)])
quantile(queries.uBH0.5.nA0.5$latency_ms[which(queries.uBH0.5.nA0.5$opType == 8)], c(0.25, 0.5, 0.75, 0.9, 0.99))

# 75/25
queries.uBH0.75.nA0.25 = as.data.frame(read.csv("/Users/ksauer/Desktop/run-1285360286054/UserByHT75_NeedsApp25-threads5-take3/queries.csv"))

quantile(queries.uBH0.75.nA0.25$latency_ms[which(queries.uBH0.75.nA0.25$opType == 6)], c(0.25, 0.5, 0.75, 0.9, 0.99))

quantile(queries.uBH0.75.nA0.25$latency_ms[which(queries.uBH0.75.nA0.25$opType == 8)], c(0.25, 0.5, 0.75, 0.9, 0.99))

# uBH = query type 6
queries.uBH1.nA0 = as.data.frame(read.csv("/Users/ksauer/Desktop/run-1285360286054/UserByHT1_NeedsApp0-threads5-take4/queries.csv"))
dim(queries.uBH1.nA0)

summary(queries.uBH1.nA0$latency_ms[which(queries.uBH1.nA0$opType == 6)])
quantile(queries.uBH1.nA0$latency_ms[which(queries.uBH1.nA0$opType == 6)], c(0.25, 0.5, 0.75, 0.9, 0.99))

summary(queries.uBH1.nA0$latency_ms[which(queries.uBH1.nA0$opType == 8)])
quantile(queries.uBH1.nA0$latency_ms[which(queries.uBH1.nA0$opType == 8)], c(0.25, 0.5, 0.75, 0.9, 0.99))



# Plot the data

# userByHometown (query 6)
pdf("~/Desktop/uBH.pdf")
par(mfrow=c(4,1))
#xlim=c(0, max(queries.uBH0.25.nA0.75$latency_ms[which(queries.uBH0.25.nA0.75$opType == 6)], queries.uBH0.5.nA0.5$latency_ms[which(queries.uBH0.5.nA0.5$opType == 6)], queries.uBH0.75.nA0.25$latency_ms[which(queries.uBH0.75.nA0.25$opType == 6)], queries.uBH1.nA0$latency_ms[which(queries.uBH1.nA0$opType == 6)]))
xlim=c(0,100)

hist(queries.uBH0.25.nA0.75$latency_ms[which(queries.uBH0.25.nA0.75$opType == 6)], breaks=100, xlab="Latency (ms)", ylab="Count", main="userByHometown (userByHometown 0.25, needsApproval 0.75)", xlim=xlim)
annotatePlotWithQuantiles(queries.uBH0.25.nA0.75$latency_ms[which(queries.uBH0.25.nA0.75$opType == 6)])

hist(queries.uBH0.5.nA0.5$latency_ms[which(queries.uBH0.5.nA0.5$opType == 6)], breaks=100, xlab="Latency (ms)", ylab="Count", main="userByHometown (userByHometown 0.5, needsApproval 0.5)", xlim=xlim)
annotatePlotWithQuantiles(queries.uBH0.5.nA0.5$latency_ms[which(queries.uBH0.5.nA0.5$opType == 6)])

hist(queries.uBH0.75.nA0.25$latency_ms[which(queries.uBH0.75.nA0.25$opType == 6)], breaks=200, xlab="Latency (ms)", ylab="Count", main="userByHometown (userByHometown 0.75, needsApproval 0.25)", xlim=xlim)
annotatePlotWithQuantiles(queries.uBH0.75.nA0.25$latency_ms[which(queries.uBH0.75.nA0.25$opType == 6)])

hist(queries.uBH1.nA0$latency_ms[which(queries.uBH1.nA0$opType == 6)], breaks=100, xlab="Latency (ms)", ylab="Count", main="userByHometown (userByHometown 1, needsApproval 0)", xlim=xlim)
annotatePlotWithQuantiles(queries.uBH1.nA0$latency_ms[which(queries.uBH1.nA0$opType == 6)])
dev.off()







# needsApproval (query 8)

pdf(file="~/Desktop/nA.pdf")
par(mfrow=c(4,1))
xlim=c(0,100)

hist(queries.uBH0.75.nA0.25$latency_ms[which(queries.uBH0.75.nA0.25$opType == 8)], breaks=200, xlab="Latency (ms)", ylab="Count", main="needsApproval (userByHometown 0.75, needsApproval 0.25)", xlim=xlim)
annotatePlotWithQuantiles(queries.uBH0.75.nA0.25$latency_ms[which(queries.uBH0.75.nA0.25$opType == 8)])

hist(queries.uBH0.5.nA0.5$latency_ms[which(queries.uBH0.5.nA0.5$opType == 8)], breaks=100, xlab="Latency (ms)", ylab="Count", main="needsApproval (userByHometown 0.5, needsApproval 0.5)", xlim=xlim)
annotatePlotWithQuantiles(queries.uBH0.5.nA0.5$latency_ms[which(queries.uBH0.5.nA0.5$opType == 8)])

hist(queries.uBH0.25.nA0.75$latency_ms[which(queries.uBH0.25.nA0.75$opType == 8)], breaks=100, xlab="Latency (ms)", ylab="Count", main="needsApproval (userByHometown 0.25, needsApproval 0.75)", xlim=xlim)
annotatePlotWithQuantiles(queries.uBH0.25.nA0.75$latency_ms[which(queries.uBH0.25.nA0.75$opType == 8)])

hist(queries.uBH0.nA1$latency_ms[which(queries.uBH0.nA1$opType == 8)], breaks=200, xlab="Latency (ms)", ylab="Count", main="needsApproval (userByHometown 0, needsApproval 1)", xlim=xlim)
annotatePlotWithQuantiles(queries.uBH0.nA1$latency_ms[which(queries.uBH0.nA1$opType == 8)])
dev.off()


# Testing loadQueryMixData
directoriesList = c("/Users/ksauer/Desktop/userByName-thoughtstream/UserByN0_Thoughtstream1-threads5-take0", "/Users/ksauer/Desktop/userByName-thoughtstream/UserByN1_Thoughtstream0-threads5-take4", "/Users/ksauer/Desktop/userByName-thoughtstream/UserByN25_Thoughtstream75-threads5-take1", "/Users/ksauer/Desktop/userByName-thoughtstream/UserByN75_Thoughtstream25-threads5-take3")

setwd("~/Desktop/multiQuery")
source("multiQuery-functions.R")
queryMixList = loadQueryMixData(directoriesList)
# looks good



# Testing getXmax
source("multiQuery-functions.R")
getXmax(queryMixList, 4)

# Testing getQueryTypes
source("multiQuery-functions.R")
getQueryTypes(queryMixList)


# Testing makePlotTitle
source("multiQuery-functions.R")
queries = c("userByName", "userByEmail", "thoughtsByHashTag", "thoughtstream", "myFollowing", "userByHometown", "myThoughts", "needsApproval")
makePlotTitle(queries[4], directoriesList[3])

# Testing getQueryLatencyForOneQueryTypeUnderOneMix
source("multiQuery-functions.R")
values = getQueryLatencyForOneQueryTypeUnderOneMix(queryMixList, 1, 1)

# Testing plotQueryLatencyForOneQueryTypeUnderOneMix
setwd("~/Desktop/multiQuery")
source("multiQuery-functions.R")
queries = c("userByName", "userByEmail", "thoughtsByHashTag", "thoughtstream", "myFollowing", "userByHometown", "myThoughts", "needsApproval")
queryType=1
mix=1
plotTitle=makePlotTitle(queries[queryType], directoriesList[mix])
plotQueryLatencyForOneQueryTypeUnderOneMix(queryMixList, queryType, mix, plotTitle)

# Testing plotQueryMixLatencyForEachQueryUnderEachMix
source("multiQuery-functions.R")
baseDir = "/Users/ksauer/Desktop/userByName-thoughtstream"
directoriesList = c("UserByN0_Thoughtstream1-threads5-take0", "UserByN1_Thoughtstream0-threads5-take4", "UserByN25_Thoughtstream75-threads5-take1", "UserByN75_Thoughtstream25-threads5-take3")

queries = c("userByName", "userByEmail", "thoughtsByHashTag", "thoughtstream", "myFollowing", "userByHometown", "myThoughts", "needsApproval")
plotQueryMixLatencyForEachQueryUnderEachMix(baseDir, directoriesList, queries)


