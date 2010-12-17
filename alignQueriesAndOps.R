# 7.19.10
# Load queries & ops files into R on R cluster; save to queriesAndTheirOps.RData

setwd("/work/ksauer/7.19.10-alignQueriesAndOps")

nAqueries = as.data.frame(read.csv("needsApproval/queries.csv"))
uBHqueries = as.data.frame(read.csv("userByHometown/queries.csv"))
mFqueries = as.data.frame(read.csv("myFollowing/queries.csv"))
mTqueries = as.data.frame(read.csv("myThoughts/queries.csv"))

nAops = as.data.frame(read.csv("needsApproval/ops.csv"))
uBHops = as.data.frame(read.csv("userByHometown/ops.csv"))
mFops = as.data.frame(read.csv("myFollowing/ops.csv"))
mTops = as.data.frame(read.csv("myThoughts/ops.csv"))

save(nAqueries, uBHqueries, mFqueries, mTqueries, nAops, uBHops, mFops, mTops, file="queriesAndTheirOps.RData")



# work with file on laptop
rm(list=ls())
load("~/Desktop/queriesAndTheirOps.RData")
ls()

# figuring out what threadNum are there
sort(unique(nAqueries$threadNum))
sort(unique(nAops$threadNum))

sort(unique(uBHqueries$threadNum))
sort(unique(uBHops$threadNum))

sort(unique(mFqueries$threadNum))
sort(unique(mFops$threadNum))

sort(unique(mTqueries$threadNum))
sort(unique(mTops$threadNum))


# try deinterleaving
nAqueries.thread5 = nAqueries[which(nAqueries$threadNum==5),]
nAqueries[1:10,]
nAqueries.thread5[1:10,]

nAops.thread5 = nAops[which(nAops$threadNum==5),]
nAops[1:10,]
nAops.thread5[1:16,]


sum(nAops.thread5[1:8,"latency_ms"])
nAqueries.thread5[1,"latency_ms"]

sum(nAops.thread5$latency_ms[9:16])
nAqueries.thread5$latency_ms[2]

queryNum=3
numOps=8
sum(nAops.thread5$latency_ms[((queryNum-1)*numOps+1):(queryNum*numOps)])
nAqueries.thread5$latency_ms[queryNum]


# deinterleave query & op matrices by thread num; create a new mtx for each by appending each thread's values => sorted by thread num

queries=nAqueries

startingThread=5
endingThread=9

print(startingThread)
queries.threadSorted = queries[which(queries$threadNum == startingThread),]

for (i in (startingThread+1):endingThread) {
	print(i)
	currentThreadData = queries[which(queries$threadNum == i),]
	queries.threadSorted = rbind(queries.threadSorted, currentThreadData)
}

dim(queries.threadSorted)

# turn this into a function
# once the queries & ops have been deinterleaved, shouldn't have to worry about threadNum anymore
nAqueries.deinterleaved = deinterleaveLogsMultiThreads(nAqueries)
nAops.deinterleaved = deinterleaveLogsMultiThreads(nAops)

dim(nAops.deinterleaved)
dim(nAops.deinterleaved[which(nAops.deinterleaved$threadNum == 5),])

threadNum=9
dim(nAops.deinterleaved[which(nAops.deinterleaved$threadNum == threadNum),])/dim(nAops.deinterleaved)
nAops.deinterleaved[1:10,]

uBHqueries.deinterleaved = deinterleaveLogsMultiThreads(uBHqueries)
uBHops.deinterleaved = deinterleaveLogsMultiThreads(uBHops)
uBHops.deinterleaved[1:10,]

mFqueries.deinterleaved = deinterleaveLogsMultiThreads(mFqueries)
mFops.deinterleaved = deinterleaveLogsMultiThreads(mFops)
mFops.deinterleaved[1:10,]

mTqueries.deinterleaved = deinterleaveLogsMultiThreads(mTqueries)
mTops.deinterleaved = deinterleaveLogsMultiThreads(mTops)
mTops.deinterleaved[1:10,]


# verify that it works
numQueries = nrow(nAqueries.deinterleaved)
queryNum=4000
numOps=8 # each query has a single value of this #
sum(nAops.deinterleaved$latency_ms[((queryNum-1)*numOps+1):(queryNum*numOps)])
nAqueries.deinterleaved$latency_ms[queryNum]
# looks good

# write a function that will return the ops for a given query #
perQueryOps = getOpsForGivenQuery(nAops.deinterleaved, 4000, "needsApproval")
sum(perQueryOps$latency_ms)

dim(uBHqueries)
dim(uBHqueries.deinterleaved)
queryNum=7500
uBHqueries.deinterleaved$latency_ms[queryNum]
perQueryOps = getOpsForGivenQuery(uBHops.deinterleaved, queryNum, "userByHometown")
sum(perQueryOps$latency_ms)

dim(mFqueries)
dim(mFqueries.deinterleaved)
queryNum=8000
mFqueries.deinterleaved$latency_ms[queryNum]
perQueryOps = getOpsForGivenQuery(mFops.deinterleaved, queryNum, "myFollowing")
sum(perQueryOps$latency_ms)

dim(mTqueries)
dim(mTqueries.deinterleaved)
queryNum=13000
mTqueries.deinterleaved$latency_ms[queryNum]
perQueryOps = getOpsForGivenQuery(mTops.deinterleaved, queryNum, "myThoughts")
sum(perQueryOps$latency_ms)
# ok, i'm pretty confident that these work for all 4 queries


save(nAqueries.deinterleaved, nAops.deinterleaved, uBHqueries.deinterleaved, uBHops.deinterleaved, mFqueries.deinterleaved, mFops.deinterleaved, mTqueries.deinterleaved, mTops.deinterleaved, file="~/Desktop/alignedQueriesAndOps.RData")


# look at gaps b/t sum of op latency, corresponding query latency
nAgaps = getGapsBetweenOpsAndQueryLatency(nAops.deinterleaved, nAqueries.deinterleaved, "needsApproval")
hist(nAgaps, breaks=1000, xlim=c(0,1))
summary(nAgaps)
quantile(nAgaps, 0.99)
quantile(nAgaps, 0.999)

uBHgaps = getGapsBetweenOpsAndQueryLatency(uBHops.deinterleaved, uBHqueries.deinterleaved, "userByHometown")
hist(uBHgaps, breaks=1000, xlim=c(0,1))
summary(uBHgaps)
quantile(uBHgaps, 0.99)
quantile(uBHgaps, 0.999)

mFgaps = getGapsBetweenOpsAndQueryLatency(mFops.deinterleaved, mFqueries.deinterleaved, "myFollowing")
hist(mFgaps, breaks=1000, xlim=c(0,1))
summary(mFgaps)
quantile(mFgaps, 0.99)
quantile(mFgaps, 0.999)

mTgaps = getGapsBetweenOpsAndQueryLatency(mTops.deinterleaved, mTqueries.deinterleaved, "myThoughts")
hist(mTgaps, breaks=1000, xlim=c(0,1))
summary(mTgaps)
quantile(mTgaps, 0.99)
quantile(mTgaps, 0.999)

save(nAgaps, uBHgaps, mFgaps, mTgaps, file="~/Desktop/gaps.RData")

# get queries whose latency values are within the range indicated
summary(uBHqueries$latency_ms)
quantile(uBHqueries$latency_ms,0.99)

queries50.60 = getQueriesByLatencyRange(queries, 50, 60)
dim(queries50.60)

length(getWhichQueriesByLatencyRange(queries, 50, 60))

# get ops for queries whose latencies fall in given range
ops = getOpsForQueriesInLatencyRange(nAqueries.deinterleaved, nAops.deinterleaved, quantile(nAqueries.deinterleaved$latency_ms,0.90), quantile(nAqueries.deinterleaved$latency_ms,0.91), "needsApproval")
dim(ops)

ops = getOpsForQueriesInLatencyRange(uBHqueries.deinterleaved, uBHops.deinterleaved, 50, 55, "userByHometown")

ops = getOpsForQueriesInLatencyRange(mFqueries.deinterleaved, mFops.deinterleaved, quantile(mFqueries.deinterleaved$latency_ms,0.90), quantile(mFqueries.deinterleaved$latency_ms,0.91), "myFollowing")
dim(ops)

ops = getOpsForQueriesInLatencyRange(mTqueries.deinterleaved, mTops.deinterleaved, quantile(mTqueries.deinterleaved$latency_ms,0.90), quantile(mTqueries.deinterleaved$latency_ms,0.91), "myThoughts")
dim(ops)
col=colnames(ops)
col[2]
i=1
which(col==paste(i))
ops[1:10,which(col==paste(i))]
ops[1:10,]


# try plotting
q99 = quantile(nAqueries.deinterleaved$latency_ms, 0.99)
min=0.95*q99
max=1.05*q99
plotOpsForQueriesInLatencyRange(paste("~/Desktop/nA-", round(min), "to", round(max), sep=""), nAqueries.deinterleaved, nAops.deinterleaved, min, max, "needsApproval")


q99 = quantile(uBHqueries.deinterleaved$latency_ms, 0.99)
min=0.95*q99
max=1.05*q99
plotOpsForQueriesInLatencyRange(paste("~/Desktop/uBH-", round(min), "to", round(max), sep=""), uBHqueries.deinterleaved, uBHops.deinterleaved, min, max, "userByHometown")

q99 = quantile(mFqueries.deinterleaved$latency_ms, 0.99)
min=0.95*q99
max=1.05*q99
plotOpsForQueriesInLatencyRange(paste("~/Desktop/mF-", round(min), "to", round(max), sep=""), mFqueries.deinterleaved, mFops.deinterleaved, min, max, "myFollowing")

q99 = quantile(mTqueries.deinterleaved$latency_ms, 0.99)
min=0.95*q99
max=1.05*q99
plotOpsForQueriesInLatencyRange(paste("~/Desktop/mT-", round(min), "to", round(max), sep=""), mTqueries.deinterleaved, mTops.deinterleaved, min, max, "myThoughts")


# checking the above
q99 = quantile(nAqueries.deinterleaved$latency_ms, 0.99)
min=0.95*q99
max=1.05*q99
queries = getQueriesByLatencyRange(nAqueries.deinterleaved, min, max)
dim(queries)

whichQueries=getWhichQueriesByLatencyRange(nAqueries.deinterleaved, min, max)
length(whichQueries)
nAqueries.deinterleaved[whichQueries,]

queryNum=339
perQueryOps = getOpsForGivenQuery(nAops.deinterleaved, queryNum, "needsApproval")
sum(perQueryOps$latency_ms)

queries[1:10,]

queryOps = getOpsForQueriesInLatencyRange(nAqueries.deinterleaved, nAops.deinterleaved, min, max, "needsApproval")
dim(queryOps)
queryOps[1:10,]


# checking the plotting
path="~/Desktop/checkPlot"
queries=nAqueries.deinterleaved
ops=nAops.deinterleaved

q99 = quantile(nAqueries.deinterleaved$latency_ms, 0.99)
min=0.95*q99
max=1.05*q99

minLatency=min
maxLatency=max
queryType="needsApproval"

dim(queryOps)





