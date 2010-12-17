## 2.9.10
## Attempting to model userByEmail using the higher-level ops (rather than primitives)

data = as.data.frame(read.csv(file="~/Desktop/thread-logs2/Thread-11.csv"))

dim(data)
colnames(data)
data[1:10,]

data[data$queryNum==1,]
sum(data[data$queryNum==1 & data$opLevel==2,"latency_ms"])/data[data$queryNum==1 & data$opLevel==3,"latency_ms"] # portion of total query latency accounted for by ops (level 2)

# TODO:  get this percentage for all queries (& plot distr)

# Make models

# "prefixGet" => opLevel=2, opType=2
par(mar=c(5,5,4,2)+0.1)
h.prefixGet = hist(data[data$opLevel==2 & data$opType==2, "latency_ms"], breaks=15, xlab="Latency (ms)", main="prefixGet Latency Distribution")

h.sDI = hist(data[data$opLevel==2 & data$opType==3, "latency_ms"], breaks=15, xlab="Latency (ms)", main="sDI Latency Distribution")

h.materialize = hist(data[data$opLevel==2 & data$opType==6, "latency_ms"], breaks=15, xlab="Latency (ms)", main="materialize Latency Distribution")

# TODO:  make breaks choice fair (to differences in range) 

# NEXT:  Try sampling & cf distr(training data, sampled)

nsamples = 1000

samples = matrix(nrow=1,ncol=nsamples)

for (i in 1:nsamples) {
	pG = sample(h.prefixGet$mids, 1, replace=TRUE, prob=h.prefixGet$density)

	sDI = sample(h.sDI$mids, 1, replace=TRUE, prob=h.sDI$density)
	
	m = sample(h.materialize$mids, 1, replace=TRUE, prob=h.materialize$density)
	
	samples[i] = pG + sDI + m
}

par(mfrow=c(2,1))
hist(samples, xlim=c(0,400), breaks=25, main="Sampled Data", xlab="Query Latency (ms)")
hist(data$latency_ms[data$opLevel==3], xlim=c(0,400), breaks=25, main="Actual Data", xlab="Query Latency (ms)")




## 2.23.10
## Attempting to model the thoughtstream query

data = as.data.frame(read.csv(file="~/Desktop/2.12.10-thoughtstream-experiment/training-logs/Thread-51.csv"))

dim(data)
colnames(data)
data[1:10,]

# Look at query latency histogram
par(mar=c(5,5,4,2)+0.1)
hist(data$latency_ms[data$opLevel==3], breaks=50, xlab="Latency (ms)", main="Latency Histogram for Thoughtstream Query")

median(data$latency_ms[data$opLevel==3])  #8s

data[which(data$queryNum==1 & data$opLevel==2),]

data[which(data$queryNum==1 & data$opLevel==2 & data$opType==6),]
data[which(data$queryNum==10 & data$opLevel==2 & data$opType==6),]
data[which(data$queryNum==20 & data$opLevel==2 & data$opType==6),]

hist(data[which(data$opLevel==2 & data$opType==6),"latency_ms"], breaks=50)
hist(data[which(data$opLevel==2 & data$opType==6),"latency_ms"], breaks=100, xlim=c(0,2000))



# Get histograms for thoughtstream's ops
h1 = hist(data[data$opLevel == 2 & data$opType == 1,"latency_ms"], breaks=25)
#h2 = hist(data[data$opLevel == 2 & data$opType == 2,"latency_ms"], breaks=25)  # doesn't appear in thoughtstream query
h3 = hist(data[data$opLevel == 2 & data$opType == 3,"latency_ms"], breaks=25)
h4 = hist(data[data$opLevel == 2 & data$opType == 4,"latency_ms"], breaks=25)
h5 = hist(data[data$opLevel == 2 & data$opType == 5,"latency_ms"], breaks=25)
h6 = hist(data[data$opLevel == 2 & data$opType == 6,"latency_ms"], breaks=25)
h7 = hist(data[data$opLevel == 2 & data$opType == 7,"latency_ms"], breaks=20)
h8 = hist(data[data$opLevel == 2 & data$opType == 8,"latency_ms"], breaks=20)
h9 = hist(data[data$opLevel == 2 & data$opType == 9,"latency_ms"], breaks=20)


nsamples = 1000
samples=matrix(data=0, nrow=1, ncol=nsamples)

for (i in 1:nsamples) {
	samples[i] = samples[i] + sum(sample(h1$mids, 2, replace=TRUE, prob=h1$density))
	samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
	samples[i] = samples[i] + sum(sample(h4$mids, 2, replace=TRUE, prob=h4$density))
	samples[i] = samples[i] + sample(h5$mids, 1, replace=TRUE, prob=h5$density)
	samples[i] = samples[i] + sum(sample(h6$mids, 5, replace=TRUE, prob=h6$density))
	samples[i] = samples[i] + sample(h7$mids, 1, replace=TRUE, prob=h7$density)
	samples[i] = samples[i] + sample(h8$mids, 1, replace=TRUE, prob=h8$density)
	samples[i] = samples[i] + sample(h9$mids, 1, replace=TRUE, prob=h9$density)
}



par(mfrow=c(2,1))
hist(data$latency_ms[data$opLevel==3], breaks=50, xlab="Latency (ms)", main="Latency Histogram for Thoughtstream Query", xlim=c(0,25000))
abline(v=quantile(data$latency_ms[data$opLevel==3], 0.99), lw=2, col="green")
hist(samples, breaks=25, xlim=c(0,25000))
abline(v=quantile(samples, 0.99), lw=2, col="green")

actual99th = quantile(data$latency_ms[data$opLevel==3], 0.99)
pred99th = quantile(samples, 0.99)

abs(actual99th-pred99th)/actual99th




## Get it to read in the files for all the threads (not just one)
newdata = as.data.frame(read.csv(file="~/Desktop/2.12.10-thoughtstream-experiment/training-logs/Thread-52.csv"))

dim(data)
dim(newdata)

data1 = matrix(c(data,newdata), nrow=nrow(data)+nrow(newdata), ncol=ncol(data))
dim(data1)


## 2.23.10
##### FULL THOUGHTSTREAM EXPERIMENT!

## Collect training data
## Merging data from each tread => one "data" array
startingThread=51
endingThread=100
#logPath="~/Desktop/2.12.10-thoughtstream-experiment/training-logs"
logPath="/work/ksauer/training-logs"

data = as.data.frame(read.csv(file=paste(logPath,"/Thread-",startingThread,".csv",sep="")))

for (i in (startingThread+1):endingThread) {
	print(i)
	newdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-",i,".csv",sep="")))
	print("Read newdata")
	#data = matrix(c(data,newdata), nrow=nrow(data)+nrow(newdata), ncol=ncol(data))
	data = rbind(data, newdata)
	print("Merged old & new")
}


## Validation
startingThread=51
endingThread=100

validation99th=matrix(nrow=1,ncol=10)

# Figuring out # queries/validation run
validationNumQueries = matrix(nrow=1,ncol=10)

for (j in 1:10) {
	print(paste("Processing data for validation run", j))
	logPath=paste("/work/ksauer/2.12.10-thoughtstream-experiment/validation",j,"-logs",sep="")

	vdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-",startingThread,".csv",sep="")))
	print(dim(vdata))

	for (i in (startingThread+1):endingThread) {
		print(i)
		newdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-",i,".csv",sep="")))

		vdata = rbind(vdata, newdata)
		print(dim(vdata))
	}
	
	validationNumQueries[j] = length(which(vdata$opLevel==3))

	validation99th[j]=quantile(vdata$latency_ms[vdata$opLevel==3], 0.99)
	print(dim(vdata))
}



avgNumQueriesPerValidationRun = mean(validationNumQueries)


###
# Get histograms for thoughtstream's ops
h1 = hist(data[data$opLevel == 2 & data$opType == 1,"latency_ms"], breaks=25)
h3 = hist(data[data$opLevel == 2 & data$opType == 3,"latency_ms"], breaks=25)
h4 = hist(data[data$opLevel == 2 & data$opType == 4,"latency_ms"], breaks=25)
h5 = hist(data[data$opLevel == 2 & data$opType == 5,"latency_ms"], breaks=25)
h6 = hist(data[data$opLevel == 2 & data$opType == 6,"latency_ms"], breaks=25)
h7 = hist(data[data$opLevel == 2 & data$opType == 7,"latency_ms"], breaks=20)
h8 = hist(data[data$opLevel == 2 & data$opType == 8,"latency_ms"], breaks=20)
h9 = hist(data[data$opLevel == 2 & data$opType == 9,"latency_ms"], breaks=20)



## Getting 10 estimates for predicted latency
sampled99th=matrix(nrow=1,ncol=10)
for (j in 1:10) {
	print(j)
	#nsamples = 1000
	#nsamples=356	# avg # of queries/validation run, got by examining vdata
	nsamples = floor(avgNumQueriesPerValidationRun)
	samples=matrix(data=0, nrow=1, ncol=nsamples)

	for (i in 1:nsamples) {
		samples[i] = samples[i] + sum(sample(h1$mids, 2, replace=TRUE, prob=h1$density))
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sum(sample(h4$mids, 2, replace=TRUE, prob=h4$density))
		samples[i] = samples[i] + sample(h5$mids, 1, replace=TRUE, prob=h5$density)
		samples[i] = samples[i] + sum(sample(h6$mids, 5, replace=TRUE, prob=h6$density))
		samples[i] = samples[i] + sample(h7$mids, 1, replace=TRUE, prob=h7$density)
		samples[i] = samples[i] + sample(h8$mids, 1, replace=TRUE, prob=h8$density)
		samples[i] = samples[i] + sample(h9$mids, 1, replace=TRUE, prob=h9$density)
	}

	sampled99th[j]=quantile(samples, 0.99)	
}


#Error
error = abs(mean(validation99th)-mean(sampled99th))/mean(validation99th)
error


##### FULL userByEmail EXPERIMENT!

## Collect training data
## Merging data from each tread => one "data" array
startingThread=51
endingThread=100
#logPath="~/Desktop/2.12.10-thoughtstream-experiment/training-logs"
logPath="/work/ksauer/2.23.10-userByEmail-experiment/training-logs"

data = as.data.frame(read.csv(file=paste(logPath,"/Thread-",startingThread,".csv",sep="")))

for (i in (startingThread+1):endingThread) {
	print(i)
	newdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-",i,".csv",sep="")))
	#print("Read newdata")
	#data = matrix(c(data,newdata), nrow=nrow(data)+nrow(newdata), ncol=ncol(data))
	data = rbind(data, newdata)
	#print("Merged old & new")
}


## Validation
startingThread=51
endingThread=100

validation99th=matrix(nrow=1,ncol=10)
validationNumQueries=matrix(nrow=1,ncol=10)

for (j in 1:10) {
	logPath=paste("/work/ksauer/2.23.10-userByEmail-experiment/validation",j,"-logs",sep="")

	vdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-",startingThread,".csv",sep="")))

	for (i in (startingThread+1):endingThread) {
		print(i)
		newdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-",i,".csv",sep="")))
		print(dim(vdata))
		vdata = rbind(vdata, newdata)
		print(dim(vdata))
	}

	validationNumQueries[j] = length(which(vdata$opLevel==3))

	validation99th[j]=quantile(vdata$latency_ms[vdata$opLevel==3], 0.99)
}

avgNumQueriesPerValidationRun = floor(mean(validationNumQueries))


###
# Get histograms for userByEmail's ops
h2 = hist(data[data$opLevel == 2 & data$opType == 2,"latency_ms"], breaks=25)
h3 = hist(data[data$opLevel == 2 & data$opType == 3,"latency_ms"], breaks=25)
h6 = hist(data[data$opLevel == 2 & data$opType == 6,"latency_ms"], breaks=25)


## Getting 10 estimates for predicted latency
sampled99th=matrix(nrow=1,ncol=10)
for (j in 1:10) {
	print(j)
	#nsamples = 1000
	nsamples=avgNumQueriesPerValidationRun
	samples=matrix(data=0, nrow=1, ncol=nsamples)

	for (i in 1:nsamples) {
		samples[i] = samples[i] + sample(h2$mids, 1, replace=TRUE, prob=h2$density)
		samples[i] = samples[i] + sample(h3$mids, 1, replace=TRUE, prob=h3$density)
		samples[i] = samples[i] + sample(h6$mids, 1, replace=TRUE, prob=h6$density)
	}

	sampled99th[j]=quantile(samples, 0.99)	
}

error = abs(mean(validation99th)-mean(sampled99th))/mean(validation99th)
error


save(h2,h3,h6,file="/work/ksauer/2.23.10-userByEmail-experiment/userByEmailHists.RData")
save(data, validation99th, sampled99th, file="/work/ksauer/2.23.10-userByEmail-experiment/userByEmailExperiment.RData")
save(validation99th, sampled99th, error, file="/work/ksauer/2.23.10-userByEmail-experiment/userByEmailResults.RData")

##### FULL userByName EXPERIMENT!

rm(list=ls())

## Collect training data
## Merging data from each tread => one "data" array
startingThread=51
endingThread=100
#logPath="~/Desktop/2.12.10-thoughtstream-experiment/training-logs"
logPath="/work/ksauer/2.23.10-userByName-experiment/training-logs"

data = as.data.frame(read.csv(file=paste(logPath,"/Thread-",startingThread,".csv",sep="")))

for (i in (startingThread+1):endingThread) {
	print(i)
	newdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-",i,".csv",sep="")))
	#print("Read newdata")
	#data = matrix(c(data,newdata), nrow=nrow(data)+nrow(newdata), ncol=ncol(data))
	data = rbind(data, newdata)
	#print("Merged old & new")
}


## Validation
startingThread=51
endingThread=100

validationNumQueries=matrix(nrow=1,ncol=10)
validation99th=matrix(nrow=1,ncol=10)

for (j in 1:10) {
	logPath=paste("/work/ksauer/2.23.10-userByName-experiment/validation",j,"-logs",sep="")

	vdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-",startingThread,".csv",sep="")))

	for (i in (startingThread+1):endingThread) {
		print(i)
		newdata = as.data.frame(read.csv(file=paste(logPath,"/Thread-",i,".csv",sep="")))
		print(dim(vdata))
		vdata = rbind(vdata, newdata)
		print(dim(vdata))
	}

	validationNumQueries[j] = length(which(vdata$opLevel==3))
	validation99th[j]=quantile(vdata$latency_ms[vdata$opLevel==3], 0.99)
}

avgNumQueriesPerValidationRun = floor(mean(validationNumQueries))
avgNumQueriesPerValidationRun

###
# Get histograms for userByName's ops
h1 = hist(data[data$opLevel == 2 & data$opType == 1,"latency_ms"], breaks=25)
h6 = hist(data[data$opLevel == 2 & data$opType == 6,"latency_ms"], breaks=25)


## Getting 10 estimates for predicted latency
sampled99th=matrix(nrow=1,ncol=10)
for (j in 1:10) {
	print(j)
	#nsamples = 1000
	nsamples=avgNumQueriesPerValidationRun
	samples=matrix(data=0, nrow=1, ncol=nsamples)

	for (i in 1:nsamples) {
		samples[i] = samples[i] + sample(h1$mids, 1, replace=TRUE, prob=h1$density)
		samples[i] = samples[i] + sample(h6$mids, 1, replace=TRUE, prob=h6$density)
	}

	sampled99th[j]=quantile(samples, 0.99)	
}

error = abs(mean(validation99th)-mean(sampled99th))/mean(validation99th)
error


save(h1,h6,file="/work/ksauer/2.23.10-userByName-experiment/userByNameHists.RData")
save(data,vdata,validation99th,sampled99th,file="/work/ksauer/2.23.10-userByName-experiment/userByNameExperiment.RData")


#######

load(file="/work/ksauer/2.12.10-thoughtstream-experiment/thoughtstreamExperiment.RData")
save(validation99th, sampled99th,file="/work/ksauer/2.12.10-thoughtstream-experiment/thoughtstreamResults.RData")

load(file="/work/ksauer/2.23.10-userByEmail-experiment/userByEmailExperiment.RData")
save(validation99th, sampled99th,file="/work/ksauer/2.23.10-userByEmail-experiment/userByEmailResults.RData")




### Plotting results for FAST poster

superpose.eb <- 
+ function (x, y, ebl, ebu = ebl, length = 0.08, ...) 
+     arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3, 
+     length = length, ...)

## Thoughtstream Query
load(file="~/Desktop/thoughtstreamResults.RData")
abs(mean(sampled99th)-mean(validation99th))
abs(mean(sampled99th)-mean(validation99th))/mean(validation99th)

ub.sampled = max(sampled99th)-mean(sampled99th)
lb.sampled = mean(sampled99th)-min(sampled99th)

ub.actual = max(validation99th)-mean(validation99th)
lb.actual = mean(validation99th)-min(validation99th)

pdf(file="~/Desktop/thoughtstream-results.pdf", height=6, width=6)
par(mar=c(5,5,4,2)+0.1)
x.abscissa = barplot(c(mean(sampled99th),mean(validation99th)), ylim=c(0,20000), names.arg=c("Sampled", "Actual"), col=c("blue","cyan"), ylab="Latency (ms)", main="Thoughtstream Query:  Sampled vs. Actual Latency")
superpose.eb(x.abscissa,c(mean(sampled99th),mean(validation99th)),c(lb.sampled,lb.actual),c(ub.sampled,ub.actual),col="green",lwd=2)
dev.off()

## userByEmail query
load(file="~/Desktop/userByEmailResults.RData")
abs(mean(sampled99th)-mean(validation99th))
abs(mean(sampled99th)-mean(validation99th))/mean(validation99th)

ub.sampled = max(sampled99th)-mean(sampled99th)
lb.sampled = mean(sampled99th)-min(sampled99th)

ub.actual = max(validation99th)-mean(validation99th)
lb.actual = mean(validation99th)-min(validation99th)

pdf(file="~/Desktop/userByEmail-results.pdf", height=6, width=6)
par(mar=c(5,5,4,2)+0.1)
x.abscissa = barplot(c(mean(sampled99th),mean(validation99th)), names.arg=c("Sampled", "Actual"), col=c("blue","cyan"), ylab="Latency (ms)", main="userByEmail Query:  Sampled vs. Actual Latency", ylim=c(0,1300))
superpose.eb(x.abscissa,c(mean(sampled99th),mean(validation99th)),c(lb.sampled,lb.actual),c(ub.sampled,ub.actual),col="green",lwd=2)
dev.off()


## userByName query
load(file="~/Desktop/userByNameResults.RData")
abs(mean(sampled99th)-mean(validation99th))
abs(mean(sampled99th)-mean(validation99th))/mean(validation99th)

ub.sampled = max(sampled99th)-mean(sampled99th)
lb.sampled = mean(sampled99th)-min(sampled99th)

ub.actual = max(validation99th)-mean(validation99th)
lb.actual = mean(validation99th)-min(validation99th)

pdf(file="~/Desktop/userByName-results.pdf", height=6, width=6)
par(mar=c(5,5,4,2)+0.1)
x.abscissa = barplot(c(mean(sampled99th),mean(validation99th)), names.arg=c("Sampled", "Actual"), col=c("blue","cyan"), ylab="Latency (ms)", main="userByName Query:  Sampled vs. Actual Latency", ylim=c(0,90))
superpose.eb(x.abscissa,c(mean(sampled99th),mean(validation99th)),c(lb.sampled,lb.actual),c(ub.sampled,ub.actual),col="green",lwd=2)
dev.off()



## 3.9.10
## Checking out thoughtsByHashTag data

data = as.data.frame(read.csv(file="~/Desktop/validation1-logs/Thread-51.csv"))

data[1:10,]
dim(data)

numqueries = length(which(data$opLevel==3))

data[data$queryNum==1 & data$opLevel==2,]

hist(data[data$opLevel==3,"latency_ms"], breaks=10)











