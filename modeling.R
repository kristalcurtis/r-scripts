op=9

data = as.data.frame(read.csv(file=paste("op",op,".csv", sep="")))
print(dim(data))
#data[1:10,]

#firstBin = data[data$aSize==10 & data$bSize==10 & data$numA==10 & data$numB==10,]
#dim(firstBin)

#h = hist(firstBin$latency_ms, breaks=25)

steps = seq(from=10, by=30, to=100)

for (i in steps) {
	for (j in steps) {
		for (k in steps) {
			for (l in steps) {
				print(paste("i=", i, ",j=", j, ",k=", k, ",l=", l, sep=""))
				bin = data[data$aSize==i & data$bSize==j & data$numA==k & data$numB==l,]
				h = hist(bin$latency_ms, breaks=25)
				save(bin, h, file=paste("i=", i, ",j=", j, ",k=", k, ",l=", l, ".RData", sep=""))
			}
		}
	}
}

