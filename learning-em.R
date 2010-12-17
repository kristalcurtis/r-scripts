library(mclust)

?em

iris

# Initialize
msEst = mstep(modelName="EEE", data = iris[,-5,drop=F], z=unmap(iris[,5]))
?mclustModelNames
?unmap
names(msEst)
msEst$modelName

# Run EM
emRun = em(modelName = msEst$modelName, data=iris[,-5], parameters=msEst$parameters)
names(emRun)
emRun$z # => how to classify results
emRun$parameters

?mclustVariance


# try sampling
library(MASS)
?mvrnorm
points1 = mvrnorm(100, mu=emRun$parameters$mean[,1], Sigma=emRun$parameters$variance$Sigma)
points2 = mvrnorm(100, mu=emRun$parameters$mean[,2], Sigma=emRun$parameters$variance$Sigma)
points3 = mvrnorm(100, mu=emRun$parameters$mean[,3], Sigma=emRun$parameters$variance$Sigma)

dim(points1)

par(mfrow=c(1,2))
plot(iris[,1:2], col=iris[,5])

plot(points1, xlim=c(4,8), ylim=c(2,4.5), col=0)
points(points1, col="black")
points(points2, col="red")
points(points3, col="green")

# But, here you have labels.  How would you initialize w/o labels?
# Random restarts to avoid local optima
# Could try above with random initializations of 'z' vector (pass in uninformative vtr to z rather than actual labels)
rdmInit = matrix(data=sample(seq(from=1, to=10), nrow(iris)*3, replace=TRUE, prob=rep.int(1/10,10)), nrow=nrow(iris), ncol=3)
for (i in 1:nrow(rdmInit)) {
	rdmInit[i,] = rdmInit[i,]/sum(rdmInit[i,])
}
rdmInit

dim(iris)
#msEst.rdmInit = mstep(modelName="EEE", data = iris[,-5], z=matrix(data=0.33, nrow=nrow(iris), ncol=3))  # gives all same means
msEst.rdmInit = mstep(modelName="EEE", data = iris[,-5], z=rdmInit)
emRun.rdmInit = em(modelName = msEst.rdmInit$modelName, data=iris[,-5], parameters=msEst.rdmInit$parameters)

emRun$parameters$mean
emRun.rdmInit$parameters$mean



# Attempting a different approach to random init
unique(iris[,5])
k = ncol(unmap(iris[,5]))

rdmInitVtr = matrix(nrow=nrow(iris), ncol=1)
for(i in 1:nrow(rdmInitVtr)) {
	rdmInitVtr[i] = sample(x=seq(from=1, by=1, to=k), size=1, prob=rep(1/k, k))
}
rdmInitVtr

rdmInitMtx = unmap(rdmInitVtr)

msEst.rdmInit2 = mstep(modelName="EEE", data=iris[,-5], z=rdmInitMtx)
emRun.rdmInit2 = em(modelName=msEst.rdmInit2$modelName, data=iris[,-5], parameters=msEst.rdmInit2$parameters)

msEst.rdmInit2 = mstep(modelName="EEE", data=iris[,1], z=rdmInitMtx)
emRun.rdmInit2 = em(modelName=msEst.rdmInit2$modelName, data=iris[,1], parameters=msEst.rdmInit2$parameters)


emRun$parameters$mean
emRun.rdmInit2$parameters$mean


# try multiple rdm inits
for(i in 1:5) {
	print(paste("Run", i, "..."))
	rdmInitVtr = matrix(nrow=nrow(iris), ncol=1)
	for(i in 1:nrow(rdmInitVtr)) {
		rdmInitVtr[i] = sample(x=seq(from=1, by=1, to=k), size=1, prob=rep(1/k, k))
	}

	rdmInitMtx = unmap(rdmInitVtr)

	msEst.rdmInit2 = mstep(modelName="EEE", data=iris[,-5], z=rdmInitMtx)
	emRun.rdmInit2 = em(modelName=msEst.rdmInit2$modelName, data=iris[,-5], 	parameters=msEst.rdmInit2$parameters)

	print(emRun.rdmInit2$parameters$mean)
}


# Sample from mixture model
k=length(unique(iris[,5]))
numSamples=1000

mixComponents = matrix(nrow=numSamples, ncol=1)
irisSamples = matrix(nrow=numSamples, ncol=ncol(iris[,-5])) 
colnames(irisSamples) = colnames(iris[,-5])

for(i in 1:numSamples) {
	mixComponents[i] = sample(x=seq(from=1, by=1, to=k), size=1, prob=emRun$pro)
	
	componentMean = emRun$parameters$mean[, mixComponents[i]]
	componentVariance = emRun$parameters$variance$Sigma
	
	irisSamples[i,] = mvrnorm(n=1, mu=componentMean, Sigma=componentVariance)
}


irisSamples[1:10,]
colMeans(irisSamples[which(mixComponents==1),])
colMeans(irisSamples[which(mixComponents==2),])
colMeans(irisSamples[which(mixComponents==3),])

# Cool, per-mixing-component params from sampled data are very close to those from gold data


# Trying EM on univariate data

#irisMtx = matrix(data=c(iris[,1], iris[,1]), ncol=2, nrow=nrow(iris))
#irisMtx = matrix(data=c(iris[,1], rep(1,nrow(iris))), ncol=2, nrow=nrow(iris))
#irisMtx[1:10,]
#dim(irisMtx)
#msEst = mstep(modelName="EEE", data = as.matrix(irisMtx), z=unmap(iris[,5]))
msEst = mstep(modelName="EEE", data = iris[,1:2,drop=FALSE], z=unmap(iris[,5]))
emRun = em(modelName=msEst$modelName, data=irisMtx, parameters=msEst$parameters)


msEst$parameters