# OLD checking code (from GMM testing)

plotCVLogLikelihoodVsNumMixtureComponents(path="~/Desktop/op3-cv-loglik-large", mixtureComponentsRange=c(seq(from=2,by=1,to=7),100))


load("~/Desktop/ops.RData")
kFoldCVToChooseNumMixtureComponentsForGMM(dataVector=op1$latency_ms, mixtureComponentsRange=seq(from=2,by=1,to=15), destinationPath="~/Desktop/op1-cv")
plotCVLogLikelihoodVsNumMixtureComponents(path="~/Desktop/op1-cv", mixtureComponentsRange=seq(from=2,by=1,to=15))


#for (i in c(2,seq(from=4,by=1,to=9))) {
for (i in c(7,8,9)) {
	if (i == 2) {
		op = op2
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
		op = 0
	}

	kFoldCVToChooseNumMixtureComponentsForGMM(dataVector=op$latency_ms, mixtureComponentsRange=seq(from=2,by=1,to=15), destinationPath=paste("~/Desktop/op", i, "-cv", sep=""))
	plotCVLogLikelihoodVsNumMixtureComponents(path=paste("~/Desktop/op", i, "-cv", sep=""), mixtureComponentsRange=seq(from=2,by=1,to=15))
	
}


# On EC2
load("ops.RData")
op=op9
i=9
path="~/Desktop"
kFoldCVToChooseNumMixtureComponentsForGMM(dataVector=op$latency_ms, mixtureComponentsRange=seq(from=2,by=1,to=15), destinationPath=paste(path, "/op", i, "-cv", sep=""))
plotCVLogLikelihoodVsNumMixtureComponents(path=paste(path, "/op", i, "-cv", sep=""), mixtureComponentsRange=2)





load("~/Desktop/op6-cv/cvLogLikelihood-2mixtureComponents.RData")
ls()
logLikelihood



## 8.3.10
# Checking CV for fitting MM to data

source("fitGeneralMMUsingCV.R")

# checking "splitDataIntoKFolds"
data=sample(10,30,replace=TRUE)
foldAssignmentVector=splitDataIntoKFolds(data, 4)
length(which(foldAssignmentVector==1))
length(which(foldAssignmentVector==2))
length(which(foldAssignmentVector==3))
length(which(foldAssignmentVector==4))

# checking "fitMMToTrainingDataAndReturnValidationLikelihood"
loglik=fitMMToTrainingDataAndReturnValidationLikelihood(data, c(1,4,5,6,9), 3, "normal", "evenlySpacedByQuantile")
loglik

# checking "findAvgLogLikelihoodViaCVFixedNumMixtureComponents"
loglik= findAvgLogLikelihoodViaCVFixedNumMixtureComponents(data, 3, 3, "normal", "evenlySpacedByQuantile")

# checking "kFoldCVToChooseNumMixtureComponentsForMM"
numMixtures = kFoldCVToChooseNumMixtureComponentsForMM(data, "normal", "evenlySpacedByQuantile", "~/Desktop/testDir")


# now try for real
kFoldCVToChooseNumMixtureComponentsForMM(op3$latency_ms, "gamma", "evenlySpacedByQuantile", "~/Desktop/op3Gamma")

# try on R cluster
dataPath="/work/ksauer/8.3.10-cv"
load(paste(dataPath, "/ops.RData", sep=""))

setwd("/work/ksauer/scads/experiments/client/performance/logparsing/src/main/R")
source("fitGeneralMMUsingCV.R")

op=op5
opNum=5

kFoldCVToChooseNumMixtureComponentsForMM(op$latency_ms, "normal", "evenlySpacedByQuantile", paste(dataPath, "/op", opNum, "Normal", sep=""))
kFoldCVToChooseNumMixtureComponentsForMM(op$latency_ms, "exponential", "evenlySpacedByQuantile", paste(dataPath, "/op", opNum, "Exponential", sep=""))
kFoldCVToChooseNumMixtureComponentsForMM(op$latency_ms, "gamma", "evenlySpacedByQuantile", paste(dataPath, "/op", opNum, "Gamma", sep=""))
kFoldCVToChooseNumMixtureComponentsForMM(op$latency_ms, "weibull", "evenlySpacedByQuantile", paste(dataPath, "/op", opNum, "Weibull", sep=""))

# try to plot
plotCVLogLikelihoodVsNumMixtureComponents(paste(dataPath, "/op", opNum, "Normal", sep=""))
plotCVLogLikelihoodVsNumMixtureComponents(paste(dataPath, "/op", opNum, "Exponential", sep=""), mixtureComponentsRange=seq(from=2, to=9, by=1))

# make all plots
maxMixtures = matrix(nrow=5, ncol=4)
rownames(maxMixtures)=c("op1", "op2", "op3", "op4", "op5")
colnames(maxMixtures)=c("normal", "exponential", "gamma", "weibull")
maxMixtures[1,] = c(9,9,7,9)
maxMixtures[2,] = c(9,6,9,9)
maxMixtures[3,] = c(9,9,9,9)
maxMixtures[4,] = c(9,8,9,9)
maxMixtures[5,] = c(9,9,9,9)

for (opNum in 1:5) {
	plotCVLogLikelihoodVsNumMixtureComponents(paste(dataPath, "/op", opNum, "Normal", sep=""), mixtureComponentsRange=seq(from=2, to=maxMixtures[opNum,1], by=1))
	plotCVLogLikelihoodVsNumMixtureComponents(paste(dataPath, "/op", opNum, "Exponential", sep=""), mixtureComponentsRange=seq(from=2, to=maxMixtures[opNum,2], by=1))
	plotCVLogLikelihoodVsNumMixtureComponents(paste(dataPath, "/op", opNum, "Gamma", sep=""), mixtureComponentsRange=seq(from=2, to=maxMixtures[opNum,3], by=1))
	plotCVLogLikelihoodVsNumMixtureComponents(paste(dataPath, "/op", opNum, "Weibull", sep=""), mixtureComponentsRange=seq(from=2, to=maxMixtures[opNum,4], by=1))
}


