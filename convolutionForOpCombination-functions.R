getQuantileFromDensity = function(d, desiredQuantile) {
	totalArea = sum(rep.int(d$x[2]-d$x[1],times=length(d$x))*d$y)

	# get quantile value at each bin
	#quantiles = vector(length=maxBin)
	#quantiles = vector(length=length(d$x))

#	for (bin in 1:length(d$x)) {
#		leftOfBin = sum(rep.int(d$x[2]-d$x[1],times=bin)*d$y[1:bin])
#	
#		quantiles[bin] = leftOfBin/totalArea
#	}

	quantiles = apply(matrix(data=1:length(d$x), nrow=length(d$x), ncol=1), 1, computeAreaLeftOfBin, d$x[2]-d$x[1], d$y)/totalArea
	
	# return value at bin whose quantile value is closest to desired quantile
	values = vector(length=length(desiredQuantile))
	
	for (i in 1:length(desiredQuantile)) {
		diffFromDesiredQuantile = abs(quantiles - desiredQuantile[i])
		bestBin=which.min(diffFromDesiredQuantile)
		#print(bestBin)
		values[i] = d$x[bestBin]
	}
	
	return(values)
}


computeAreaLeftOfBin = function(bin, binWidth, binHeights) {
	return(sum(rep.int(binWidth, times=bin)*binHeights[1:bin]))
}


## DEPRECATED
getMultipleQuantilesFromDensity = function(d, quantiles) {
	values = vector(length=length(quantiles))
	
	for (i in 1:length(quantiles)) {
		values[i] = getQuantileFromDensity(d, quantiles[i])
	}
	
	return(values)
}


# n is a param for the "density" command
#getQueryDensityViaConvolution = function(ops, queryType, n) {
getQueryDensityViaConvolution = function(opDensityList, queryType) {
	if (queryType == "needsApproval") {
		d = getNeedsApprovalDensityViaConvolution(opDensityList)
	} else if (queryType == "userByHometown") {
		d = getUserByHometownDensityViaConvolution(opDensityList)
	} else if (queryType == "myFollowing") {
		d = getMyFollowingDensityViaConvolution(opDensityList)
	} else if (queryType == "myThoughts") {
		d = getMyThoughtsDensityViaConvolution(opDensityList)
	} else {
		print("Unsupported query type.")
	}
	
	return(d)
}

getNeedsApprovalDensityViaConvolution = function(opDensityList) {
	# convolve ops => unnormalized query density
	conv.aligned = convolve(opDensityList$d1$y, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6
	conv.aligned = convolve(conv.aligned, opDensityList$d1$y, type="circular", conj=FALSE) # 1, 6, 1
	conv.aligned = convolve(conv.aligned, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6, 1, 6
	conv.aligned = convolve(conv.aligned, opDensityList$d4$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4
	conv.aligned = convolve(conv.aligned, opDensityList$d3$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4, 3
	conv.aligned = convolve(conv.aligned, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4, 3, 6
	conv.aligned = convolve(conv.aligned, opDensityList$d9$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4, 3, 6, 9
	
	# normalize query density
	normalization=sum(rep.int(opDensityList$d1$x[2]-opDensityList$d1$x[1],times=length(opDensityList$d1$x))*conv.aligned)
	convDensity = list(x=opDensityList$d1$x, y=conv.aligned/normalization)	
	return(convDensity)
}


getNeedsApprovalDensityViaConvolutionGivenOps = function(ops, n) {
	# align ops
	from=0
	to=max(ops$op1, ops$op3, ops$op4, ops$op6, ops$op9)
	
	d1.aligned = density(ops$op1, from=from, to=to, n=n)
	d3.aligned = density(ops$op3, from=from, to=to, n=n)
	d4.aligned = density(ops$op4, from=from, to=to, n=n)
	d6.aligned = density(ops$op6, from=from, to=to, n=n)
	d9.aligned = density(ops$op9, from=from, to=to, n=n)

	return(getNeedsApprovalDensityViaConvolution(list(d1=d1.aligned, d3=d3.aligned, d4=d4.aligned, d6=d6.aligned, d9=d9.aligned)))
}


getUserByHometownDensityViaConvolution = function(opDensityList) {
	# convolve ops => unnormalized query density
	conv.aligned = convolve(opDensityList$d2$y, opDensityList$d3$y, type="circular", conj=FALSE)
	convUBH.aligned = convolve(conv.aligned, opDensityList$d6$y, type="circular", conj=FALSE)
	
	# normalize query density
	normalization=sum(rep.int(opDensityList$d2$x[2]-opDensityList$d2$x[1],times=length(opDensityList$d2$x))*convUBH.aligned)
	convDensityUBH = list(x=opDensityList$d2$x, y=convUBH.aligned/normalization)

	return(convDensityUBH)
}


getUserByHometownDensityViaConvolutionGivenOps = function(ops, n) {
	# align ops
	from=0
	to=max(ops$op2, ops$op3, ops$op6)

	d2.aligned = density(ops$op2, from=from, to=to, n=n)
	d3.aligned = density(ops$op3, from=from, to=to, n=n)
	d6.aligned = density(ops$op6, from=from, to=to, n=n)

	return(getUserByHometownDensityViaConvolution(list(d2=d2.aligned, d3=d3.aligned, d6=d6.aligned)))
}


getMyFollowingDensityViaConvolution = function(opDensityList) {
	# convolve ops => unnormalized query density
	conv.aligned = convolve(opDensityList$d1$y, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6
	conv.aligned = convolve(conv.aligned, opDensityList$d1$y, type="circular", conj=FALSE) # 1, 6, 1
	conv.aligned = convolve(conv.aligned, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6, 1, 6
	conv.aligned = convolve(conv.aligned, opDensityList$d4$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4
	conv.aligned = convolve(conv.aligned, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4, 6
	conv.aligned = convolve(conv.aligned, opDensityList$d5$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4, 6, 5
	conv.aligned = convolve(conv.aligned, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4, 6, 5, 6

	# normalize query density
	normalization=sum(rep.int(opDensityList$d1$x[2]-opDensityList$d1$x[1],times=length(opDensityList$d1$x))*conv.aligned)
	convDensity = list(x=opDensityList$d1$x, y=conv.aligned/normalization)	
	return(convDensity)	
}


getMyFollowingDensityViaConvolutionGivenOps = function(ops, n) {
	# align ops
	from=0
	to=max(ops$op1, ops$op4, ops$op5, ops$op6)
	
	d1.aligned = density(ops$op1, from=from, to=to, n=n)
	d4.aligned = density(ops$op4, from=from, to=to, n=n)
	d5.aligned = density(ops$op5, from=from, to=to, n=n)
	d6.aligned = density(ops$op6, from=from, to=to, n=n)

	return(getMyFollowingDensityViaConvolution(list(d1=d1.aligned, d4=d4.aligned, d5=d5.aligned, d6=d6.aligned)))
}


getMyThoughtsDensityViaConvolution = function(opDensityList) {
	# convolve ops => unnormalized query density
	conv.aligned = convolve(opDensityList$d1$y, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6
	conv.aligned = convolve(conv.aligned, opDensityList$d1$y, type="circular", conj=FALSE) # 1, 6, 1
	conv.aligned = convolve(conv.aligned, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6, 1, 6
	conv.aligned = convolve(conv.aligned, opDensityList$d4$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4
	conv.aligned = convolve(conv.aligned, opDensityList$d6$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4, 6
	conv.aligned = convolve(conv.aligned, opDensityList$d9$y, type="circular", conj=FALSE) # 1, 6, 1, 6, 4, 6, 9

	# normalize query density
	normalization=sum(rep.int(opDensityList$d1$x[2]-opDensityList$d1$x[1],times=length(opDensityList$d1$x))*conv.aligned)
	convDensity = list(x=opDensityList$d1$x, y=conv.aligned/normalization)	
	return(convDensity)	
}


getMyThoughtsDensityViaConvolutionGivenOps = function(ops, n) {
	# align ops
	from=0
	to=max(ops$op1, ops$op4, ops$op6, ops$op9)
	
	d1.aligned = density(ops$op1, from=from, to=to, n=n)
	d4.aligned = density(ops$op4, from=from, to=to, n=n)
	d6.aligned = density(ops$op6, from=from, to=to, n=n)
	d9.aligned = density(ops$op9, from=from, to=to, n=n)

	return(getMyThoughtsDensityViaConvolution(list(d1=d1.aligned, d4=d4.aligned, d6=d6.aligned, d9=d9.aligned)))
}



