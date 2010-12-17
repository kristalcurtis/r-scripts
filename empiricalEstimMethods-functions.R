createExtremeHistogramForRawData = function(rawData) {
	mids = sort(rawData)
	density = rep(1/length(rawData), length(rawData))
	
	return(list(mids=mids, density=density))
}