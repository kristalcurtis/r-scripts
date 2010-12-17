plotKeyQuantilesAndLegend = function(distr, quantiles, colorsVtr, legendPos="topright", numDigits=2, legendLineWidth=2) {
	legendText = matrix(nrow=1,ncol=length(quantiles))
	
	for (i in 1:length(quantiles)) {
		abline(v=quantile(distr, quantiles[i]), col=colorsVtr[i], lw=2)
		legendText[i] = paste(quantiles[i]*100, "th %ile = ", round(quantile(distr, quantiles[i]), digits=numDigits), "ms", sep="")	
	}
	
	legend(legendPos, col=colorsVtr, legend=legendText, lwd=legendLineWidth)
}


plotMed90th99thQuantilesAndLegend = function(distr) {
	plotKeyQuantilesAndLegend(distr, c(0.5, 0.9, 0.99), c("red", "green", "blue"))
}
