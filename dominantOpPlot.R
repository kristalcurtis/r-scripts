## 8.3.10
# identify dominant op(s) for each query

# first: how to make a stacked bar chart?

?barplot
require(grDevices)
tN = table(Ni = stats::rpois(100,lambda=5))
lines(r, tN, type="h", col="red", lwd=2)

barplot(tN, space=1.5, axisnames=FALSE, sub="barplot(..., space=1.5, axisnames=FALSE)")

barplot(VADeaths, col=c("blue", "turquoise", "blue4", "green"))