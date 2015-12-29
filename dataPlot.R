
plotData <- function(x,y,xlab,ylab,col=col){
  plot(x, y, xlab="", ylab="", col=col, xlim=c(0,140), ylim=c(0,100), type="b")
  par(new=T)
}

# data
data.num = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140)
independence.inc = c(88, 69, 60, 55, 51, 47, 44, 42, 40, 38, 36,35, 33, 32)
independence.dec = c(10,15, 9, 6, 3, 2.7, 1.75, 0.93, 0.63, 0.38, 0.25, 0.15, 0.099, 0.065)
dependence.inc = c(0,0, 0, 0, 0, 0.07, 0.16, 0.34, 0.71, 0.95, 1.18,1.52, 1.769, 1.98)
dependence.dec = c(0,15, 29, 38, 45, 49, 53, 56, 58, 60, 61, 63, 64, 65)


plotData(data.num, independence.inc, "Data Size", "independence inc","red")
plotData(data.num, independence.dec, "Data Size", "independence dec", "blue")
plotData(data.num, dependence.inc, "Data Size", "dependence inc", "black")
plotData(data.num, dependence.dec, "Data Size", "dependence dec", "yellow")
