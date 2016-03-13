library(ggplot2)

xval = round(runif(10)*10,0)
yval = round(runif(10)*10,0)
data = data.frame(x = xval, y = yval)

g = ggplot(
  data,
  aes (
    x = xval,
    y = yval
  )
)

g = g + geom_hline(yintercept=0.05, linetype="dashed" ,colour="red")
g = g + geom_vline(xintercept=0,linetype="dashed" , colour="red")
g = g + xlab("Score.diff") + ylab("p value")+ ggtitle("Contradict Region")
g = g + coord_cartesian(xlim = c(-3, 3), ylim=c(0, 1))

ggsave(paste("Contradict Region", ".wmf"), g)
plot(g)

