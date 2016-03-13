library(bnlearn)
library(ggplot2)

getSampleData <- function(data, data.length, random.length){
  random.index = sort(floor(runif(random.length) * data.length))
  random.val = data[random.index,]
  #  rownames(random.val) = c(1:random.length)
  return(random.val)
}

compareBN <- function(data, data.size.vec, res.correct, try.size){
  tp.num.vec = c()
  fp.num.vec = c()
  fn.num.vec = c()
  for(data.size in data.size.vec){
    tp.sum = 0
    fp.sum = 0
    fn.sum = 0
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), data.size)
      res.learn = hc(sample.data)
      diff.table = compare(target=res.correct, current=res.learn, arcs=FALSE)
      tp.sum = tp.sum + diff.table$tp
      fp.sum = fp.sum + diff.table$fp
      fn.sum = fn.sum + diff.table$fn
    }
    tp.num.vec = append(tp.num.vec, tp.sum / try.size)
    fp.num.vec = append(fp.num.vec, fp.sum / try.size)
    fn.num.vec = append(fn.num.vec, fn.sum / try.size)
  }
  return(list(tp.num.vec, fp.num.vec, fn.num.vec))
}

correctBnlearn <- function(data, correct.graph.string){
  res.correct = empty.graph(names(data))
  modelstring(res.correct) = correct.graph.string
  
  graphviz.plot(res.correct)
  return(res.correct)
}

plotRateAboutTPFP <- function(data.size.vec, tp.vec, fp.vec, try.size, data.name){
  rate.tp.vec = c()
  rate.fp.vec = c()
  for(i in 1:length(tp.vec)){
    rate.tp.vec = append(rate.tp.vec, 100 * (tp.vec[i] / (tp.vec[i] + fp.vec[i])))
    rate.fp.vec = append(rate.fp.vec, 100 * (fp.vec[i] / (tp.vec[i] + fp.vec[i])))
  }
  rate.data = data.frame(x = data.size.vec, y = rate.tp.vec)
  g = ggplot(
    rate.data,
    aes (
      x = data.size.vec,
      y = rate.tp.vec
    )
  )
  
  g = g + geom_line(
    color = "red",
    linetype = 1,
    size = 1
  )
  
  g = g + xlab("Data size") + ylab("TP rate")+ ggtitle(paste(data.name, "TP rate"))
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec)))
  g = g + theme_bw()
  ggsave(paste(data.name, ".wmf"), g)
  plot(g)
  
  
  #plot(data.size.vec, rate.tp.vec,xilm=c(0, max(data.size.vec)), ylim=c(0, 100), xlab="", ylab="", col="red", type="b")
  #par(new=T)
  #title(paste("try.size : ", try.size, " tp-rate:red fp-rate:blue"))
  #par(new=T)
  #plot(data.size.vec, rate.fp.vec,xilm=c(0, max(data.size.vec)), ylim=c(0, 100),xlab="data size", ylab="rate", col="blue", type="b")
  #par(new=F)
}

plotArcNum <- function(data.size.vec, tp.vec, fp.vec, fn.vec, try.size){
  plot(data.size.vec, tp.vec, xlim=c(0, max(data.size.vec)) , ylim=c(0, max(c(tp.vec, fp.vec, fn.vec))) ,col="blue", type="b", xlab="", ylab="")
  par(new=T)
  plot(data.size.vec, fp.vec, xlim=c(0, max(data.size.vec)) , ylim=c(0, max(c(tp.vec, fp.vec, fn.vec))) ,col="red", type="b", xlab="", ylab="")
  par(new=T)
  plot(data.size.vec, fn.vec, xlim=c(0, max(data.size.vec)) , ylim=c(0, max(c(tp.vec, fp.vec, fn.vec))) ,col="black", type="b", xlab="vector num", ylab="data size")
  par(new=T)
  title(paste("try size : ", try.size, "tp:blue, fp:red, fn:black"))
  par(new=F)
  
}

main <- function(){
  data(alarm)
  data.size.vec = seq(10,100,by=10)
  try.size = 50
  correct.graph.string = paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
                               "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA]",
                               "[HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK]",
                               "[MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB]",
                               "[SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS]",
                               "[VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV]",
                               "[CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]", sep = "")
  res.correct = correctBnlearn(alarm, correct.graph.string)
  result = compareBN(alarm, data.size.vec, res.correct, try.size)
  plotArcNum(data.size.vec, result[[1]], result[[2]], result[[3]], try.size)
  plotRateAboutTPFP(data.size.vec, result[[1]], result[[2]], try.size, "alarm")
  
  data(insurance)
  data.size.vec = seq(10,100,by=10)
  try.size = 50
  correct.graph.string = paste("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
                               "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon][VehicleYear|SocioEcon:RiskAversion]",
                               "[MakeModel|SocioEcon:RiskAversion][SeniorTrain|Age:RiskAversion]",
                               "[HomeBase|SocioEcon:RiskAversion][AntiTheft|SocioEcon:RiskAversion]",
                               "[RuggedAuto|VehicleYear:MakeModel][Antilock|VehicleYear:MakeModel]",
                               "[DrivingSkill|Age:SeniorTrain][CarValue|VehicleYear:MakeModel:Mileage]",
                               "[Airbag|VehicleYear:MakeModel][DrivQuality|RiskAversion:DrivingSkill]",
                               "[Theft|CarValue:HomeBase:AntiTheft][Cushioning|RuggedAuto:Airbag]",
                               "[DrivHist|RiskAversion:DrivingSkill][Accident|DrivQuality:Mileage:Antilock]",
                               "[ThisCarDam|RuggedAuto:Accident][OtherCarCost|RuggedAuto:Accident]",
                               "[MedCost|Age:Accident:Cushioning][ILiCost|Accident]",
                               "[ThisCarCost|ThisCarDam:Theft:CarValue][PropCost|ThisCarCost:OtherCarCost]",
                               sep = "")
  res.correct = correctBnlearn(insurance, correct.graph.string)
  result = compareBN(insurance, data.size.vec, res.correct, try.size)
  plotArcNum(data.size.vec, result[[1]], result[[2]], result[[3]], try.size)
  plotRateAboutTPFP(data.size.vec, result[[1]], result[[2]], try.size, "insurance")
  
  data(asia)
  data.size.vec = seq(10,100,by=10)
  try.size = 50
  correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
  res.correct = correctBnlearn(asia, correct.graph.string)
  result = compareBN(asia, data.size.vec, res.correct, try.size)
  plotArcNum(data.size.vec, result[[1]], result[[2]], result[[3]], try.size)
  plotRateAboutTPFP(data.size.vec, result[[1]], result[[2]], try.size, "asia")
}


main()



