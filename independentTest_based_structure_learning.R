library(bnlearn)
library(ggplot2)

plotFPResult <- function(data.size.vec, data.name, fp.avg.vec, fp.sd.vec=NULL, flag.error.bar = FALSE){
  df = data.frame(id = data.size.vec, fp.avg = fp.avg.vec)
  if(flag.error.bar){
    data.sd = data.frame(fp.sd = fp.sd.vec)
    df = cbind(df, data.sd)
  }
  g = ggplot(
    df,
    aes(
      x = id,
      y = fp.avg
    )
  )
  if(flag.error.bar){
    g = g + geom_errorbar(aes(x=id, y=fp.avg, ymin=fp.avg-fp.sd, ymax=fp.avg + fp.sd), width=1)
    g = g + geom_point()
  }
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec))) + ggtitle(paste("fp-avg-Data :",data.name))
  plot(g)
}

plotPrecisionResult <- function(data.size.vec, data.name, precision, precision.sd.vec=NULL, flag.error.bar=FALSE){
  df = data.frame(id = data.size.vec, precision = precision)
  if(flag.error.bar){
    data.sd = data.frame(precision.sd = precision.sd.vec)
    df = cbind(df, data.sd)
  }
  g = ggplot(
    df,
    aes(
      x = id,
      y = precision
    )
  )
  if(flag.error.bar){
    g = g + geom_errorbar(aes(x=id, y=precision, ymin=precision-precision.sd, ymax=precision + precision.sd), width=1)
    g = g + geom_point()
  }
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec))) + ggtitle(paste("Precision-Data :",data.name))
  plot(g)
}

plotRecallResult <- function(data.size.vec, data.name, recall, recall.sd.vec=NULL, flag.error.bar=FALSE){
  df = data.frame(id = data.size.vec, recall = recall)
  if(flag.error.bar){
    data.sd = data.frame(recall.sd = recall.sd.vec)
    df = cbind(df, data.sd)
  }
  g = ggplot(
    df,
    aes(
      x = id,
      y = recall
    )
  )
  if(flag.error.bar){
    g = g + geom_errorbar(aes(x=id, y=recall, ymin=recall - recall.sd, ymax=recall + recall.sd), width=1)
    g = g + geom_point()
  }
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec))) + ggtitle(paste("Recall-Data :",data.name))
  plot(g)
}

plotPrecisionRecallResult <- function(precision.vec, recall.vec, data.name){
  df = data.frame(x = recall.vec, y = precision.vec)
  g = ggplot(
    df,
    aes (
      x = x,
      y = y
    )
  )
  g = g + geom_line() + geom_point()
  g = g + coord_cartesian(xlim = c(0,1), ylim = c(0,1))
  g = g + xlab("recall") + ylab("precision") + ggtitle(paste("Precision-Recall Data :", data.name))
  plot(g)
}


getTableData <- function(data){
  data1 = data[,1]
  data2 = data[,2]
  data.vec = c()
  for(i in 1:length(levels(data1))){
    for(j in 1:length(levels(data2))){
      len = length(data[data[,1] == levels(data1)[i] & data[,2] == levels(data2)[j], ][,1])
      data.vec = append(data.vec, len)
    }
  }
  data.table = matrix(data.vec, nrow=length(levels(data1)), byrow=T, dimnames=list(levels(data1), levels(data2)))
  return (data.table)
}

getStructureWithFisher <- function(data, p.value=0.05){
  bn = empty.graph(names(data))
  for(i in 1:length(names(data))){
    for(j in 1:length(names(data))){
      if(i != j){
        data.fisher = getTableData(data[c(i,j)])
        res.fisher = fisher.test(data.fisher)
        if(res.fisher$p.value < p.value){
          bn = set.arc(bn, names(data)[i], names(data)[j])
        }
      }
    }
  }
  return(bn)
}

learnBN <- function(data, data.size.vec, try.size, correct.graph.string){
  res.correct = empty.graph(names(data))
  modelstring(res.correct) = correct.graph.string
  res.correct = skeleton(res.correct)
  index = 0
  tp.avg.vec = c()
  fp.avg.vec = c()
  fn.avg.vec = c()
  tp.sd.vec = c()
  fp.sd.vec = c()
  fn.sd.vec = c()
  precision.avg.vec = c()
  precision.sd.vec = c()
  
  for(data.size in data.size.vec){
    tp.num.vec = c()
    fp.num.vec = c()
    fn.num.vec = c()
    precision.vec = c()
    
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), data.size)
      res.learn = getStructureWithFisher(sample.data)
      res.learn = skeleton(res.learn)
      
      diff.table = compare(target=res.correct, current=res.learn, arcs=FALSE)
      tp.num.vec = append(tp.num.vec, diff.table$tp)
      fp.num.vec = append(fp.num.vec, diff.table$fp)
      fn.num.vec = append(fn.num.vec, diff.table$fn)
      precision.vec = append(precision.vec, diff.table$tp/(diff.table$tp + diff.table$fp))
    }
    tp.avg.vec = append(tp.avg.vec, sum(tp.num.vec) / try.size)
    fp.avg.vec = append(fp.avg.vec, sum(fp.num.vec) / try.size)
    fn.avg.vec = append(fn.avg.vec, sum(fn.num.vec) / try.size)
    tp.sd.vec = append(tp.sd.vec, sd(tp.num.vec))
    fp.sd.vec = append(fp.sd.vec, sd(fp.num.vec))
    fn.sd.vec = append(fn.sd.vec, sd(fn.num.vec))
    precision.avg.vec = append(precision.avg.vec, sum(precision.vec) / try.size)
    precision.sd.vec = append(precision.sd.vec, sd(precision.vec))
    index = index + 1
  }
  print(paste("tp.avg.vec : ", tp.avg.vec))
  print(paste("fp.avg.vec:", fp.avg.vec))
  print(paste("fn.avg.vec:", fn.avg.vec))
  print(paste("tp.sd.vec", tp.sd.vec))
  print(paste("fp.sd.vec", fp.sd.vec))
  print(paste("fn.sd.vec", fn.sd.vec))
  print(paste("precision.arg.vec", precision.avg.vec))
  print(paste("precision.sd.vec", precision.sd.vec))
  return(list(tp.avg.vec, fp.avg.vec, fn.avg.vec, tp.sd.vec, fp.sd.vec, fn.sd.vec, precision.avg.vec, precision.sd.vec))
}

main <- function(){
  data(insurance)
  data.size.vec = seq(10,30, by=5)
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
  
  
  result = learnBN(insurance, data.size.vec, try.size, correct.graph.string)
  plotPrecisionResult(data.size.vec, "insurance", 100*result[[7]], 100*result[[8]])
  plotFPResult(data.size.vec, "insurance",result[[2]], result[[5]])
}


main()
