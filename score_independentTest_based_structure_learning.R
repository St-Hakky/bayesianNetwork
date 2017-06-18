library(bnlearn)
library(reshape2)
library(ggplot2)

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

getSampleData <- function(data, data.length, random.length){
  random.index = sort(floor(runif(random.length) * data.length))
  random.val = data[random.index,]
  #  rownames(random.val) = c(1:random.length)
  return(random.val)
}

filterFisher <- function(data, arcs, p.value=0.05){
  delete.arc.index = c()
  bn = empty.graph(names(data))
  for(j in 1:length(arcs[,1])){
    data.fisher = getTableData(data[arcs[j,]])
    res.fisher = fisher.test(data.fisher)
    if(res.fisher$p.value < p.value){
      bn = set.arc(bn, arcs[[j,1]], arcs[[j,2]])
    }
  }
  return(bn)
}

filterXSquare <- function(data, arcs, p.value=0.05){
  delete.arc.index = c()
  bn = empty.graph(names(data))
  for(j in 1:length(arcs[,1])){
    data.x.square = getTableData(data[arcs[j,]])
    res.x.square = chisq.test(data.x.square)
    if(!is.nan(res.x.square$p.value)){
      if(res.x.square$p.value < p.value){
        bn = set.arc(bn, arcs[[j,1]], arcs[[j,2]])
      }
    }
  }
  return(bn)
}



learnBN <- function(data, correct.graph.string, data.size.vec, try.size, include.p.value=FALSE, flag.skeleton=FALSE){
  res.correct = empty.graph(names(data))
  modelstring(res.correct) = correct.graph.string
  
  index = 0
  tp.avg.vec = c()
  fp.avg.vec = c()
  fn.avg.vec = c()
  tp.sd.vec = c()
  fp.sd.vec = c()
  fn.sd.vec = c()
  precision.arg.vec = c()
  precision.sd.vec = c()
  recall.arg.vec = c()
  recall.sd.vec = c()

  for(data.size in data.size.vec){
    tp.num.vec = c()
    fp.num.vec = c()
    fn.num.vec = c()
    precision.vec = c()
    recall.vec = c()
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), data.size)
      res.learn = hc(sample.data, score="bic")
      if(length(res.learn$arcs) == 0){
        next
      }
      if(include.p.value){
        res.learn = filterFisher(sample.data, res.learn$arcs)
        #res.learn = filterXSquare(sample.data, res.learn$arcs)
      }
      if(flag.skeleton){
        res.correct = skeleton(res.correct)
        res.learn = skeleton(res.learn)
      }
      diff.table = compare(target=res.correct, current=res.learn, arcs=FALSE)
      tp.num.vec = append(tp.num.vec, diff.table$tp)
      fp.num.vec = append(fp.num.vec, diff.table$fp)
      fn.num.vec = append(fn.num.vec, diff.table$fn)
      precision.vec = append(precision.vec, diff.table$tp / (diff.table$tp + diff.table$fp))
      recall.vec = append(recall.vec, diff.table$tp / (diff.table$tp + diff.table$fn))
    }
    tp.avg.vec = append(tp.avg.vec, sum(tp.num.vec) / try.size)
    fp.avg.vec = append(fp.avg.vec, sum(fp.num.vec) / try.size)
    fn.avg.vec = append(fn.avg.vec, sum(fn.num.vec) / try.size)
    tp.sd.vec = append(tp.sd.vec, sd(tp.num.vec))
    fp.sd.vec = append(fp.sd.vec, sd(fp.num.vec))
    fn.sd.vec = append(fn.sd.vec, sd(fn.num.vec))
    precision.arg.vec = append(precision.arg.vec, sum(precision.vec) / try.size)
    precision.sd.vec = append(precision.sd.vec, sd(precision.vec))
    recall.arg.vec = append(recall.arg.vec, sum(recall.vec)/ try.size)
    recall.sd.vec = append(recall.sd.vec, sd(recall.sd.vec))
    index = index + 1    
  }
  print(paste("tp.avg.vec : ", tp.avg.vec))
  print(paste("fp.avg.vec:", fp.avg.vec))
  print(paste("fn.avg.vec:", fn.avg.vec))
  print(paste("tp.sd.vec", tp.sd.vec))
  print(paste("fp.sd.vec", fp.sd.vec))
  print(paste("fn.sd.vec", fn.sd.vec))
  print(paste("precision.arg.vec", precision.arg.vec))
  print(paste("precision.sd.vec", precision.sd.vec))
  print(paste("recall.arg.vec", recall.arg.vec))
  print(paste("recall.sd.vec", recall.sd.vec))
  return (list(tp.avg.vec, fp.avg.vec, fn.avg.vec, tp.sd.vec, fp.sd.vec, fn.sd.vec, precision.arg.vec, precision.sd.vec, recall.arg.vec, recall.sd.vec))
}

plotFPResult <- function(fp.arc.avg.bic, fp.arc.avg.filter.fisher, fp.sd.bic, fp.sd.filter.fisher, data.size.vec, data.name, flag.error.bar=FALSE){
  data = data.frame(id = data.size.vec, fp.arc.avg.bic = fp.arc.avg.bic, fp.arc.avg.filter.fisher= fp.arc.avg.filter.fisher)
  df = melt(data, id.var = c("id"))
  if(flag.error.bar){
    data.sd = data.frame(fp.sd = c(fp.sd.bic,fp.sd.filter.fisher))
    df = cbind(df, data.sd)
  }
  g = ggplot(
    df,
    aes(
      x = id,
      y = value,
      group = variable,
      colour = variable
    )
  )
  if(flag.error.bar){
    g = g + geom_errorbar(aes(x=id, y=value, ymin=value-fp.sd, ymax=value + fp.sd, group = variable, colour=variable), width=1)
    g = g + geom_point()
  }
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec))) + ggtitle(paste("fp-arc-num-Data :",data.name))
  plot(g)
}

plotPrecisionResult <- function(precision.bic, precision.filter.fisher, precision.sd.bic, precision.sd.filter.fisher, data.size.vec, data.name, flag.error.bar=FALSE){
  data = data.frame(id = data.size.vec, precision.bic = precision.bic, precision.filter.fisher= precision.filter.fisher)
  df = melt(data, id.var = c("id"))
  if(flag.error.bar){
    data.sd = data.frame(precision.sd = c(precision.sd.bic, precision.sd.filter.fisher))
    df = cbind(df, data.sd)
  }
  g = ggplot(
    df,
    aes(
      x = id,
      y = value,
      group = variable,
      colour = variable
    )
  )
  if(flag.error.bar){
    
    g = g + geom_errorbar(aes(x=id, y=value, ymin=value-precision.sd, ymax=value + precision.sd, group = variable, colour=variable), width=1)
    g = g + geom_point()
  }
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec))) + ggtitle(paste("Precision-Data :",data.name))
  plot(g)
}

plotRecallResult <- function(recall.bic, recall.filter.fisher, recall.sd.bic, recall.sd.filter.fisher, data.size.vec, data.name, flag.error.bar=FALSE){
  data = data.frame(id = data.size.vec, recall.bic = recall.bic, recall.filter.fisher= recall.filter.fisher)
  df = melt(data, id.var = c("id"))
  if(flag.error.bar){
    data.sd = data.frame(recall.sd = c(recall.sd.bic, recall.sd.filter.fisher))
    df = cbind(df, data.sd)
  }
  g = ggplot(
    df,
    aes(
      x = id,
      y = value,
      group = variable,
      colour = variable
    )
  )
  if(flag.error.bar){
    
    g = g + geom_errorbar(aes(x=id, y=value, ymin=value-recall.sd, ymax=value + recall.sd, group = variable, colour=variable), width=1)
    g = g + geom_point()
  }
  g = g + geom_line()
  g = g + xlab("sample size") + ylab("Recall")
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


main <- function(){
  if(TRUE){
    print("insurance")
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
    result.bic = learnBN(insurance, correct.graph.string, data.size.vec, try.size, include.p.value=FALSE, flag.skeleton=FALSE)
    result.filter.fisher = learnBN(insurance, correct.graph.string, data.size.vec, try.size, include.p.value=TRUE, flag.skeleton=FALSE)
    plotPrecisionResult(100*result.bic[[7]], 100*result.filter.fisher[[7]], 100*result.bic[[8]], 100*result.filter.fisher[[8]], data.size.vec, "insurance", flag.error.bar=TRUE)
    plotFPResult(result.bic[[2]], result.filter.fisher[[2]],result.bic[[5]], result.filter.fisher[[5]], data.size.vec, "insurance", flag.error.bar=TRUE)
    plotRecallResult(100*result.bic[[9]], 100*result.filter.fisher[[9]], 100*result.bic[[10]], 100*result.filter.fisher[[10]], data.size.vec, "insurance", flag.error.bar=TRUE)
    plotPrecisionRecallResult(result.bic[[7]], result.bic[[9]], "insurance")
    plotPrecisionRecallResult(result.filter.fisher[[7]], result.filter.fisher[[9]], "insurance")
    print("asia")
    data(asia)
    data.size.vec = seq(10,50,by=5)
    try.size = 50
    correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
    #result.bic = learnBN(asia, correct.graph.string, data.size.vec, try.size, include.p.value=FALSE,flag.skeleton=TRUE)
    #result.filter.fisher = learnBN(asia, correct.graph.string, data.size.vec, try.size, include.p.value=TRUE,flag.skeleton=TRUE)
    #plotPrecisionResult(result.bic, result.filter.fisher, data.size.vec, "asia")
    #plotFPResult(result.bic, result.filter.fisher, data.size.vec, "asia")
  }
}


main()