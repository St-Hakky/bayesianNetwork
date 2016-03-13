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

learnBN <- function(data, correct.graph.string, data.size.vec, try.size, include.p.value=FALSE){
  res.correct = empty.graph(names(data))
  modelstring(res.correct) = correct.graph.string
  res.correct = skeleton(res.correct)
  #graphviz.plot(res.correct)
  
  index = 0
  for(data.size in data.size.vec){
    tp.sum = 0
    fp.sum = 0
    fn.sum = 0
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
      res.learn = skeleton(res.learn)
      diff.table = compare(target=res.correct, current=res.learn, arcs=FALSE)
      tp.sum = tp.sum + diff.table$tp
      fp.sum = fp.sum + diff.table$fp
      fn.sum = fn.sum + diff.table$fn
    }
    tp.num = tp.sum / try.size
    fp.num = fp.sum / try.size
    fn.num = fn.sum / try.size
    if(index == 0){
      tp.num.vec = c(tp.num)
      fp.num.vec = c(fp.num)
      fn.num.vec = c(fn.num)
    }else{
      tp.num.vec = append(tp.num.vec, tp.num)
      fp.num.vec = append(fp.num.vec, fp.num)
      fn.num.vec = append(fn.num.vec, fn.num)
    }
    index = index + 1    
  }
  print(paste("tp.num.vec : ", tp.num.vec))
  print(paste("fp.num.vec:", fp.num.vec))
  print(paste("fn.num.vec:", fn.num.vec))
  return (list(tp.num.vec, fp.num.vec, fn.num.vec))
}

plotFPResult <- function(result.bic, result.filter.fisher, data.size.vec, data.name){
  fp.arc.num.bic = c()
  fp.arc.num.filter.fisher = c()
  for(i in 1:length(result.bic[[2]])){
    fp.arc.num.bic = append(fp.arc.num.bic, result.bic[[2]][[i]])
  }
  for(i in 1:length(result.filter.fisher[[2]])){
    fp.arc.num.filter.fisher = append(fp.arc.num.filter.fisher, result.filter.fisher[[2]][[i]])
  }
  data = data.frame(id = data.size.vec, fp.arc.num.bic = fp.arc.num.bic, fp.arc.num.filter.fisher= fp.arc.num.filter.fisher)
  df = melt(data, id.var = c("id"))
  g = ggplot(
    df,
    aes(
      x = id,
      y = value,
      group = variable,
      colour = variable
    )
  )
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec)), ylim=c(0, max(c(fp.arc.num.bic, fp.arc.num.filter.fisher)))) + ggtitle(paste("fp-arc-num-Data :",data.name))
  plot(g)
  ggsave(paste("fp-arc-num-Data -",data.name ,".wmf"), g)
  
}

plotTPRateResult <- function(result.bic, result.filter.fisher, data.size.vec, data.name){
  tp.rate.bic = c()
  tp.rate.filter.fisher = c()
  for(i in 1:length(result.bic[[1]])){
    tp.rate.bic = append(tp.rate.bic, 100*(result.bic[[1]][[i]]/(result.bic[[1]][[i]] + result.bic[[2]][[i]])))
  }
  for(i in 1:length(result.filter.fisher[[1]])){
    tp.rate.filter.fisher = append(tp.rate.filter.fisher, 100*(result.filter.fisher[[1]][[i]]/(result.filter.fisher[[1]][[i]] + result.filter.fisher[[2]][[i]])))
  }
  data = data.frame(id = data.size.vec, tp.rate.bic = tp.rate.bic, tp.rate.filter.fisher= tp.rate.filter.fisher)
  df = melt(data, id.var = c("id"))
  g = ggplot(
    df,
    aes(
      x = id,
      y = value,
      group = variable,
      colour = variable
    )
  )
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec))) + ggtitle(paste("tp-rate-Data :",data.name))
  plot(g)
  ggsave(paste("precision-Data-",data.name ,".wmf"), g)
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
    result.bic = learnBN(insurance, correct.graph.string, data.size.vec, try.size, include.p.value=FALSE)
    result.filter.fisher = learnBN(insurance, correct.graph.string, data.size.vec, try.size, include.p.value=TRUE)
    plotTPRateResult(result.bic, result.filter.fisher, data.size.vec, "insurance")
    plotFPResult(result.bic, result.filter.fisher, data.size.vec, "insurance")
    
    print("asia")
    data(asia)
    data.size.vec = seq(10,50,by=5)
    try.size = 50
    correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
    #result.bic = learnBN(asia, correct.graph.string, data.size.vec, try.size, include.p.value=FALSE)
    #result.filter.fisher = learnBN(asia, correct.graph.string, data.size.vec, try.size, include.p.value=TRUE)
    #plotTPRateResult(result.bic, result.filter.fisher, data.size.vec, "asia")
    #plotFPResult(result.bic, result.filter.fisher, data.size.vec, "asia")
  }
}
  

main()