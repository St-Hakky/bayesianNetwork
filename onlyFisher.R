library(bnlearn)
library(ggplot2)

plotFPResult <- function(result, data.size.vec, data.name){
  fp.arc.num = c()
  for(i in 1:length(result[[2]])){
    fp.arc.num = append(fp.arc.num, result[[2]][[i]])
  }
  data = data.frame(id = data.size.vec, fp.arc.num = fp.arc.num)
  g = ggplot(
    data,
    aes(
      x = id,
      y = fp.arc.num
    )
  )
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec)), ylim=c(0, max(c(fp.arc.num)))) + ggtitle(paste("fp-arc-num-Data :",data.name))
  plot(g)
  ggsave(paste("fp-arc-num-Data -",data.name ,".wmf"), g)
}

plotTPRateResult <- function(result, data.size.vec, data.name){
  tp.rate = c()
  for(i in 1:length(result[[1]])){
    tp.rate = append(tp.rate, 100*(result[[1]][[i]]/(result[[1]][[i]] + result[[2]][[i]])))
  }
  data = data.frame(id = data.size.vec, tp.rate = tp.rate)
  g = ggplot(
    data,
    aes(
      x = id,
      y = tp.rate
    )
  )
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data.size.vec))) + ggtitle(paste("tp-rate-Data :",data.name))
  plot(g)
  ggsave(paste("precision-Data-",data.name ,".wmf"), g)
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
  tp.num.vec = c()
  fp.num.vec = c()
  fn.num.vec = c()
  
  for(data.size in data.size.vec){
    tp.sum = 0
    fp.sum = 0
    fn.sum = 0
    
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), data.size)
      res.learn = getStructureWithFisher(sample.data)
      res.learn = skeleton(res.learn)
      
      diff.table = compare(target=res.correct, current=res.learn, arcs=FALSE)
      tp.sum = tp.sum + diff.table$tp
      fp.sum = fp.sum + diff.table$fp
      fn.sum = fn.sum + diff.table$fn
    }
    tp.num.vec = append(tp.num.vec, tp.sum / try.size)
    fp.num.vec = append(fp.num.vec, fp.sum / try.size)
    fn.num.vec = append(fn.num.vec, fn.sum / try.size)
    index = index + 1    
  }
  print(paste("tp.num.vec :", tp.num.vec))
  print(paste("fp.num.vec :", fp.num.vec))
  print(paste("fn.num.vec :", fn.num.vec))
  return(list(tp.num.vec, fp.num.vec, fn.num.vec))
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
  plotTPRateResult(result, data.size.vec, "insurance")
  plotFPResult(result, data.size.vec, "insurance")
}


main()
