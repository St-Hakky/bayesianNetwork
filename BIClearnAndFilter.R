library(bnlearn)

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
  graphviz.plot(res.correct)
  
  index = 0
  for(data.size in data.size.vec){
    tp.sum = 0
    fp.sum = 0
    fn.sum = 0
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), data.size)
      res.learn = hc(sample.data)
      if(length(res.learn$arcs) == 0){
        next
      }
      if(include.p.value){
        #res.learn = filterFisher(sample.data, res.learn$arcs)
        res.learn = filterXSquare(sample.data, res.learn$arcs)
      }
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
  return (list(tp.num.vec, fp.num.vec, fn.num.vec))
}


main <- function(){
  if(FALSE){
    data(alarm)
    data.size.vec = seq(10,50, by=5)
    try.size = 50
    correct.graph.string = paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
                                 "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA]",
                                 "[HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK]",
                                 "[MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB]",
                                 "[SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS]",
                                 "[VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV]",
                                 "[CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]", sep = "")
    
    result.bic = learnBN(alarm, correct.graph.string, data.size.vec, try.size, include.p.value=FALSE)
    result.filter.fisher = learnBN(alarm, correct.graph.string, data.size.vec, try.size, include.p.value=TRUE)
    print(result.bic)
    print(result.filter.fisher)
  }
  
  
  if(TRUE){
    print("insurance")
    data(insurance)
    data.size.vec = seq(10,50, by=5)
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
    print(result.bic)
    print(result.filter.fisher)
    
    print("asia")
    data(asia)
    data.size.vec = seq(10,50,by=5)
    try.size = 50
    correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
    result.bic = learnBN(asia, correct.graph.string, data.size.vec, try.size, include.p.value=FALSE)
    result.filter.fisher = learnBN(asia, correct.graph.string, data.size.vec, try.size, include.p.value=TRUE)
    print(result.bic)
    print(result.filter.fisher)
  }
}
  

main()