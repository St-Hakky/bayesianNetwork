library(bnlearn)
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
  return(random.val)
}

getPvalueWithArcs <- function(data, arcs){
  arcs.p.value.vec = c()
  for(i in 1:length(arcs[,1])){
    data.fisher = getTableData(data[arcs[i,]])
    res.fisher = fisher.test(data.fisher)
    arcs.p.value.vec = append(arcs.p.value.vec, res.fisher$p.value)
  }
  return(arcs.p.value.vec)
}

learn <- function(data, correct.graph.string, data.size.vec, try.size){
  res.correct = empty.graph(names(data))
  modelstring(res.correct) = correct.graph.string
  graphviz.plot(res.correct)
  data.roc = list()
  index = 1
  for(data.size in data.size.vec){
    p.value.vec = c()
    ans.vec = c()
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), data.size)
      res.learn = hc(sample.data)
      
      diff.table = compare(target = res.correct, current=res.learn, arcs=TRUE)
      if(length(diff.table$tp[,1]) != 0){
        p.value.vec = append(p.value.vec, getPvalueWithArcs(sample.data, diff.table$tp))
        ans.vec = append(ans.vec, rep(1, length(diff.table$tp[,1])))
      }
      
      if(length(diff.table$fp[,1]) != 0){
        p.value.vec = append(p.value.vec, getPvalueWithArcs(sample.data, diff.table$fp))
        ans.vec = append(ans.vec, rep(0, length(diff.table$fp[,1])))        
      }
    }
    data.roc = c(data.roc, list(list()))
    data.roc[[index]] = c(data.roc[[index]], list(sort(p.value.vec)))
    data.roc[[index]] = c(data.roc[[index]], list(ans.vec[order(p.value.vec)]))
    index = index + 1
  }
  return(data.roc)
}

LatherThanThreshold <- function(data.roc.list, threshold){
  tp.rate.vec = c()
  fp.rate.vec = c()
  for(i in 1:length(data.roc.list)){
    total = length(data.roc.list[[i]][[2]][data.roc.list[[i]][[1]] >= threshold])
    tp.rate = length(data.roc.list[[i]][[2]][data.roc.list[[i]][[1]] >= threshold & data.roc.list[[i]][[2]] == 1]) / total 
    fp.rate = length(data.roc.list[[i]][[2]][data.roc.list[[i]][[1]] >= threshold & data.roc.list[[i]][[2]] == 0]) / total
    tp.rate.vec = append(tp.rate.vec, tp.rate)
    fp.rate.vec = append(fp.rate.vec, fp.rate)
  }
  return (list(tp.rate.vec, fp.rate.vec))
}

LessThanThreshold <- function(data.roc.list, threshold){
  tp.rate.vec = c()
  fp.rate.vec = c()
  for(i in 1:length(data.roc.list)){
    total = length(data.roc.list[[i]][[2]][data.roc.list[[i]][[1]] <= threshold])
    tp.rate = length(data.roc.list[[i]][[2]][data.roc.list[[i]][[1]] <= threshold & data.roc.list[[i]][[2]] == 1]) / total 
    fp.rate = length(data.roc.list[[i]][[2]][data.roc.list[[i]][[1]] <= threshold & data.roc.list[[i]][[2]] == 0]) / total
    tp.rate.vec = append(tp.rate.vec, tp.rate)
    fp.rate.vec = append(fp.rate.vec, fp.rate)
  }
  return (list(tp.rate.vec, fp.rate.vec))
}

plotROC <- function(data.size.vec, tp.rate.vec, fp.rate.vec, title){
  tp.fp.rate = data.frame(
    Fp.Rate = fp.rate.vec,
    Tp.Rate = tp.rate.vec
  )
  
  print(tp.fp.rate)
  
  g = ggplot(tp.fp.rate, aes(x = Fp.Rate, y = Tp.Rate, label=data.size.vec))
  g + geom_point(colour="gray50", size=10)
  g=g+scale_x_continuous(limits=c(min(fp.rate.vec),max(fp.rate.vec)))
  g=g+scale_y_continuous(limits=c(min(tp.rate.vec),max(tp.rate.vec)))
  g=g+geom_point()
  g=g+geom_text(size=3,hjust=0,vjust=0)
  g=g+labs(title=title)
  plot(g)
}

main <- function(data, data.size.vec, try.size, correct.graph.string, threshold, data.name){
  data.roc.list = learn(data, correct.graph.string, data.size.vec, try.size)
  print(data.roc.list)
  lather.than.threshold = LatherThanThreshold(data.roc.list, threshold)
  less.than.threshold = LessThanThreshold(data.roc.list, threshold)
  plotROC(data.size.vec, lather.than.threshold[[1]], lather.than.threshold[[2]], paste("Data is", data.name ,"Less than threshold. threshold =", threshold))
  plotROC(data.size.vec, less.than.threshold[[1]], less.than.threshold[[2]], paste("Data is", data.name ,"Lather than threshold. threshold =", threshold))
}

data(asia)
data.size.vec = seq(10,50,by=5)
try.size = 50
threshold = 0.05
correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
main(asia, data.size.vec, try.size,correct.graph.string, threshold, "asia")

data(insurance)
data.size.vec = seq(10,50,by=5)
try.size = 50
threshold = 0.05
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

main(insurance, data.size.vec, try.size,correct.graph.string, threshold, "insurance")




