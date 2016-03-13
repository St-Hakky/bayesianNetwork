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
  #  rownames(random.val) = c(1:random.length)
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
  data.boxplot = list()
  index = 1
  for(data.size in data.size.vec){
    tp.p.value.vec = c()
    fp.p.value.vec = c()
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), data.size)
      res.learn = hc(sample.data)
      
      diff.table = compare(target = res.correct, current=res.learn, arcs=TRUE)
      if(length(diff.table$tp[,1]) != 0){
        tp.p.value.vec = append(tp.p.value.vec, getPvalueWithArcs(sample.data, diff.table$tp))
      }
      
      if(length(diff.table$fp[,1]) != 0){
        fp.p.value.vec = append(fp.p.value.vec, getPvalueWithArcs(sample.data, diff.table$fp))
      }
    }
    data.boxplot = c(data.boxplot, list(list()))
    data.boxplot[[index]] = c(data.boxplot[[index]], list(tp.p.value.vec))
    data.boxplot[[index]] = c(data.boxplot[[index]], list(fp.p.value.vec))
    index = index + 1
  }
  return(data.boxplot)
}

main <- function(data, data.size.vec, data.name , try.size, correct.graph.string){
  data.boxplot = learn(data, correct.graph.string, data.size.vec, try.size)
  for(i in 1:length(data.boxplot)){
    print(data.boxplot[[i]])
    #tp = cbind(rep("tp", length(data.boxplot[[i]][[1]])), data.boxplot[[i]][[1]])
    #fp = cbind(rep("fp", length(data.boxplot[[i]][[2]])), data.boxplot[[i]][[2]])
    #data = rbind(tp,fp)
    #df = data.frame(variable = data[,1], value=data[,2])
    #print(df)
    #g = ggplot(
    #  df,
    #  aes (
    #    x = variable,
    #    y = value,
    #    group = variable,
    #    fill=variable
    #  )
    #)
    #g = g + geom_boxplot()
    #g = g + ggtitle(paste("data size : ", data.size.vec[i]))
    #g = g + coord_cartesian(ylim=c(0, 1))
    #g = g + xlab("Arc type")
    #g = g + ylab("p value")
    #plot(g)
    win.metafile(paste(data.name , "- boxplot -", "data size -", data.size.vec[i], ".wmf"))
    #png(paste(data.name , "- boxplot -", "data size -", data.size.vec[i], ".png"))
    boxplot(data.boxplot[[i]],
            names=c("TP", "FP"),
            main=paste("data size : ", data.size.vec[i]),
            col = c("lightcyan", "red"),
            xlab = "arc type",
            ylab = "p value",
            ylim=c(0, 0.35))
    abline(h = 0.05)
    dev.off()
  }
}

data(asia)
data.size.vec = seq(10,50,by=5)
try.size = 50
correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
#main(asia, data.size.vec, "asia" , try.size, correct.graph.string)


data(insurance)
data.size.vec = seq(10,50,by=5)
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

main(insurance, data.size.vec, "insurance" , try.size, correct.graph.string)




