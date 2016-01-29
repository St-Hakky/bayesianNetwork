library(bnlearn)


getSampleData <- function(data, data.length, random.length){
  random.index = sort(floor(runif(random.length) * data.length))
  random.val = data[random.index,]
#  rownames(random.val) = c(1:random.length)
  return(random.val)
}

plotResult <- function(x, y, xlab, ylab, title){
  plot(x,y,xlab=xlab, ylab=ylab, type="b")
  par(new=T)
  title(title)
  par(new=F)
}

compareAndReturnTPFPFN <- function(res.correct, sample.size.vec, try.size, data, skeleton=FALSE){
  index = 0
  for(sample.size in sample.size.vec){
    tp.sum = 0
    fp.sum = 0
    fn.sum = 0
    print(paste("sample.size : ", sample.size))
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), sample.size)
      res.learn = hc(sample.data)
      if(skeleton){
        res.learn = skeleton(res.learn)
      }
      diff.table = compare(target=res.correct, current=res.learn, arcs=FALSE)
      tp.sum = tp.sum + diff.table$tp
      fp.sum = fp.sum + diff.table$fp
      fn.sum = fn.sum + diff.table$fn
    }
    tp.parcent = tp.sum / try.size
    fp.parcent = fp.sum / try.size
    fn.parcent = fn.sum / try.size
    print(paste("the tp's parcent is ", tp.parcent))
    print(paste("the fp's parcent is ", fp.parcent))
    print(paste("the fn's parcent is ", fn.parcent))
    if(index == 0){
      tp.parcent.vec = c(tp.parcent)
      fp.parcent.vec = c(fp.parcent)
      fn.parcent.vec = c(fn.parcent)
    }else{
      tp.parcent.vec = append(tp.parcent.vec, tp.parcent)
      fp.parcent.vec = append(fp.parcent.vec, fp.parcent)
      fn.parcent.vec = append(fn.parcent.vec, fn.parcent)
    }
    index = index + 1
  }
  return (list(tp.parcent.vec, fp.parcent.vec, fn.parcent.vec))
}

getCsvData <- function(data.size.num.vec, tp.parcent.vec, fp.parcent.vec, fn.parcent.vec){
  data.vec = c(tp.parcent.vec, fp.parcent.vec, fn.parcent.vec)
  print(data.vec)
  data.csv.matrix = matrix(data.vec, nrow=length(data.vec)/3, ncol=3)
  print(data.csv.matrix)
  colnames(data.csv.matrix) = c("tp.num.vec", "fp.num.vec", "fn.num.vec")
  rownames(data.csv.matrix) = data.size.num.vec
  return(data.csv.matrix)
}

outputResult <- function(result, sample.size.vec, data.name, try.size){
  tp.parcent.vec = result[[1]]
  fp.parcent.vec = result[[2]]
  fn.parcent.vec = result[[3]]
  
  max.val = ceiling(max(c(max(tp.parcent.vec), max(fp.parcent.vec), max(fn.parcent.vec))))
  
  print(paste("tp.parcent.vec : ", tp.parcent.vec))
  print(paste("fp.parcent.vec : ", fp.parcent.vec))
  print(paste("fn.parcent.vec : ", fn.parcent.vec))
  
  # plotResult(data.size.vec, parcent, x-axis tile, y-axis tile, title)
  print(paste("Data Name : ", data.name, ", Try Number : ", try.size, ".png"))
  png(paste("Data Name ", data.name, ", Try Number ", try.size, ".png"))
  plotResult(sample.size.vec, tp.parcent.vec, "data size", "tp", paste("Data Name : ", data.name, ", Try Number : ", try.size))
  dev.off()
  png(paste("Data Name ", data.name, ", Try Number ", try.size, ".png"))
  plotResult(sample.size.vec, fp.parcent.vec, "data size", "fp", paste("Data Name : ", data.name, ", Try Number : ", try.size))
  dev.off()
  png(paste("Data Name ", data.name, ", Try Number ", try.size, ".png"))
  plotResult(sample.size.vec, fn.parcent.vec, "data size", "fn", paste("Data Name : ", data.name, ", Try Number : ", try.size))
  dev.off()
  
  # 
  png(paste(data.name, " (TP-Red FP-blue FN-black) ", "Try Number ", try.size, ".png"))
  plot(sample.size.vec, tp.parcent.vec, xlab="data size", ylab="", ylim=c(0, max.val), type="b", col="red")
  par(new=T)
  plot(sample.size.vec, fp.parcent.vec, xlab="", ylab="", ylim=c(0, max.val), type="b", col="blue")
  par(new=T)
  plot(sample.size.vec, fn.parcent.vec, xlab="", ylab="the number of arcs", ylim=c(0, max.val), type="b", col="black")
  par(new=T)
  title(paste(data.name, " (TP:Red, FP:blue, FN:black), ", "Try Number : ", try.size))
  par(new=F)
  dev.off()
  print(sample.size.vec)
  print(tp.parcent.vec)
  data.csv.matrix = getCsvData(sample.size.vec, tp.parcent.vec, fp.parcent.vec, fn.parcent.vec)
  print(data.csv.matrix)
  write.csv(data.csv.matrix, paste(data.name, ".csv"), quote=FALSE)
  
}

checkData <- function(data, sample.size.vec, try.size, correct.graph.string, data.name, skeleton=FALSE){
  print(data.name)
  # correct graph data
  res.correct = empty.graph(names(data))
  modelstring(res.correct) = correct.graph.string
  if(skeleton){
    res.correct = skeleton(res.correct)
  }
  graphviz.plot(res.correct)
  
  # result = list(tp.parcent.vec, fp.parcent.vec, fn.parcent.vec)
  result = compareAndReturnTPFPFN(res.correct, sample.size.vec, try.size, data, skeleton)
  return(result)
}

outputFPResultwithOverride <- function(result.directed,col.directed = "blue",result.undirected,col.undirected="red",sample.size.vec, title, try.size ){
  result=append(result.directed, result.undirected)
  png(paste(title, "outputFPResultwithOverride" ,".png"))
  plot(sample.size.vec, result.directed, col=col.directed,ylim=c(0, ceiling(max(result))) ,xlab=paste("data size, ", col.directed, "=directed, ",col.undirected ,"=undirected"), ylab="fp", type="b" )
  par(new=T)
  plot(sample.size.vec, result.undirected,ylim=c(0, ceiling(max(result))), col=col.undirected,xlab="",ylab="", type="b")
  title(title)
  par(new=F)
  dev.off()
}

main <- function(try.size = 50, skeleton=FALSE){
  
  # check asia
  data(asia)
  #  asia.size.vec = seq(10,length(asia[,1]), by=10)
  asia.size.vec = seq(10,500,by=5)
  asia.correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
  asia.result.undirected = checkData(asia, asia.size.vec, try.size, asia.correct.graph.string, "asia", skeleton = TRUE)
  asia.result.directed   = checkData(asia, asia.size.vec, try.size, asia.correct.graph.string, "asia", skeleton = FALSE)
  outputResult(asia.result.undirected, asia.size.vec, "asia.undirected", try.size)
  outputResult(asia.result.directed, asia.size.vec, "asia.directed", try.size)
  outputFPResultwithOverride(asia.result.directed[[2]],"blue",asia.result.undirected[[2]],"red",asia.size.vec, "asia fp", try.size )
  
  # check alarm
  data(alarm)
  alarm.correct.graph.string = paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
                                     "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA]",
                                     "[HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK]",
                                     "[MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB]",
                                     "[SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS]",
                                     "[VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV]",
                                     "[CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]", sep = "")
  #alarm.size.vec = c(10,50,100,500,1000,2000,3000)
  alarm.size.vec = seq(10,500,by=5)
  alarm.result.undirected = checkData(alarm, alarm.size.vec, try.size, alarm.correct.graph.string, "alarm", skeleton = TRUE)
  alarm.result.directed = checkData(alarm, alarm.size.vec, try.size, alarm.correct.graph.string, "alarm", skeleton = FALSE)
  outputResult(alarm.result.undirected, alarm.size.vec, "alarm.undirected", try.size)
  outputResult(alarm.result.directed, alarm.size.vec, "alarm.directed", try.size)
  outputFPResultwithOverride(alarm.result.directed[[2]],"blue",alarm.result.undirected[[2]],"red",alarm.size.vec, "alarm fp", try.size )
  
  if(FALSE){
    # clgaussian.test
    data(clgaussian.test)
    clgaussian.test.correct.graph.string = "[A][B][C][H][D|A:H][F|B:C][E|B:D][G|A:D:E:F]"
    #clgaussian.test.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
    clgaussian.test.size.vec = seq(10,500,by=5)
    clgaussian.test.result.undirected = checkData(clgaussian.test, clgaussian.test.size.vec, try.size, clgaussian.test.correct.graph.string, "clgaussian.test", skeleton = TRUE)
    clgaussian.test.result.directed = checkData(clgaussian.test, clgaussian.test.size.vec, try.size, clgaussian.test.correct.graph.string, "clgaussian.test", skeleton = FALSE)
    outputResult(clgaussian.test.result.undirected, clgaussian.test.size.vec, "clgaussian.test.undirected", try.size)
    outputResult(clgaussian.test.result.directed, clgaussian.test.size.vec, "clgaussian.test.directed", try.size)
    outputFPResultwithOverride(clgaussian.test.result.directed[[2]],"blue",clgaussian.test.result.undirected[[2]],"red",clgaussian.test.size.vec, "clgaussian.test fp", try.size )
    
    # gaussian.test
    data(gaussian.test)
    gaussian.test.correct.graph.string = "[A][B][E][G][C|A:B][D|B][F|A:D:E:G]"
    #gaussian.test.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
    gaussian.test.size.vec = seq(10,500,by=5)
    gaussian.test.result.undirected = checkData(gaussian.test, gaussian.test.size.vec, try.size, gaussian.test.correct.graph.string, "gaussian.test", skeleton=TRUE)
    gaussian.test.result.directed = checkData(gaussian.test, gaussian.test.size.vec, try.size, gaussian.test.correct.graph.string, "gaussian.test", skeleton=FALSE)
    outputResult(gaussian.test.result.undirected, gaussian.test.size.vec, "gaussian.test.undirected",try.size)
    outputResult(gaussian.test.result.directed, gaussian.test.size.vec, "gaussian.test.directed",try.size)
    outputFPResultwithOverride(gaussian.test.result.directed[[2]],"blue" , gaussian.test.result.undirected[[2]], "red", gaussian.test.size.vec, "gaussian.test fp", try.size)
    
    # insurance
    data(insurance)
    insurance.correct.graph.string = paste("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
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
    #insurance.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
    insurance.size.vec = seq(10,500,by=5)
    insurance.result.undirected = checkData(insurance, insurance.size.vec, try.size, insurance.correct.graph.string, "insurance", skeleton=TRUE)
    insurance.result.directed = checkData(insurance, insurance.size.vec, try.size, insurance.correct.graph.string, "insurance", skeleton=FALSE)
    outputResult(insurance.result.undirected, insurance.size.vec, "insurance.undirected",try.size)
    outputResult(insurance.result.directed, insurance.size.vec, "insurance.directed",try.size)
    outputFPResultwithOverride(insurance.result.directed[[2]],"blue" , insurance.result.undirected[[2]], "red", insurance.size.vec, "insurance fp", try.size)
    checkData(insurance, insurance.size.vec, try.size, insurance.correct.graph.string, "insurance", skeleton)
    
    #learning.test
    data(learning.test)
    learning.test.correct.graph.string = "[A][C][F][B|A][D|A:C][E|B:F]"
    #learning.test.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
    learning.test.size.vec = seq(10,500,by=5)
    learning.test.result.undirected = checkData(learning.test, learning.test.size.vec, try.size, learning.test.correct.graph.string, "learning.test", skeleton=TRUE)
    learning.test.result.directed = checkData(learning.test, learning.test.size.vec, try.size, learning.test.correct.graph.string, "learning.test", skeleton=FALSE)
    outputResult(learning.test.result.undirected, learning.test.size.vec, "learning.test.undirected",try.size)
    outputResult(learning.test.result.directed, learning.test.size.vec, "learning.test.directed",try.size)
    outputFPResultwithOverride(learning.test.result.directed[[2]],"blue" , learning.test.result.undirected[[2]], "red", learning.test.size.vec, "learning.test fp", try.size)
    
    #lizards
    data(lizards)
    res = empty.graph(names(lizards))
    lizards.correct.graph.string = "[Species][Diameter|Species][Height|Species]"
    #lizards.size.vec = c(10,50,100,200,300,400,409)
    lizards.size.vec = seq(10,500,by=5)
    lizards.result.undirected = checkData(lizards, lizards.size.vec, try.size, lizards.correct.graph.string, "lizards", skeleton=TRUE)
    lizards.result.directed = checkData(lizards, lizards.size.vec, try.size, lizards.correct.graph.string, "lizards", skeleton=FALSE)
    outputResult(lizards.result.undirected, lizards.size.vec, "lizards.undirected",try.size)
    outputResult(lizards.result.directed, lizards.size.vec, "lizards.directed",try.size)
    outputFPResultwithOverride(lizards.result.directed[[2]],"blue" , lizards.result.undirected[[2]], "red", lizards.size.vec, "lizards fp", try.size)
    
    
    #hailfinder
    data(hailfinder)
    hailfinder.correct.graph.string = paste("[N07muVerMo][SubjVertMo][QGVertMotion][SatContMoist][RaoContMoist]",
                                            "[VISCloudCov][IRCloudCover][AMInstabMt][WndHodograph][MorningBound][LoLevMoistAd][Date]",
                                            "[MorningCIN][LIfr12ZDENSd][AMDewptCalPl][LatestCIN][LLIW]",
                                            "[CombVerMo|N07muVerMo:SubjVertMo:QGVertMotion][CombMoisture|SatContMoist:RaoContMoist]",
                                            "[CombClouds|VISCloudCov:IRCloudCover][Scenario|Date][CurPropConv|LatestCIN:LLIW]",
                                            "[AreaMesoALS|CombVerMo][ScenRelAMCIN|Scenario][ScenRelAMIns|Scenario][ScenRel34|Scenario]",
                                            "[ScnRelPlFcst|Scenario][Dewpoints|Scenario][LowLLapse|Scenario][MeanRH|Scenario]",
                                            "[MidLLapse|Scenario][MvmtFeatures|Scenario][RHRatio|Scenario][SfcWndShfDis|Scenario]",
                                            "[SynForcng|Scenario][TempDis|Scenario][WindAloft|Scenario][WindFieldMt|Scenario]",
                                            "[WindFieldPln|Scenario][AreaMoDryAir|AreaMesoALS:CombMoisture]",
                                            "[AMCINInScen|ScenRelAMCIN:MorningCIN][AMInsWliScen|ScenRelAMIns:LIfr12ZDENSd:AMDewptCalPl]",
                                            "[CldShadeOth|AreaMesoALS:AreaMoDryAir:CombClouds][InsInMt|CldShadeOth:AMInstabMt]",
                                            "[OutflowFrMt|InsInMt:WndHodograph][CldShadeConv|InsInMt:WndHodograph][MountainFcst|InsInMt]",
                                            "[Boundaries|WndHodograph:OutflowFrMt:MorningBound][N34StarFcst|ScenRel34:PlainsFcst]",
                                            "[CompPlFcst|AreaMesoALS:CldShadeOth:Boundaries:CldShadeConv][CapChange|CompPlFcst]",
                                            "[InsChange|CompPlFcst:LoLevMoistAd][CapInScen|CapChange:AMCINInScen]",
                                            "[InsSclInScen|InsChange:AMInsWliScen][R5Fcst|MountainFcst:N34StarFcst]",
                                            "[PlainsFcst|CapInScen:InsSclInScen:CurPropConv:ScnRelPlFcst]",
                                            sep = "")
    #hailfinder.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
    hailfinder.size.vec= seq(10,500,by=5)
    hailfinder.result.undirected = checkData(hailfinder, hailfinder.size.vec, try.size, hailfinder.correct.graph.string, "hailfinder", skeleton=TRUE)
    hailfinder.result.directed = checkData(hailfinder, hailfinder.size.vec, try.size, hailfinder.correct.graph.string, "hailfinder", skeleton=FALSE)
    outputResult(hailfinder.result.undirected, hailfinder.size.vec, "hailfinder.undirected",try.size)
    outputResult(hailfinder.result.directed, hailfinder.size.vec, "hailfinder.directed",try.size)
    outputFPResultwithOverride(hailfinder.result.directed[[2]],"blue" , hailfinder.result.undirected[[2]], "red", hailfinder.size.vec, "hailfinder fp", try.size)
  }
}

main(try.size = 50,FALSE)

