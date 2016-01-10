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

compareAndReturnTPFPFN <- function(res.correct, sample.size.vec, try.size, data){
  index = 0
  for(sample.size in sample.size.vec){
    tp.sum = 0
    fp.sum = 0
    fn.sum = 0
    print(paste("sample.size : ", sample.size))
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), sample.size)
      res.learn = hc(sample.data)
      diff.table = compare(target=res.learn, current=res.correct, arcs=FALSE)
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

outputResult <- function(result, sample.size.vec, data.name, try.size){
  tp.parcent.vec = result[[1]]
  fp.parcent.vec = result[[2]]
  fn.parcent.vec = result[[3]]
  
  print(paste("tp.parcent.vec : ", tp.parcent.vec))
  print(paste("fp.parcent.vec : ", fp.parcent.vec))
  print(paste("fn.parcent.vec : ", fn.parcent.vec))
  
  # plotResult(data.size.vec, parcent, x-axis tile, y-axis tile, title)
  plotResult(sample.size.vec, tp.parcent.vec, "data size", "tp", paste("Data Name : ", data.name, ", Try Number : ", try.size))
  plotResult(sample.size.vec, fp.parcent.vec, "data size", "fp", paste("Data Name : ", data.name, ", Try Number : ", try.size))
  plotResult(sample.size.vec, fn.parcent.vec, "data size", "fn", paste("Data Name : ", data.name, ", Try Number : ", try.size))
  
}

checkData <- function(data, sample.size.vec, try.size, correct.graph.string, data.name){
  print(data.name)
  # correct graph data
  res.correct = empty.graph(names(data))
  modelstring(res.correct) = correct.graph.string
  graphviz.plot(res.correct)
  
  # result = list(tp.parcent.vec, fp.parcent.vec, fn.parcent.vec)
  result = compareAndReturnTPFPFN(res.correct, sample.size.vec, try.size, data)
  
  outputResult(result, sample.size.vec, data.name, try.size)
}

main <- function(try.size = 50){
  
  # check asia
  data(asia)
  asia.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
  asia.correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
  checkData(asia, asia.size.vec, try.size, asia.correct.graph.string, "asia")

  # check alarm
  data(alarm)
  alarm.correct.graph.string = paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
                                   "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA]",
                                   "[HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK]",
                                   "[MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB]",
                                   "[SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS]",
                                   "[VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV]",
                                   "[CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]", sep = "")
  alarm.size.vec = c(10,50,100,500,1000,2000,3000)
  checkData(alarm, alarm.size.vec, try.size, alarm.correct.graph.string, "alarm")
  
  # clgaussian.test
  data(clgaussian.test)
  clgaussian.test.correct.graph.string = "[A][B][C][H][D|A:H][F|B:C][E|B:D][G|A:D:E:F]"
  clgaussian.test.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
  checkData(clgaussian.test, clgaussian.test.size.vec, try.size, clgaussian.test.correct.graph.string, "clgaussian.test")
  
  # gaussian.test
  data(gaussian.test)
  gaussian.test.correct.graph.string = "[A][B][E][G][C|A:B][D|B][F|A:D:E:G]"
  gaussian.test.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
  checkData(gaussian.test, gaussian.test.size.vec, try.size, gaussian.test.correct.graph.string, "gaussian.test")
  
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
  insurance.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
  checkData(insurance, insurance.size.vec,try.size, insurance.correct.graph.string, "insurance")
  
  #learning.test
  data(learning.test)
  learning.test.correct.graph.string = "[A][C][F][B|A][D|A:C][E|B:F]"
  learning.test.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
  checkData(learning.test, learning.test.size.vec, try.size, learning.test.correct.graph.string, "learning.test")
  
  #lizards
  data(lizards)
  res = empty.graph(names(lizards))
  lizards.correct.graph.string = "[Species][Diameter|Species][Height|Species]"
  lizards.size.vec = c(10,50,100,200,300,400,409)
  checkData(lizards, lizards.size.vec, try.size, lizards.correct.graph.string, "lizards")
  
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
  hailfinder.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
  checkData(hailfinder, hailfinder.size.vec, try.size, hailfinder.correct.graph.string, "hailfinder")
  
}

main(try.size = 50)

