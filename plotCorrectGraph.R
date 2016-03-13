library(bnlearn)


correctBnlearn <- function(data, correct.graph.string){
  res.correct = empty.graph(names(data))
  modelstring(res.correct) = correct.graph.string
  graphviz.plot(res.correct)
  return(res.correct)
}

main <- function(){
  data(insurance)
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
  correct = correctBnlearn(insurance, correct.graph.string)
  print(length(correct$arcs[,1]))
  
  data(asia)
  correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
  correct = correctBnlearn(asia, correct.graph.string)
  print(correct)
}

main()
