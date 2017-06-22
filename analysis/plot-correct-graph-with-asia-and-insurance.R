library(bnlearn)

# plot correct graph with asia
data(asia)
asia_correct_graph_string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
asia_graph <- empty.graph(names(asia))
modelstring(asia_graph) <- asia_correct_graph_string
graphviz.plot(asia_graph)


# plot correct graph with insurance
data(insurance)
insurance_correct_graph_string <-
  paste(
    "[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
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
    sep = ""
  )
insurance_graph <- empty.graph(names(insurance))
modelstring(insurance_graph) <- insurance_correct_graph_string
graphviz.plot(insurance_graph)
