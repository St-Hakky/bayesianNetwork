library(bnlearn)
library(ggplot2)
source(file.path(getwd(), "script", "functions", "common.R"))

main <-  function(data,
                  data_size_vec,
                  data_name ,
                  try_size,
                  correct_graph_string) {
  data_boxplot <- learn(data, correct_graph_string, data_size_vec, try_size)
  print(data_boxplot)
  for (i in 1:length(data_boxplot)) {
    print(data_boxplot[[i]])
    #tp = cbind(rep("tp", length(data_boxplot[[i]][[1]])), data_boxplot[[i]][[1]])
    #fp = cbind(rep("fp", length(data_boxplot[[i]][[2]])), data_boxplot[[i]][[2]])
    #data = rbind(tp,fp)
    #df = data_frame(variable = data[,1], value=data[,2])
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
    #g = g + ggtitle(paste("data size : ", data_size_vec[i]))
    #g = g + coord_cartesian(ylim=c(0, 1))
    #g = g + xlab("Arc type")
    #g = g + ylab("p value")
    #plot(g)
    win.metafile(paste(
      data_name ,
      "- boxplot -",
      "data size -",
      data_size_vec[i],
      ".wmf"
    ))
    #png(paste(data_name , "- boxplot -", "data size -", data_size_vec[i], "_png"))
    boxplot(
      data_boxplot[[i]],
      names = c("TP", "FP"),
      main = paste("data size : ", data_size_vec[i]),
      col = c("lightcyan", "red"),
      xlab = "arc type",
      ylab = "p value",
      ylim = c(0, 0.35)
    )
    abline(h = 0.05)
    dev.off()
  }
}

data(asia)
data_size_vec <- seq(10, 50, by = 5)
try_size <- 50
correct_graph_string <- "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
#main(asia, data_size_vec, "asia" , try_size, correct_graph_string)


data(insurance)
data_size_vec <- seq(10, 50, by = 5)
try_size <- 50
correct_graph_string <- paste(
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

main(insurance,
     data_size_vec,
     "insurance" ,
     try_size,
     correct_graph_string)
