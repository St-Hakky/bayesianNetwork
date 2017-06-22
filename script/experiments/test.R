library(bnlearn)
library(ggplot2)

getSampleData <- function(data, data_length, random_length) {
  random_index <- sort(floor(runif(random_length) * data_length))
  random_val <- data[random_index, ]
  return(random_val)
}

compareBN <- function(data, data_size_vec, res_correct, try_size) {
  tp_num_vec <- c()
  fp_num_vec <- c()
  fn_num_vec <- c()
  for (data_size in data_size_vec) {
    tp_sum <- 0
    fp_sum <- 0
    fn_sum <- 0
    for (i in 1:try_size) {
      sample_data <- getSampleData(data, length(data[, 1]), data_size)
      res_learn <- hc(sample_data)
      diff_table <- compare(target = res_correct, current = res_learn, arcs = FALSE)
      tp_sum <- tp_sum + diff_table$tp
      fp_sum <- fp_sum + diff_table$fp
      fn_sum <- fn_sum + diff_table$fn
    }
    tp_num_vec <- append(tp_num_vec, tp_sum/try_size)
    fp_num_vec <- append(fp_num_vec, fp_sum/try_size)
    fn_num_vec <- append(fn_num_vec, fn_sum/try_size)
  }
  return(list(tp_num_vec, fp_num_vec, fn_num_vec))
}

correctBnlearn <- function(data, correct_graph_string) {
  res_correct <- empty_graph(names(data))
  modelstring(res_correct) = correct_graph_string

  graphviz_plot(res_correct)
  return(res_correct)
}

plotRateAboutTPFP <- function(data_size_vec, tp_vec, fp_vec, try_size, data_name) {
  rate_tp_vec <- c()
  rate_fp_vec <- c()
  for (i in 1:length(tp_vec)) {
    rate_tp_vec <- append(rate_tp_vec, 100 * (tp_vec[i]/(tp_vec[i] + fp_vec[i])))
    rate_fp_vec <- append(rate_fp_vec, 100 * (fp_vec[i]/(tp_vec[i] + fp_vec[i])))
  }
  rate_data <- data_frame(x = data_size_vec, y = rate_tp_vec)
  g <- ggplot(rate_data, aes(x = data_size_vec, y = rate_tp_vec))

  g <- g + geom_line(color = "red", linetype = 1, size = 1)

  g <- g + xlab("Data size") + ylab("TP rate") + ggtitle(paste(data_name, "TP rate"))
  g <- g + coord_cartesian(xlim = c(0, max(data_size_vec)))
  g <- g + theme_bw()
  # ggsave(paste(data_name, '_wmf'), g)
  plot(g)
}

plotArcNum <- function(data_size_vec, tp_vec, fp_vec, fn_vec, try_size) {
  plot(data_size_vec, tp_vec, xlim = c(0, max(data_size_vec)), ylim = c(0, max(c(tp_vec,
    fp_vec, fn_vec))), col = "blue", type = "b", xlab = "", ylab = "")
  par(new = T)
  plot(data_size_vec, fp_vec, xlim = c(0, max(data_size_vec)), ylim = c(0, max(c(tp_vec,
    fp_vec, fn_vec))), col = "red", type = "b", xlab = "", ylab = "")
  par(new = T)
  plot(data_size_vec, fn_vec, xlim = c(0, max(data_size_vec)), ylim = c(0, max(c(tp_vec,
    fp_vec, fn_vec))), col = "black", type = "b", xlab = "vector num", ylab = "data size")
  par(new = T)
  title(paste("try size : ", try_size, "tp:blue, fp:red, fn:black"))
  par(new = F)

}

main <- function() {
  data(alarm)
  data_size_vec <- seq(10, 100, by = 10)
  try_size <- 50
  correct_graph_string <- paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
    "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA]", "[HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK]",
    "[MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB]", "[SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS]",
    "[VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV]", "[CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]",
    sep = "")
  res_correct <- correctBnlearn(alarm, correct_graph_string)
  result <- compareBN(alarm, data_size_vec, res_correct, try_size)
  plotArcNum(data_size_vec, result[[1]], result[[2]], result[[3]], try_size)
  plotRateAboutTPFP(data_size_vec, result[[1]], result[[2]], try_size, "alarm")

  data(insurance)
  data_size_vec <- seq(10, 100, by = 10)
  try_size <- 50
  correct_graph_string <- paste("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
    "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon][VehicleYear|SocioEcon:RiskAversion]",
    "[MakeModel|SocioEcon:RiskAversion][SeniorTrain|Age:RiskAversion]", "[HomeBase|SocioEcon:RiskAversion][AntiTheft|SocioEcon:RiskAversion]",
    "[RuggedAuto|VehicleYear:MakeModel][Antilock|VehicleYear:MakeModel]", "[DrivingSkill|Age:SeniorTrain][CarValue|VehicleYear:MakeModel:Mileage]",
    "[Airbag|VehicleYear:MakeModel][DrivQuality|RiskAversion:DrivingSkill]",
    "[Theft|CarValue:HomeBase:AntiTheft][Cushioning|RuggedAuto:Airbag]", "[DrivHist|RiskAversion:DrivingSkill][Accident|DrivQuality:Mileage:Antilock]",
    "[ThisCarDam|RuggedAuto:Accident][OtherCarCost|RuggedAuto:Accident]", "[MedCost|Age:Accident:Cushioning][ILiCost|Accident]",
    "[ThisCarCost|ThisCarDam:Theft:CarValue][PropCost|ThisCarCost:OtherCarCost]",
    sep = "")
  res_correct <- correctBnlearn(insurance, correct_graph_string)
  result <- compareBN(insurance, data_size_vec, res_correct, try_size)
  plotArcNum(data_size_vec, result[[1]], result[[2]], result[[3]], try_size)
  plotRateAboutTPFP(data_size_vec, result[[1]], result[[2]], try_size, "insurance")

  data(asia)
  data_size_vec <- seq(10, 100, by = 10)
  try_size <- 50
  correct_graph_string <- "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
  res_correct <- correctBnlearn(asia, correct_graph_string)
  result <- compareBN(asia, data_size_vec, res_correct, try_size)
  plotArcNum(data_size_vec, result[[1]], result[[2]], result[[3]], try_size)
  plotRateAboutTPFP(data_size_vec, result[[1]], result[[2]], try_size, "asia")
}


main()
