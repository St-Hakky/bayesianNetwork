library(bnlearn)
library(ggplot2)

plotFPResult <- function(data_size_vec, data_name, fp_avg_vec, fp_sd_vec=NULL, flag_error_bar = FALSE){
  df = data_frame(id = data_size_vec, fp_avg = fp_avg_vec)
  if(flag_error_bar){
    data_sd = data_frame(fp_sd = fp_sd_vec)
    df = cbind(df, data_sd)
  }
  g = ggplot(
    df,
    aes(
      x = id,
      y = fp_avg
    )
  )
  if(flag_error_bar){
    g = g + geom_errorbar(aes(x=id, y=fp_avg, ymin=fp_avg-fp_sd, ymax=fp_avg + fp_sd), width=1)
    g = g + geom_point()
  }
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data_size_vec))) + ggtitle(paste("fp-avg-Data :",data_name))
  plot(g)
}

plotPrecisionResult <- function(data_size_vec, data_name, precision, precision_sd_vec=NULL, flag_error_bar=FALSE){
  df = data_frame(id = data_size_vec, precision = precision)
  if(flag_error_bar){
    data_sd = data_frame(precision_sd = precision_sd_vec)
    df = cbind(df, data_sd)
  }
  g = ggplot(
    df,
    aes(
      x = id,
      y = precision
    )
  )
  if(flag_error_bar){
    g = g + geom_errorbar(aes(x=id, y=precision, ymin=precision-precision_sd, ymax=precision + precision_sd), width=1)
    g = g + geom_point()
  }
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data_size_vec))) + ggtitle(paste("Precision-Data :",data_name))
  plot(g)
}

plotRecallResult <- function(data_size_vec, data_name, recall, recall_sd_vec=NULL, flag_error_bar=FALSE){
  df = data_frame(id = data_size_vec, recall = recall)
  if(flag_error_bar){
    data_sd = data_frame(recall_sd = recall_sd_vec)
    df = cbind(df, data_sd)
  }
  g = ggplot(
    df,
    aes(
      x = id,
      y = recall
    )
  )
  if(flag_error_bar){
    g = g + geom_errorbar(aes(x=id, y=recall, ymin=recall - recall_sd, ymax=recall + recall_sd), width=1)
    g = g + geom_point()
  }
  g = g + geom_line()
  g = g + coord_cartesian(xlim = c(0, max(data_size_vec))) + ggtitle(paste("Recall-Data :",data_name))
  plot(g)
}

plotPrecisionRecallResult <- function(precision_vec, recall_vec, data_name){
  df = data_frame(x = recall_vec, y = precision_vec)
  g = ggplot(
    df,
    aes (
      x = x,
      y = y
    )
  )
  g = g + geom_line() + geom_point()
  g = g + coord_cartesian(xlim = c(0,1), ylim = c(0,1))
  g = g + xlab("recall") + ylab("precision") + ggtitle(paste("Precision-Recall Data :", data_name))
  plot(g)
}


getTableData <- function(data){
  data1 = data[,1]
  data2 = data[,2]
  data_vec = c()
  for(i in 1:length(levels(data1))){
    for(j in 1:length(levels(data2))){
      len = length(data[data[,1] == levels(data1)[i] & data[,2] == levels(data2)[j], ][,1])
      data_vec = append(data_vec, len)
    }
  }
  data_table = matrix(data_vec, nrow=length(levels(data1)), byrow=T, dimnames=list(levels(data1), levels(data2)))
  return (data_table)
}

getStructureWithFisher <- function(data, p_value=0_05){
  bn = empty_graph(names(data))
  for(i in 1:length(names(data))){
    for(j in 1:length(names(data))){
      if(i != j){
        data_fisher = getTableData(data[c(i,j)])
        res_fisher = fisher_test(data_fisher)
        if(res_fisher$p_value < p_value){
          bn = set_arc(bn, names(data)[i], names(data)[j])
        }
      }
    }
  }
  return(bn)
}

learnBN <- function(data, data_size_vec, try_size, correct_graph_string){
  res_correct = empty_graph(names(data))
  modelstring(res_correct) = correct_graph_string
  res_correct = skeleton(res_correct)
  index = 0
  tp_avg_vec = c()
  fp_avg_vec = c()
  fn_avg_vec = c()
  tp_sd_vec = c()
  fp_sd_vec = c()
  fn_sd_vec = c()
  precision_avg_vec = c()
  precision_sd_vec = c()

  for(data_size in data_size_vec){
    tp_num_vec = c()
    fp_num_vec = c()
    fn_num_vec = c()
    precision_vec = c()

    for(i in 1:try_size){
      sample_data = getSampleData(data, length(data[,1]), data_size)
      res_learn = getStructureWithFisher(sample_data)
      res_learn = skeleton(res_learn)

      diff_table = compare(target=res_correct, current=res_learn, arcs=FALSE)
      tp_num_vec = append(tp_num_vec, diff_table$tp)
      fp_num_vec = append(fp_num_vec, diff_table$fp)
      fn_num_vec = append(fn_num_vec, diff_table$fn)
      precision_vec = append(precision_vec, diff_table$tp/(diff_table$tp + diff_table$fp))
    }
    tp_avg_vec = append(tp_avg_vec, sum(tp_num_vec) / try_size)
    fp_avg_vec = append(fp_avg_vec, sum(fp_num_vec) / try_size)
    fn_avg_vec = append(fn_avg_vec, sum(fn_num_vec) / try_size)
    tp_sd_vec = append(tp_sd_vec, sd(tp_num_vec))
    fp_sd_vec = append(fp_sd_vec, sd(fp_num_vec))
    fn_sd_vec = append(fn_sd_vec, sd(fn_num_vec))
    precision_avg_vec = append(precision_avg_vec, sum(precision_vec) / try_size)
    precision_sd_vec = append(precision_sd_vec, sd(precision_vec))
    index = index + 1
  }
  print(paste("tp_avg_vec : ", tp_avg_vec))
  print(paste("fp_avg_vec:", fp_avg_vec))
  print(paste("fn_avg_vec:", fn_avg_vec))
  print(paste("tp_sd_vec", tp_sd_vec))
  print(paste("fp_sd_vec", fp_sd_vec))
  print(paste("fn_sd_vec", fn_sd_vec))
  print(paste("precision_arg_vec", precision_avg_vec))
  print(paste("precision_sd_vec", precision_sd_vec))
  return(list(tp_avg_vec, fp_avg_vec, fn_avg_vec, tp_sd_vec, fp_sd_vec, fn_sd_vec, precision_avg_vec, precision_sd_vec))
}

main <- function(){
  data(insurance)
  data_size_vec = seq(10,30, by=5)
  try_size = 50

  correct_graph_string = paste("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
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


  result = learnBN(insurance, data_size_vec, try_size, correct_graph_string)
  plotPrecisionResult(data_size_vec, "insurance", 100*result[[7]], 100*result[[8]])
  plotFPResult(data_size_vec, "insurance",result[[2]], result[[5]])
}


main()
