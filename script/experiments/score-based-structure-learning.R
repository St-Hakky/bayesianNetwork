library(bnlearn)
library(reshape2)
library(ggplot2)

getTableData <- function(data) {
  data1 <- data[, 1]
  data2 <- data[, 2]

  data_vec <- c()
  for (i in 1:length(levels(data1))) {
    for (j in 1:length(levels(data2))) {
      len <- length(data[data[, 1] == levels(data1)[i] & data[, 2] == levels(data2)[j],
        ][, 1])
      data_vec <- append(data_vec, len)
    }
  }
  data_table <- matrix(data_vec, nrow = length(levels(data1)), byrow = T, dimnames = list(levels(data1),
    levels(data2)))
  return(data_table)
}

getSampleData <- function(data, data_length, random_length) {
  random_index <- sort(floor(runif(random_length) * data_length))
  random_val <- data[random_index, ]
  # rownames(random_val) = c(1:random_length)
  return(random_val)
}

learnBN <- function(data, correct_graph_string, data_size_vec, try_size, flag_skeleton = FALSE) {
  res_correct <- empty.graph(names(data))
  modelstring(res_correct) = correct_graph_string

  index <- 0
  tp_avg_vec <- c()
  fp_avg_vec <- c()
  fn_avg_vec <- c()
  tp_sd_vec <- c()
  fp_sd_vec <- c()
  fn_sd_vec <- c()
  precision_arg_vec <- c()
  precision_sd_vec <- c()
  recall_arg_vec <- c()
  recall_sd_vec <- c()

  for (data_size in data_size_vec) {
    tp_num_vec <- c()
    fp_num_vec <- c()
    fn_num_vec <- c()
    precision_vec <- c()
    recall_vec <- c()
    for (i in 1:try_size) {
      sample_data <- getSampleData(data, length(data[, 1]), data_size)
      res_learn <- hc(sample_data, score = "bic")
      if (length(res_learn$arcs) == 0) {
        next
      }
      if (flag_skeleton) {
        res_correct <- skeleton(res_correct)
        res_learn <- skeleton(res_learn)
      }
      diff_table <- compare(target = res_correct, current = res_learn, arcs = FALSE)
      tp_num_vec <- append(tp_num_vec, diff_table$tp)
      fp_num_vec <- append(fp_num_vec, diff_table$fp)
      fn_num_vec <- append(fn_num_vec, diff_table$fn)
      precision_vec <- append(precision_vec, diff_table$tp/(diff_table$tp +
        diff_table$fp))
      recall_vec <- append(recall_vec, diff_table$tp/(diff_table$tp + diff_table$fn))
    }
    tp_avg_vec <- append(tp_avg_vec, sum(tp_num_vec)/try_size)
    fp_avg_vec <- append(fp_avg_vec, sum(fp_num_vec)/try_size)
    fn_avg_vec <- append(fn_avg_vec, sum(fn_num_vec)/try_size)
    tp_sd_vec <- append(tp_sd_vec, sd(tp_num_vec))
    fp_sd_vec <- append(fp_sd_vec, sd(fp_num_vec))
    fn_sd_vec <- append(fn_sd_vec, sd(fn_num_vec))
    precision_arg_vec <- append(precision_arg_vec, sum(precision_vec)/try_size)
    precision_sd_vec <- append(precision_sd_vec, sd(precision_vec))
    recall_arg_vec <- append(recall_arg_vec, sum(recall_vec)/try_size)
    recall_sd_vec <- append(recall_sd_vec, sd(recall_vec))
    index <- index + 1
  }
  print(paste("tp_avg_vec : ", tp_avg_vec))
  print(paste("fp_avg_vec:", fp_avg_vec))
  print(paste("fn_avg_vec:", fn_avg_vec))
  print(paste("tp_sd_vec", tp_sd_vec))
  print(paste("fp_sd_vec", fp_sd_vec))
  print(paste("fn_sd_vec", fn_sd_vec))
  print(paste("precision_arg_vec", precision_arg_vec))
  print(paste("precision_sd_vec", precision_sd_vec))
  print(paste("recall_arg_vec", recall_arg_vec))
  print(paste("recall_sd_vec", recall_sd_vec))
  return(list(tp_avg_vec, fp_avg_vec, fn_avg_vec, tp_sd_vec, fp_sd_vec, fn_sd_vec,
    precision_arg_vec, precision_sd_vec, recall_arg_vec, recall_sd_vec))
}

plotFPResult <- function(data_size_vec,
                         data_name,
                         fp_avg_vec,
                         fp_sd_vec = NULL,
                         flag_error_bar = FALSE) {
  df <- data.frame(id = data_size_vec, fp_avg = fp_avg_vec)
  if (flag_error_bar) {
    data_sd <- data.frame(fp_sd = fp_sd_vec)
    df <- cbind(df, data_sd)
  }
  g <- ggplot(df, aes(x = id, y = fp_avg))
  if (flag_error_bar) {
    g <- g + geom_errorbar(aes(x = id,
                               y = fp_avg,
                               ymin = fp_avg - fp_sd,
                               ymax = fp_avg + fp_sd),
                           width = 1)
    g <- g + geom_point()
  }
  g <- g + geom_line()
  g <- g + coord_cartesian(xlim = c(0, max(data_size_vec))) + ggtitle(paste("fp-avg-Data :", data_name))
  plot(g)
}

plotPrecisionResult <- function(data_size_vec,
                                data_name,
                                precision,
                                precision_sd_vec = NULL,
                                flag_error_bar = FALSE) {
  df <- data.frame(id = data_size_vec, precision = precision)
  if (flag_error_bar) {
    data_sd <- data.frame(precision_sd = precision_sd_vec)
    df <- cbind(df, data_sd)
  }
  g <- ggplot(df, aes(x = id, y = precision))
  if (flag_error_bar) {
    g <- g + geom_errorbar(aes(x = id,
                               y = precision,
                               ymin = precision - precision_sd,
                               ymax = precision + precision_sd),
                            width = 1)
    g <- g + geom_point()
  }
  g <- g + geom_line()
  g <- g + coord_cartesian(xlim = c(0, max(data_size_vec))) + ggtitle(paste("Precision-Data :", data_name))
  plot(g)
}

plotRecallResult <- function(data_size_vec,
                             data_name, recall,
                             recall_sd_vec = NULL,
                             flag_error_bar = FALSE) {
  df <- data.frame(id = data_size_vec, recall = recall)
  if (flag_error_bar) {
    data_sd <- data.frame(recall_sd = recall_sd_vec)
    df <- cbind(df, data_sd)
  }
  g <- ggplot(df, aes(x = id, y = recall))
  if (flag_error_bar) {
    g <- g + geom_errorbar(aes(x = id,
                               y = recall,
                               ymin = recall - recall_sd,
                               ymax = recall + recall_sd),
                            width = 1)
    g <- g + geom_point()
  }
  g <- g + geom_line()
  g <- g + coord_cartesian(xlim = c(0, max(data_size_vec))) + ggtitle(paste("Recall-Data :", data_name))
  plot(g)
}

plotPrecisionRecallResult <- function(precision_vec, recall_vec, data_name) {
  df <- data.frame(x = recall_vec, y = precision_vec)
  g <- ggplot(df, aes(x = x, y = y))
  g <- g + geom_line() + geom_point()
  g <- g + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
  g <- g + xlab("recall") + ylab("precision") + ggtitle(paste("Precision-Recall Data :", data_name))
  plot(g)
}

main <- function() {
  if (TRUE) {
    print("insurance")
    data(insurance)
    data_size_vec <- seq(100, 2000, by = 100)
    try_size <- 50
    correct_graph_string <- paste("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
      "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon][VehicleYear|SocioEcon:RiskAversion]",
      "[MakeModel|SocioEcon:RiskAversion][SeniorTrain|Age:RiskAversion]", "[HomeBase|SocioEcon:RiskAversion][AntiTheft|SocioEcon:RiskAversion]",
      "[RuggedAuto|VehicleYear:MakeModel][Antilock|VehicleYear:MakeModel]",
      "[DrivingSkill|Age:SeniorTrain][CarValue|VehicleYear:MakeModel:Mileage]",
      "[Airbag|VehicleYear:MakeModel][DrivQuality|RiskAversion:DrivingSkill]",
      "[Theft|CarValue:HomeBase:AntiTheft][Cushioning|RuggedAuto:Airbag]",
      "[DrivHist|RiskAversion:DrivingSkill][Accident|DrivQuality:Mileage:Antilock]",
      "[ThisCarDam|RuggedAuto:Accident][OtherCarCost|RuggedAuto:Accident]",
      "[MedCost|Age:Accident:Cushioning][ILiCost|Accident]", "[ThisCarCost|ThisCarDam:Theft:CarValue][PropCost|ThisCarCost:OtherCarCost]",
      sep = "")

    result <- learnBN(insurance,
                      correct_graph_string,
                      data_size_vec,
                      try_size,
                      flag_skeleton = FALSE)

    plotPrecisionResult(data_size_vec, "insurance", 100 * result[[7]], 100 * result[[8]])
    plotRecallResult(data_size_vec, "insurance", 100 * result[[9]], 100 * result[[10]])
    plotFPResult(data_size_vec, "insurance", result[[2]], result[[5]])
    plotPrecisionRecallResult(result[[7]], result[[9]], "insurance")

    print("asia")
    data(asia)
    data_size_vec <- seq(10, 50, by = 5)
    try_size <- 50
    correct_graph_string <- "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
    # result_bic = learnBN(asia, correct_graph_string, data_size_vec, try_size)
    # result_filter_fisher = learnBN(asia, correct_graph_string, data_size_vec,
    # try_size) plotPrecisionResult(result_bic, result_filter_fisher, data_size_vec,
    # 'asia') plotFPResult(result_bic, result_filter_fisher, data_size_vec, 'asia')
  }
}


main()
