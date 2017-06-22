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

filterFisher <- function(data, arcs, p_value = 0.05) {
  delete_arc_index <- c()
  bn <- empty.graph(names(data))
  for (j in 1:length(arcs[, 1])) {
    data_fisher <- getTableData(data[arcs[j, ]])
    res_fisher <- fisher.test(data_fisher)
    if (res_fisher$p.value < p_value) {
      bn <- set.arc(bn, arcs[[j, 1]], arcs[[j, 2]])
    }
  }
  return(bn)
}

filterXSquare <- function(data, arcs, p_value = 0.05) {
  delete_arc_index <- c()
  bn <- empty.graph(names(data))
  for (j in 1:length(arcs[, 1])) {
    data_x_square = getTableData(data[arcs[j, ]])
    res_x_square = chisq_test(data_x_square)
    if (!is_nan(res_x_square$p.value)) {
      if (res_x_square$p.value < p_value) {
        bn <- set.arc(bn, arcs[[j, 1]], arcs[[j, 2]])
      }
    }
  }
  return(bn)
}



learnBN <- function(data, correct_graph_string, data_size_vec, try_size, include_p_value = FALSE,
  flag_skeleton = FALSE) {
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
      if (include_p_value) {
        res_learn <- filterFisher(sample_data, res_learn$arcs)
        # res_learn <- filterXSquare(sample_data, res_learn$arcs)
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
    recall_sd_vec <- append(recall_sd_vec, sd(recall_sd_vec))
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

plotFPResult <- function(fp_arc_avg_bic, fp_arc_avg_filter_fisher, fp_sd_bic, fp_sd_filter_fisher,
  data_size_vec, data_name, flag_error_bar = FALSE) {
  data <- data.frame(id = data_size_vec, fp_arc_avg_bic = fp_arc_avg_bic, fp_arc_avg_filter_fisher = fp_arc_avg_filter_fisher)
  df <- melt(data, id_var = c("id"))
  if (flag_error_bar) {
    data_sd <- data.frame(fp_sd = c(fp_sd_bic, fp_sd_filter_fisher))
    df <- cbind(df, data_sd)
  }
  g <- ggplot(df, aes(x = id, y = value, group = variable, colour = variable))
  if (flag_error_bar) {
    g <- g + geom_errorbar(aes(x = id, y = value, ymin = value - fp_sd, ymax = value +
      fp_sd, group = variable, colour = variable), width = 1)
    g <- g + geom_point()
  }
  g <- g + geom_line()
  g <- g + coord_cartesian(xlim = c(0, max(data_size_vec))) + ggtitle(paste("fp-arc-num-Data :",
    data_name))
  plot(g)
}

plotPrecisionResult <- function(precision_bic, precision_filter_fisher, precision_sd_bic,
  precision_sd_filter_fisher, data_size_vec, data_name, flag_error_bar = FALSE) {
  data <- data.frame(id = data_size_vec, precision_bic = precision_bic, precision_filter_fisher = precision_filter_fisher)
  df <- melt(data, id_var = c("id"))
  if (flag_error_bar) {
    data_sd <- data.frame(precision_sd = c(precision_sd_bic, precision_sd_filter_fisher))
    df <- cbind(df, data_sd)
  }
  g <- ggplot(df, aes(x = id, y = value, group = variable, colour = variable))
  if (flag_error_bar) {

    g <- g + geom_errorbar(aes(x = id, y = value, ymin = value - precision_sd,
      ymax = value + precision_sd, group = variable, colour = variable), width = 1)
    g <- g + geom_point()
  }
  g <- g + geom_line()
  g <- g + coord_cartesian(xlim = c(0, max(data_size_vec))) + ggtitle(paste("Precision-Data :",
    data_name))
  plot(g)
}

plotRecallResult <- function(recall_bic, recall_filter_fisher, recall_sd_bic, recall_sd_filter_fisher,
  data_size_vec, data_name, flag_error_bar = FALSE) {
  data <- data.frame(id = data_size_vec, recall_bic = recall_bic, recall_filter_fisher = recall_filter_fisher)
  df <- melt(data, id_var = c("id"))
  if (flag_error_bar) {
    data_sd <- data.frame(recall_sd = c(recall_sd_bic, recall_sd_filter_fisher))
    df <- cbind(df, data_sd)
  }
  g <- ggplot(df, aes(x = id, y = value, group = variable, colour = variable))
  if (flag_error_bar) {

    g <- g + geom_errorbar(aes(x = id, y = value, ymin = value - recall_sd, ymax = value +
      recall_sd, group = variable, colour = variable), width = 1)
    g <- g + geom_point()
  }
  g <- g + geom_line()
  g <- g + xlab("sample size") + ylab("Recall")
  g <- g + coord_cartesian(xlim = c(0, max(data_size_vec))) + ggtitle(paste("Recall-Data :",
    data_name))
  plot(g)
}

plotPrecisionRecallResult <- function(precision_vec, recall_vec, data_name) {
  df <- data.frame(x = recall_vec, y = precision_vec)
  g <- ggplot(df, aes(x = x, y = y))
  g <- g + geom_line() + geom_point()
  g <- g + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
  g <- g + xlab("recall") + ylab("precision") + ggtitle(paste("Precision-Recall Data :",
    data_name))
  plot(g)
}


main <- function() {
  if (TRUE) {
    print("insurance")
    data(insurance)
    data_size_vec <- seq(10, 30, by = 5)
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
    result_bic <- learnBN(insurance, correct_graph_string, data_size_vec, try_size,
      include_p_value = FALSE, flag_skeleton = FALSE)
    result_filter_fisher <- learnBN(insurance, correct_graph_string, data_size_vec,
      try_size, include_p_value = TRUE, flag_skeleton = FALSE)
    plotPrecisionResult(100 * result_bic[[7]], 100 * result_filter_fisher[[7]],
      100 * result_bic[[8]], 100 * result_filter_fisher[[8]], data_size_vec,
      "insurance", flag_error_bar = TRUE)
    plotFPResult(result_bic[[2]], result_filter_fisher[[2]], result_bic[[5]],
      result_filter_fisher[[5]], data_size_vec, "insurance", flag_error_bar = TRUE)
    plotRecallResult(100 * result_bic[[9]], 100 * result_filter_fisher[[9]],
      100 * result_bic[[10]], 100 * result_filter_fisher[[10]], data_size_vec,
      "insurance", flag_error_bar = TRUE)
    plotPrecisionRecallResult(result_bic[[7]], result_bic[[9]], "insurance")
    plotPrecisionRecallResult(result_filter_fisher[[7]], result_filter_fisher[[9]],
      "insurance")
    print("asia")
    data(asia)
    data_size_vec <- seq(10, 50, by = 5)
    try_size <- 50
    correct_graph_string <- "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
    # result_bic = learnBN(asia, correct_graph_string, data_size_vec, try_size,
    # include_p_value=FALSE,flag_skeleton=TRUE) result_filter_fisher = learnBN(asia,
    # correct_graph_string, data_size_vec, try_size,
    # include_p_value=TRUE,flag_skeleton=TRUE) plotPrecisionResult(result_bic,
    # result_filter_fisher, data_size_vec, 'asia') plotFPResult(result_bic,
    # result_filter_fisher, data_size_vec, 'asia')
  }
}


main()
