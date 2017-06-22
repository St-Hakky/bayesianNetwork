library(bnlearn)
library(ggplot2)

getTableData <- function(data) {
  data1 <- data[, 1]
  data2 <- data[, 2]
  
  data_vec <- c()
  for (i in 1:length(levels(data1))) {
    for (j in 1:length(levels(data2))) {
      len <- length(data[data[, 1] == levels(data1)[i] &
                          data[, 2] == levels(data2)[j],][, 1])
      data_vec <- append(data_vec, len)
    }
  }
  data_table <- matrix(
    data_vec,
    nrow = length(levels(data1)),
    byrow = T,
    dimnames = list(levels(data1), levels(data2))
  )
  return (data_table)
}

getSampleData <- function(data, data_length, random_length) {
  random_index <- sort(floor(runif(random_length) * data_length))
  random_val <- data[random_index, ]
  return(random_val)
}

getPvalueWithArcs <- function(data, arcs) {
  arcs_p_value_vec <- c()
  for (i in 1:length(arcs[, 1])) {
    data_fisher <- getTableData(data[arcs[i, ]])
    res_fisher <- fisher.test(data_fisher)
    arcs_p_value_vec <- append(arcs_p_value_vec, res_fisher$p_value)
  }
  return(arcs_p_value_vec)
}

learn <-  function(data,
                   correct_graph_string,
                   data_size_vec,
                   try_size) {
  res_correct <- empty.graph(names(data))
  modelstring(res_correct) <- correct_graph_string
  graphviz.plot(res_correct)
  data_roc <- list()
  index <- 1
  for (data_size in data_size_vec) {
    p_value_vec <- c()
    ans_vec <- c()
    for (i in 1:try_size) {
      sample_data <- getSampleData(data, length(data[, 1]), data_size)
      res_learn = hc(sample_data)
      
      diff_table = compare(target = res_correct,
                           current = res_learn,
                           arcs = TRUE)
      if (length(diff_table$tp[, 1]) != 0) {
        p_value_vec = append(p_value_vec,
                             getPvalueWithArcs(sample_data, diff_table$tp))
        ans_vec = append(ans_vec, rep(1, length(diff_table$tp[, 1])))
      }
      
      if (length(diff_table$fp[, 1]) != 0) {
        p_value_vec = append(p_value_vec,
                             getPvalueWithArcs(sample_data, diff_table$fp))
        ans_vec = append(ans_vec, rep(0, length(diff_table$fp[, 1])))
      }
    }
    data_roc = c(data_roc, list(list()))
    data_roc[[index]] = c(data_roc[[index]], list(sort(p_value_vec)))
    print("bbb")
    data_roc[[index]] = c(data_roc[[index]], list(ans_vec[order(p_value_vec)]))
    print("aaa")
    index = index + 1
  }
  return(data_roc)
}

LatherThanThreshold <- function(data_roc_list, threshold) {
  tp_rate_vec = c()
  fp_rate_vec = c()
  for (i in 1:length(data_roc_list)) {
    total = length(data_roc_list[[i]][[2]][data_roc_list[[i]][[1]] >= threshold])
    tp_rate = length(data_roc_list[[i]][[2]][data_roc_list[[i]][[1]] >= threshold &
                                               data_roc_list[[i]][[2]] == 1]) / total
    fp_rate = length(data_roc_list[[i]][[2]][data_roc_list[[i]][[1]] >= threshold &
                                               data_roc_list[[i]][[2]] == 0]) / total
    tp_rate_vec = append(tp_rate_vec, tp_rate)
    fp_rate_vec = append(fp_rate_vec, fp_rate)
  }
  return (list(tp_rate_vec, fp_rate_vec))
}

LessThanThreshold <- function(data_roc_list, threshold) {
  tp_rate_vec = c()
  fp_rate_vec = c()
  for (i in 1:length(data_roc_list)) {
    total = length(data_roc_list[[i]][[2]][data_roc_list[[i]][[1]] <= threshold])
    tp_rate = length(data_roc_list[[i]][[2]][data_roc_list[[i]][[1]] <= threshold &
                                               data_roc_list[[i]][[2]] == 1]) / total
    fp_rate = length(data_roc_list[[i]][[2]][data_roc_list[[i]][[1]] <= threshold &
                                               data_roc_list[[i]][[2]] == 0]) / total
    tp_rate_vec = append(tp_rate_vec, tp_rate)
    fp_rate_vec = append(fp_rate_vec, fp_rate)
  }
  return (list(tp_rate_vec, fp_rate_vec))
}

plotROC <- function(data_size_vec,
                    tp_rate_vec,
                    fp_rate_vec,
                    title) {
  tp_fp_rate = data_frame(Fp_Rate = fp_rate_vec,
                          Tp_Rate = tp_rate_vec)
  
  print(tp_fp_rate)
  
  g = ggplot(tp_fp_rate, aes(x = Fp_Rate, y = Tp_Rate, label = data_size_vec))
  g + geom_point(colour = "gray50", size = 10)
  g = g + scale_x_continuous(limits = c(min(fp_rate_vec), max(fp_rate_vec)))
  g = g + scale_y_continuous(limits = c(min(tp_rate_vec), max(tp_rate_vec)))
  g = g + geom_point()
  g = g + geom_text(size = 3,
                    hjust = 0,
                    vjust = 0)
  g = g + labs(title = title)
  plot(g)
}

main <-
  function(data,
           data_size_vec,
           try_size,
           correct_graph_string,
           threshold,
           data_name) {
    data_roc_list = learn(data, correct_graph_string, data_size_vec, try_size)
    print(data_roc_list)
    lather_than_threshold = LatherThanThreshold(data_roc_list, threshold)
    less_than_threshold = LessThanThreshold(data_roc_list, threshold)
    plotROC(
      data_size_vec,
      lather_than_threshold[[1]],
      lather_than_threshold[[2]],
      paste(
        "Data is",
        data_name ,
        "Less than threshold_ threshold =",
        threshold
      )
    )
    plotROC(
      data_size_vec,
      less_than_threshold[[1]],
      less_than_threshold[[2]],
      paste(
        "Data is",
        data_name ,
        "Lather than threshold_ threshold =",
        threshold
      )
    )
  }

data(asia)
data_size_vec = seq(10, 50, by = 5)
try_size = 50
threshold = 0.05
correct_graph_string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
main(asia,
     data_size_vec,
     try_size,
     correct_graph_string,
     threshold,
     "asia")

data(insurance)
data_size_vec = seq(10, 50, by = 5)
try_size = 50
threshold = 0.05
correct_graph_string = paste(
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
     try_size,
     correct_graph_string,
     threshold,
     "insurance")
