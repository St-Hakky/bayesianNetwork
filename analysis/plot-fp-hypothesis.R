library(bnlearn)

RESULT_DIR_PATH = file.path(getwd(), "report", "analysis", "plot-fp-hypothesis")


getSampleData <- function(data, data_length, random_length) {
  random_index <- sort(floor(runif(random_length) * data_length))
  random_val <- data[random_index,]
  return(random_val)
}

plotResult <- function(x, y, xlab, ylab, title) {
  plot(
    x,
    y,
    xlab = xlab,
    ylab = ylab,
    type = "b",
    main = title
  )
}

compareAndReturnTPFPFN <- function(res_correct,
                                   sample_size_vec,
                                   try_size,
                                   data,
                                   skeleton = FALSE) {
  tp_average_vec <- c()
  fp_average_vec <- c()
  fn_average_vec <- c()
  for (sample_size in sample_size_vec) {
    tp_sum <- 0
    fp_sum <- 0
    fn_sum <- 0
    for (i in 1:try_size) {
      sample_data <- getSampleData(data, length(data[, 1]), sample_size)
      
      if (skeleton) {
        directed_result <- hc(sample_data)
        learning_result <- skeleton(directed_result)
      } else{
        learning_result <- hc(sample_data)
      }
      
      diff_table <- compare(target = res_correct,
                            current = learning_result,
                            arcs = FALSE)
      tp_sum <- tp_sum + diff_table$tp
      fp_sum <- fp_sum + diff_table$fp
      fn_sum <- fn_sum + diff_table$fn
    }
    tp_average_vec <- append(tp_average_vec, tp_sum / try_size)
    fp_average_vec <- append(fp_average_vec, fp_sum / try_size)
    fn_average_vec <- append(fn_average_vec, fn_sum / try_size)
  }
  return (list(tp_average_vec, fp_average_vec, fn_average_vec))
}

getCsvData <- function(data_size_num_vec,
                       tp_average_vec,
                       fp_average_vec,
                       fn_average_vec) {
  data_vec <- c(tp_average_vec, fp_average_vec, fn_average_vec)
  
  data_csv_matrix <- matrix(data_vec,
                            nrow = length(data_vec) / 3,
                            ncol = 3)
  
  colnames(data_csv_matrix) <- c("tp_num_vec",
                                 "fp_num_vec",
                                 "fn_num_vec")
  
  rownames(data_csv_matrix) <- data_size_num_vec
  return(data_csv_matrix)
}

outputResult <-  function(result,
                          sample_size_vec,
                          data_name,
                          try_size) {
  tp_average_vec <- result[[1]]
  fp_average_vec <- result[[2]]
  fn_average_vec <- result[[3]]
  
  max_val_of_tp_average_vec <- max(tp_average_vec)
  max_val_of_fp_average_vec <- max(fp_average_vec)
  max_val_of_fn_average_vec <- max(fn_average_vec)
  
  max_val <- max(
    c(
      max_val_of_tp_average_vec,
      max_val_of_fp_average_vec,
      max_val_of_fn_average_vec
    )
  )
  max_val <- ceiling(max_val)
  
  plot_title <-
    paste("Data Name ", data_name, ", Try Number ", try_size)
  file_name <- paste(plot_title, ".png")
  png(file.path(RESULT_DIR_PATH, file_name))
  plotResult(sample_size_vec,
             tp_average_vec,
             "data size",
             "tp",
             plot_title)
  dev.off()
  
  plot_title <-
    paste("Data Name ", data_name, ", Try Number ", try_size)
  file_name <- paste(plot_title, ".png")
  png(file.path(RESULT_DIR_PATH, file_name))
  plotResult(sample_size_vec,
             fp_average_vec,
             "data size",
             "fp",
             plot_title)
  dev.off()
  
  plot_title <-
    paste("Data Name ", data_name, ", Try Number ", try_size)
  file_name <- paste(plot_title, ".png")
  png(file.path(RESULT_DIR_PATH, file_name))
  plotResult(sample_size_vec,
             fn_average_vec,
             "data size",
             "fn",
             plot_title)
  dev.off()
  
  #
  plot_title <-  paste(data_name,
                       " (TP-Red FP-blue FN-black) ",
                       "Try Number ",
                       try_size)
  file_name <- paste(plot_title, ".png")
  png(file.path(RESULT_DIR_PATH, file_name))
  plot(
    sample_size_vec,
    tp_average_vec,
    xlab = "data size",
    ylab = "",
    ylim = c(0, max_val),
    type = "b",
    col = "red"
  )
  par(new = T)
  plot(
    sample_size_vec,
    fp_average_vec,
    xlab = "",
    ylab = "",
    ylim = c(0, max_val),
    type = "b",
    col = "blue"
  )
  par(new = T)
  plot(
    sample_size_vec,
    fn_average_vec,
    xlab = "",
    ylab = "the number of arcs",
    ylim = c(0, max_val),
    type = "b",
    col = "black"
  )
  par(new = T)
  title(paste(
    data_name,
    " (TP:Red, FP:blue, FN:black), ",
    "Try Number : ",
    try_size
  ))
  par(new = F)
  dev.off()
  
  data.csv_matrix <- getCsvData(sample_size_vec,
                                tp_average_vec,
                                fp_average_vec,
                                fn_average_vec)
  file_name <- paste(data_name, ".csv")
  write.csv(data.csv_matrix,
            file.path(RESULT_DIR_PATH, file_name),
            quote = FALSE)
  
}

checkData <-  function(data,
                       sample_size_vec,
                       try_size,
                       correct_graph_string,
                       data_name,
                       skeleton = FALSE) {
  res_correct <- empty.graph(names(data))
  modelstring(res_correct) <- correct_graph_string
  if (skeleton) {
    res_correct <- skeleton(res_correct)
  }
  graphviz.plot(res_correct)
  
  result <-
    compareAndReturnTPFPFN(res_correct, sample_size_vec, try_size, data, skeleton)
  return(result)
}

outputFPResultwithOverride <-  function(result_directed,
                                        col_directed = "blue",
                                        result_undirected,
                                        col_undirected = "red",
                                        sample_size_vec,
                                        title,
                                        try_size) {
  result <- append(result_directed, result_undirected)
  file_name <- paste(title, "outputFPResultwithOverride" , ".png")
  png(file.path(RESULT_DIR_PATH, file_name))
  plot(
    sample_size_vec,
    result_directed,
    col = col_directed,
    ylim = c(0, ceiling(max(result))),
    xlab = paste(
      "data size, ",
      col_directed,
      "=directed, ",
      col_undirected ,
      "=undirected"
    ),
    ylab = "fp",
    type = "b"
  )
  par(new = T)
  plot(
    sample_size_vec,
    result_undirected,
    ylim = c(0, ceiling(max(result))),
    col = col_undirected,
    xlab = "",
    ylab = "",
    type = "b"
  )
  title(title)
  par(new = F)
  dev.off()
}

main <- function(try_size = 50,
                 skeleton = FALSE) {
  # check asia
  data(asia)
  asia_size_vec <- seq(10, 50, by = 5)
  asia_correct_graph_string <-
    "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
  asia_result_undirected <- checkData(asia,
                                      asia_size_vec,
                                      try_size,
                                      asia_correct_graph_string,
                                      "asia",
                                      skeleton = TRUE)
  asia_result_directed <- checkData(asia,
                                    asia_size_vec,
                                    try_size,
                                    asia_correct_graph_string,
                                    "asia",
                                    skeleton = FALSE)
  outputResult(asia_result_undirected,
               asia_size_vec,
               "asia_undirected",
               try_size)
  outputResult(asia_result_directed,
               asia_size_vec,
               "asia_directed",
               try_size)
  outputFPResultwithOverride(
    asia_result_directed[[2]],
    "blue",
    asia_result_undirected[[2]],
    "red",
    asia_size_vec,
    "asia fp",
    try_size
  )
  
  # insurance
  data(insurance)
  insurance_correct_graph_string <- paste(
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
  insurance_size_vec <- seq(10, 50, by = 5)
  insurance_result_undirected <- checkData(
    insurance,
    insurance_size_vec,
    try_size,
    insurance_correct_graph_string,
    "insurance",
    skeleton = TRUE
  )
  insurance_result_directed <- checkData(
    insurance,
    insurance_size_vec,
    try_size,
    insurance_correct_graph_string,
    "insurance",
    skeleton = FALSE
  )
  outputResult(
    insurance_result_undirected,
    insurance_size_vec,
    "insurance_undirected",
    try_size
  )
  outputResult(insurance_result_directed,
               insurance_size_vec,
               "insurance_directed",
               try_size)
  outputFPResultwithOverride(
    insurance_result_directed[[2]],
    "blue" ,
    insurance_result_undirected[[2]],
    "red",
    insurance_size_vec,
    "insurance fp",
    try_size
  )
  checkData(
    insurance,
    insurance_size_vec,
    try_size,
    insurance_correct_graph_string,
    "insurance",
    skeleton
  )
  
  if (FALSE) {
    # check alarm
    data(alarm)
    alarm_correct_graph_string <- paste(
      "[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
      "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA]",
      "[HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK]",
      "[MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB]",
      "[SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS]",
      "[VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV]",
      "[CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]",
      sep = ""
    )
    alarm_size_vec <- seq(10, 50, by = 5)
    alarm_result_undirected <- checkData(alarm,
                                         alarm_size_vec,
                                         try_size,
                                         alarm_correct_graph_string,
                                         "alarm",
                                         skeleton = TRUE)
    alarm_result_directed <- checkData(alarm,
                                       alarm_size_vec,
                                       try_size,
                                       alarm_correct_graph_string,
                                       "alarm",
                                       skeleton = FALSE)
    outputResult(alarm_result_undirected,
                 alarm_size_vec,
                 "alarm_undirected",
                 try_size)
    outputResult(alarm_result_directed,
                 alarm_size_vec,
                 "alarm_directed",
                 try_size)
    outputFPResultwithOverride(
      alarm_result_directed[[2]],
      "blue",
      alarm_result_undirected[[2]],
      "red",
      alarm_size_vec,
      "alarm fp",
      try_size
    )
    
    # clgaussian_test
    data(clgaussian_test)
    clgaussian_test_correct_graph_string <-
      "[A][B][C][H][D|A:H][F|B:C][E|B:D][G|A:D:E:F]"
    clgaussian_test_size_vec <- seq(10, 500, by = 5)
    clgaussian_test_result_undirected <- checkData(
      clgaussian_test,
      clgaussian_test_size_vec,
      try_size,
      clgaussian_test_correct_graph_string,
      "clgaussian_test",
      skeleton = TRUE
    )
    clgaussian_test_result_directed <- checkData(
      clgaussian_test,
      clgaussian_test_size_vec,
      try_size,
      clgaussian_test_correct_graph_string,
      "clgaussian_test",
      skeleton = FALSE
    )
    outputResult(
      clgaussian_test_result_undirected,
      clgaussian_test_size_vec,
      "clgaussian_test_undirected",
      try_size
    )
    outputResult(
      clgaussian_test_result_directed,
      clgaussian_test_size_vec,
      "clgaussian_test_directed",
      try_size
    )
    outputFPResultwithOverride(
      clgaussian_test_result_directed[[2]],
      "blue",
      clgaussian_test_result_undirected[[2]],
      "red",
      clgaussian_test_size_vec,
      "clgaussian_test fp",
      try_size
    )
    
    # gaussian_test
    data(gaussian_test)
    gaussian_test_correct_graph_string <-
      "[A][B][E][G][C|A:B][D|B][F|A:D:E:G]"
    gaussian_test_size_vec <- seq(10, 500, by = 5)
    gaussian_test_result_undirected <- checkData(
      gaussian_test,
      gaussian_test_size_vec,
      try_size,
      gaussian_test_correct_graph_string,
      "gaussian_test",
      skeleton = TRUE
    )
    gaussian_test_result_directed <- checkData(
      gaussian_test,
      gaussian_test_size_vec,
      try_size,
      gaussian_test_correct_graph_string,
      "gaussian_test",
      skeleton = FALSE
    )
    outputResult(
      gaussian_test_result_undirected,
      gaussian_test_size_vec,
      "gaussian_test_undirected",
      try_size
    )
    outputResult(
      gaussian_test_result_directed,
      gaussian_test_size_vec,
      "gaussian_test_directed",
      try_size
    )
    outputFPResultwithOverride(
      gaussian_test_result_directed[[2]],
      "blue" ,
      gaussian_test_result_undirected[[2]],
      "red",
      gaussian_test_size_vec,
      "gaussian_test fp",
      try_size
    )
    
    #learning_test
    data(learning_test)
    learning_test_correct_graph_string <-
      "[A][C][F][B|A][D|A:C][E|B:F]"
    learning_test_size_vec <- seq(10, 500, by = 5)
    learning_test_result_undirected <- checkData(
      learning_test,
      learning_test_size_vec,
      try_size,
      learning_test_correct_graph_string,
      "learning_test",
      skeleton = TRUE
    )
    learning_test_result_directed <- checkData(
      learning_test,
      learning_test_size_vec,
      try_size,
      learning_test_correct_graph_string,
      "learning_test",
      skeleton = FALSE
    )
    outputResult(
      learning_test_result_undirected,
      learning_test_size_vec,
      "learning_test_undirected",
      try_size
    )
    outputResult(
      learning_test_result_directed,
      learning_test_size_vec,
      "learning_test_directed",
      try_size
    )
    outputFPResultwithOverride(
      learning_test_result_directed[[2]],
      "blue" ,
      learning_test_result_undirected[[2]],
      "red",
      learning_test_size_vec,
      "learning_test fp",
      try_size
    )
    
    #lizards
    data(lizards)
    res = empty.graph(names(lizards))
    lizards_correct_graph_string <-
      "[Species][Diameter|Species][Height|Species]"
    lizards_size_vec <- seq(10, 500, by = 5)
    lizards_result_undirected <- checkData(
      lizards,
      lizards_size_vec,
      try_size,
      lizards_correct_graph_string,
      "lizards",
      skeleton = TRUE
    )
    lizards_result_directed <- checkData(
      lizards,
      lizards_size_vec,
      try_size,
      lizards_correct_graph_string,
      "lizards",
      skeleton = FALSE
    )
    outputResult(lizards_result_undirected,
                 lizards_size_vec,
                 "lizards_undirected",
                 try_size)
    outputResult(lizards_result_directed,
                 lizards_size_vec,
                 "lizards_directed",
                 try_size)
    outputFPResultwithOverride(
      lizards_result_directed[[2]],
      "blue" ,
      lizards_result_undirected[[2]],
      "red",
      lizards_size_vec,
      "lizards fp",
      try_size
    )
    
    
    #hailfinder
    data(hailfinder)
    hailfinder_correct_graph_string <- paste(
      "[N07muVerMo][SubjVertMo][QGVertMotion][SatContMoist][RaoContMoist]",
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
      sep = ""
    )
    hailfinder_size_vec <- seq(10, 500, by = 5)
    hailfinder_result_undirected <- checkData(
      hailfinder,
      hailfinder_size_vec,
      try_size,
      hailfinder_correct_graph_string,
      "hailfinder",
      skeleton = TRUE
    )
    hailfinder_result_directed <- checkData(
      hailfinder,
      hailfinder_size_vec,
      try_size,
      hailfinder_correct_graph_string,
      "hailfinder",
      skeleton = FALSE
    )
    outputResult(
      hailfinder_result_undirected,
      hailfinder_size_vec,
      "hailfinder_undirected",
      try_size
    )
    outputResult(
      hailfinder_result_directed,
      hailfinder_size_vec,
      "hailfinder_directed",
      try_size
    )
    outputFPResultwithOverride(
      hailfinder_result_directed[[2]],
      "blue" ,
      hailfinder_result_undirected[[2]],
      "red",
      hailfinder_size_vec,
      "hailfinder fp",
      try_size
    )
  }
}

main(try_size = 50, FALSE)
