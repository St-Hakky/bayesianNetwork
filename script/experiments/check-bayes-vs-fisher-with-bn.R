library(bnlearn)
RESULT_DIR_PATH <- file.path(getwd(), "report", "script", "experiments", "check-bayes-vs-fisher-with-bn")
dir.create(file.path(getwd(), "report"), showWarnings = FALSE)
dir.create(file.path(getwd(), "report", "script"), showWarnings = FALSE)
dir.create(file.path(getwd(), "report", "script", "experiments"), showWarnings = FALSE)
dir.create(file.path(getwd(), "report", "script", "experiments", "check-bayes-vs-fisher-with-bn"), showWarnings = FALSE)


plotResult <- function(x, y, p_value, xlab, ylab, title_main, p_line_col = "blue",
  BIC_eq_col = "red") {
  plot(x, y, xlab = xlab, ylab = ylab, ylim = c(0, 1), main = title_main)
  par(new = T)
  abline(h = p_value, col = p_line_col)
  abline(v = 0, col = BIC_eq_col)
  par(new = F)
}

getSampleData <- function(data, data_length, random_length) {
  random_index <- sort(floor(runif(random_length) * data_length))
  random_val <- data[random_index, ]
  return(random_val)
}

printResultbyLine <- function(independence_betterScore, independence_warseScore,
  dependence_betterScore, dependence_warseScore) {
  total <- independence_betterScore +
           independence_warseScore +
           dependence_betterScore +
           dependence_warseScore
  print(paste("independence is true and BIC score is better : ",
              independence_betterScore,
              "(",
              100 * independence_betterScore/total, " parcent)"))
  print(paste("independence is true and BIC score is warse : ",
              independence_warseScore,
              "(",
              100 * independence_warseScore/total, " parcent)"))
  print(paste("independence is false and BIC score is better : ",
              dependence_betterScore,
              "(",
              100 * dependence_betterScore/total, " parcent)"))
  print(paste("independence is false and BIC score is warse : ",
              dependence_warseScore,
              "(",
              100 * dependence_warseScore/total, " parcent)"))
}

printResultbyTable <- function(independence_betterScore,
                               independence_warseScore,
                               dependence_betterScore,
                               dependence_warseScore,
                               colnames = c("independence", "dependence"),
                               rownames = c("better score", "warse score")) {
  total <- independence_betterScore +
           independence_warseScore +
           dependence_betterScore +
           dependence_warseScore

  result_table <- matrix(0, 2, 2)
  result_table[1, ] <- c(independence_betterScore, dependence_betterScore)
  result_table[2, ] <- c(independence_warseScore, dependence_warseScore)

  colnames(result_table) <- colnames
  rownames(result_table) <- rownames
  
  print(result_table)
  result_table[1, ] <- c(100 * independence_betterScore/total, 100 * dependence_betterScore/total)
  result_table[2, ] <- c(100 * independence_warseScore/total, 100 * dependence_warseScore/total)
  print(result_table)
}

plotData <- function(x, y, xlab, ylab, col = col) {
  plot(x, y, xlab = xlab, ylab = ylab, col = col, xlim = c(0, max(x)), ylim = c(0,
    100), type = "b")
  par(new = T)
}

getCsvData <- function(data_size_num_vec,
                       independence_betterScore_vec,
                       independence_warseScore_vec,
                       dependence_betterScore_vec,
                       dependence_warseScore_vec) {

  data_vec <- c(independence_betterScore_vec,
                independence_warseScore_vec,
                dependence_betterScore_vec,
                dependence_warseScore_vec)

  data_csv_matrix <- matrix(data_vec,
                            nrow = length(data_vec)/4,
                            ncol = 4)

  colnames(data_csv_matrix) <- c("fp_independence_betterScore_vec",
                                 "fp_independence_warseScore_vec",
                                 "fp_dependence_betterScore_vec",
                                 "fp_dependence_warseScore_vec")
  rownames(data_csv_matrix) <- data_size_num_vec
  return(data_csv_matrix)
}


getScore <- function(type, data) {
  if (type == "bic" || type == "aic") {

    # dataFrame
    data <- data.frame(A = data[, 1], B = data[, 2])
    data$A <- as.factor(data$A)
    data$B <- as.factor(data$B)

    # pre_score
    res <- empty.graph(names(data))
    pre_score <- -2 * score(res, data, type = type)

    # pro_score from A to B
    res <- set.arc(res, "A", "B")
    pro_score <- -2 * score(res, data, type = type)
  }
  return(list(pre_score, pro_score))
}

getTableData <- function(data, val_0 = "no", val_1 = "yes") {
  data1 <- data[, 1]
  data2 <- data[, 2]
  data_length <- length(data[, 1])
  a_sum <- 0
  b_sum <- 0
  c_sum <- 0
  d_sum <- 0
  for (i in 1:data_length) {
    if (data1[i] == val_1 && data2[i] == val_0) {
      a_sum <- a_sum + 1
    } else if (data1[i] == val_0 && data2[i] == val_0) {
      b_sum <- b_sum + 1
    } else if (data1[i] == val_1 && data2[i] == val_1) {
      c_sum <- c_sum + 1
    } else {
      d_sum <- d_sum + 1
    }
  }

  data_table <- matrix(c(a_sum, b_sum, c_sum, d_sum),
                       nrow = 2,
                       byrow = T,
                       dimnames = list(c("B_0","B_1"), c("A_1", "A_0")))

  return(data_table)
}

compareBICandFisher <- function(data, data_size_vec, try_size, bn_graph_correct,
  data_name, skeleton, two_side = FALSE) {
  data_index <- 0
  for (data_size in data_size_vec) {
    plot_index <- 0

    # fp
    fp_independence_betterScore <- 0
    fp_independence_warseScore <- 0
    fp_dependence_betterScore <- 0
    fp_dependence_warseScore <- 0

    print(paste("data_size : ", data_size))
    for (i in 1:try_size) {
      sample_data <- getSampleData(data, length(data[, 1]), data_size)
      bn_graph_learn <- hc(sample_data)
      # graphviz.plot(bn_graph_learn)
      if (skeleton) {
        bn_graph_learn <- skeleton(bn_graph_learn)
        # graphviz.plot(bn_graph_learn)
      }

      # get tp, fp, fn arcs
      diff_table <- compare(target = bn_graph_correct,
                            current = bn_graph_learn,
                            arcs = TRUE)
      # print('start : fp')
      if (length(diff_table$fp[, 1]) != 0) {
        # print(diff_table$fp)
        for (i in 1:length(diff_table$fp[, 1])) {
          fp_data <- sample_data[diff_table$fp[i, ]]
          score <- getScore("bic", fp_data)
          pre_score <- score[[1]]
          pro_score <- score[[2]]

          # fisher
          data_fisher <- getTableData(fp_data)
          if (two_side) {
            res_fisher <- fisher.test(data_fisher)
          } else {
            res_fisher_less <- fisher.test(data_fisher, alternative = "l")
            res_fisher_greater <- fisher.test(data_fisher, alternative = "g")
            print(res_fisher_less)
          if (res_fisher_less$p.value > res_fisher_greater$p.value) {
            res_fisher <- res_fisher_greater
          } else {
            res_fisher <- res_fisher_less
          }
        }
        # print(paste('res_fisher_less_p_value : ', res_fisher_less$p.value))
        # print(paste('res_fisher_greater_p_value : ', res_fisher_greater$p.value))
        # print(paste('res_fisher : ', res_fisher$p.value))

          if (plot_index == 0) {
          x <- c(pre_score - pro_score)
          y <- c(res_fisher$p.value)
          } else {
          x <- append(x, pre_score - pro_score)
          y <- append(y, res_fisher$p.value)
          }
          plot_index <- plot_index + 1

          if ((pre_score - pro_score < 0) && res_fisher$p.value > 0.05) {
          #
          fp_independence_warseScore <- fp_independence_warseScore + 1
          # print(paste('fp_independence_warseScore : ',diff_table$fp[i,]))
          } else if ((pre_score - pro_score >= 0) && res_fisher$p.value > 0.05) {
          fp_independence_betterScore <- fp_independence_betterScore +
            1
          # print(paste('fp_independence_betterScore : ', diff_table$fp[i,]))
          } else if ((pre_score - pro_score < 0) && (res_fisher$p.value <=
          0.05)) {
          fp_dependence_warseScore <- fp_dependence_warseScore + 1
          # print(paste('fp_dependence_warseScore : ', diff_table$fp[i,]))
          } else {
          fp_dependence_betterScore <- fp_dependence_betterScore + 1
          # print(paste('fp_dependence_betterScore : ', diff_table$fp[i,]))
          }
        }
      }
    }

    total <- fp_independence_betterScore + fp_independence_warseScore + fp_dependence_betterScore +
      fp_dependence_warseScore
    if (data_index == 0) {
      fp_independence_betterScore_vec <- c(100 * fp_independence_betterScore/total)
      fp_independence_warseScore_vec <- c(100 * fp_independence_warseScore/total)
      fp_dependence_betterScore_vec <- c(100 * fp_dependence_betterScore/total)
      fp_dependence_warseScore_vec <- c(100 * fp_dependence_warseScore/total)
    } else {
      fp_independence_betterScore_vec <- append(fp_independence_betterScore_vec,
        100 * fp_independence_betterScore/total)
      fp_independence_warseScore_vec <- append(fp_independence_warseScore_vec,
        100 * fp_independence_warseScore/total)
      fp_dependence_betterScore_vec <- append(fp_dependence_betterScore_vec,
        100 * fp_dependence_betterScore/total)
      fp_dependence_warseScore_vec <- append(fp_dependence_warseScore_vec, 100 *
        fp_dependence_warseScore/total)
    }
    data_index <- data_index + 1
    png(file.path(RESULT_DIR_PATH, paste("FP two side ", two_side, " Data Size ", data_size, ".png")))
    if (two_side) {
      plotResult(x, y, 0.05, "BIC score diff", "p-value", paste("fp two_side Data size : ",
        data_size))
    } else {
      plotResult(x, y, 0.05, "BIC score diff", "p-value", paste("fp one_side Data size : ",
        data_size))
    }
    dev.off()
    print(paste("Data Size : ", data_size))
    printResultbyTable(fp_independence_betterScore, fp_independence_warseScore,
      fp_dependence_betterScore, fp_dependence_warseScore)
    printResultbyLine(fp_independence_betterScore, fp_independence_warseScore,
      fp_dependence_betterScore, fp_dependence_warseScore)
  }
  png(file.path(RESULT_DIR_PATH, "all.png"))
  plotData(data_size_vec, fp_independence_betterScore_vec, "Data Size", "independence and better Score",
    "red")
  plotData(data_size_vec, fp_independence_warseScore_vec, "Data Size", "independence and warse Score",
    "blue")
  plotData(data_size_vec, fp_dependence_betterScore_vec, "Data Size", "dependence and better Score",
    "black")
  plotData(data_size_vec, fp_dependence_warseScore_vec, "Data Size", "dependence and warseScore",
    "green")
  par(new = F)
  dev.off()
  data_csv_matrix <- getCsvData(data_size_vec, fp_independence_betterScore_vec,
    fp_independence_warseScore_vec, fp_dependence_betterScore_vec, fp_dependence_warseScore_vec)
  print(data_csv_matrix)
  write_csv(data_csv_matrix, "data_csv", quote = FALSE)

  # return (list(tp_parcent_vec, fp_parcent_vec, fn_parcent_vec))
}

checkData <- function(data, data_size_vec, try_size, correct_graph_string, data_name,
  skeleton = FALSE, two_side = FALSE) {
  print(data_name)
  # correct graph data
  res_correct <- empty.graph(names(data))
  modelstring(res_correct) = correct_graph_string
  if (skeleton) {
    res_correct <- skeleton(res_correct)
  }
  graphviz.plot(res_correct)

  result <- compareBICandFisher(data, data_size_vec, try_size, res_correct, data_name,
    skeleton, two_side)
  return(result)
}

main <- function(try_size = 50, two_side = FALSE) {
  data(asia)
  # asia_size_vec = c(10,50,100,500,1000,2000,3000,4000,5000)
  asia_size_vec <- seq(10, 500, by = 5)
  asia_correct_graph_string <- "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
  asia_result_directed <- checkData(asia, asia_size_vec, try_size, asia_correct_graph_string,
    "asia", skeleton = "FALSE", two_side)
  # asia_result_undirected = checkData(asia, asia_size_vec, try_size,
  # asia_correct_graph_string, 'asia', skeleton='TRUE',two_side)
}

main(two_side = TRUE)
