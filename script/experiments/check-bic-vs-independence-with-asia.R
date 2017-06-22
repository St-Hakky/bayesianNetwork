library(bnlearn)
library(dplyr)

printResultbyLine <- function(independence_betterScore, independence_warseScore,
  dependence_betterScore, dependence_warseScore) {
  total <- independence_betterScore + independence_warseScore + dependence_betterScore +
    dependence_warseScore
  print(paste("independence is true and BIC score is better : ", independence_betterScore,
    "(", 100 * independence_betterScore/total, " parcent)"))
  print(paste("independence is true and BIC score is warse : ", independence_warseScore,
    "(", 100 * independence_warseScore/total, " parcent)"))
  print(paste("independence is false and BIC score is better : ", dependence_betterScore,
    "(", 100 * dependence_betterScore/total, " parcent)"))
  print(paste("independence is false and BIC score is warse : ", dependence_warseScore,
    "(", 100 * dependence_warseScore/total, " parcent)"))
}

printResultbyTable <- function(independence_betterScore,
                               independence_warseScore,
                               dependence_betterScore,
                               dependence_warseScore,
                               colnames = c("independence", "dependence"),
                               rownames = c("better score", "warse score")) {
  total <- independence_betterScore + independence_warseScore + dependence_betterScore +
    dependence_warseScore
  result_table <- matrix(0, 2, 2)
  result_table[1, ] <- c(independence_betterScore, independence_warseScore)
  result_table[2, ] <- c(dependence_betterScore, dependence_warseScore)
  colnames(result_table) <- colnames
  rownames(result_table) <- rownames
  print(result_table)
  result_table[1, ] <- c(100 * independence_betterScore/total, 100 * independence_warseScore/total)
  result_table[2, ] <- c(100 * dependence_betterScore/total, 100 * dependence_warseScore/total)
  print(result_table)
}

plotResult <- function(x, y, p_value,
                       xlab, ylab, title_main,
                       title_sub, p_line_col = "blue",
                       BIC_eq_col = "red") {
  plot(x, y, xlab = xlab, ylab = ylab, ylim = c(0, 1))
  par(new = T)
  title(title_main)
  abline(h = p_value/2, col = p_line_col)
  abline(h = 1 - p_value/2, col = p_line_col)
  abline(v = 0, col = BIC_eq_col)
  par(new = F)
}

getScore <- function(type, a_sum_i, b_sum_i, c_sum_i, d_sum_i) {
  if (type == "bic" || type == "aic") {
    # dataFrame
    data <- getDataFrame(a_sum_i, b_sum_i, c_sum_i, d_sum_i)
    data$A <- as.factor(data$A)
    data$B <- as.factor(data$B)

    # pre_score
    res <- empty.graph(names(data))
    pre_score <- -2 * score(res, data, type = type)

    # pro_score from A to B
    res <- set.arc(res, "A", "B")
    pro_score_fromAtoB <- -2 * score(res, data, type = type)

    # pro_score from B to A
    res <- empty.graph(names(data))
    res <- set.arc(res, "B", "A")
    pro_score_fromBtoA <- -2 * score(res, data, type = type)

    # the smaller score is better
    if (pro_score_fromAtoB < pro_score_fromBtoA) {
      pro_score <- pro_score_fromAtoB
    } else {
      pro_score <- pro_score_fromBtoA
    }

  }
  return(list(pre_score, pro_score))
}


plotData <- function(x, y, xlab, ylab, col = col, title, save = "false") {
  plot(x, y, xlab = xlab, ylab = ylab,
       col = col, xlim = c(0, 140), ylim = c(0, 100),
       type = "b")
  par(new = T)
  title(title)
  par(new = T)
}


getRandomValue <- function(data, data_length, random_length) {
  random_index <- sort(round(runif(random_length) * data_length))
  random_val <- asia[random_index, ]
  rownames(random_val) <- c(1:random_length)
  return(random_val)
}

getRowData <- function(data1, data2, data1_name, data2_name) {
  data_row <- cbind(data1, data2)
  data_row[data_row == 1] = 0
  data_row[data_row == 2] = 1
  add_matrix <- cbind(c(0, 0, 1, 1), c(0, 1, 0, 1))
  data_row <- rbind(data_row, add_matrix)
  colnames(data_row) <- c(data1_name, data2_name)
  return(data_row)
}


getTableData <- function(data1, data2) {
  data_length <- length(data1)
  a_sum <- 1
  b_sum <- 1
  c_sum <- 1
  d_sum <- 1
  for (i in 1:data_length) {
    if (data1[i] == "yes" && data2[i] == "no") {
      a_sum <- a_sum + 1
    } else if (data1[i] == "no" && data2[i] == "no") {
      b_sum <- b_sum + 1
    } else if (data1[i] == "yes" && data2[i] == "yes") {
      c_sum <- c_sum + 1
    } else {
      d_sum <- d_sum + 1
    }
  }
  data_table <- matrix(c(a_sum, b_sum, c_sum, d_sum),
                       nrow = 2,
                       byrow = T,
                       dimnames = list(c("B_0", "B_1"), c("A_1", "A_0")))
  return(data_table)
}

getSampleData <- function(try_size, data, data_names, sample_data_size) {
  sample_table_data <- list()
  sample_row_data <- list()
  for (try_num in 1:try_size) {
    random_val <- getRandomValue(asia, length(data[, 1]), sample_data_size - 4)
    index <- 1
    for (i in 1:length(data)) {
      for (j in i:length(data)) {
        if (i != j) {
          data_table <- getTableData(random_val[, i], random_val[, j])
          random_val[, i]
          data_row <- getRowData(random_val[, i], random_val[, j], data_names[i],
          data_names[j])
          if (try_num == 1) {
          sample_table_data <- c(sample_table_data, list(list()))
          sample_row_data <- c(sample_row_data, list(list()))
          # print(sample_table_data)
          sample_table_data[[index]] <- c(sample_table_data[[index]], list(data_table))
          sample_row_data[[index]] <- c(sample_row_data[[index]], list(data_row))
          } else {
          sample_table_data[[index]] <- c(sample_table_data[[index]], list(data_table))
          sample_row_data[[index]] <- c(sample_row_data[[index]], list(data_row))
          }
          index <- index + 1
        }
      }
    }
  }
  return(list(sample_table_data, sample_row_data))
}

getDataFrame <- function(data) {
  a <- data[, 1]
  b <- data[, 2]
  data <- data.frame(A = a, B = b)
  return(data)
}

getScore <- function(data, type = "bic") {
  data <- getDataFrame(data)
  data[, 1] <- as.factor(data[, 1])
  data[, 2] <- as.factor(data[, 2])

  # pre_score
  res <- empty.graph(colnames(data))
  pre_score <- -2 * score(res, data, type = type)

  # pro_score from A to B
  res <- set.arc(res, "A", "B")
  pro_score_fromAtoB <- -2 * score(res, data, type = type)

  # pro_score from B to A
  res <- empty.graph(names(data))
  res <- set.arc(res, "B", "A")
  pro_score_fromBtoA <- -2 * score(res, data, type = type)

  if (pro_score_fromAtoB < pro_score_fromBtoA) {
    pro_score <- pro_score_fromBtoA
  } else {
    pro_score <- pro_score_fromAtoB
  }
  return(list(pre_score, pro_score))

}

main <- function(data, sample_data_size_vec = c(10), try_size = 20) {

  data_names <- names(data)
  data_length <- length(data[, data_names[1]])

  independence_betterScore_list <- list()
  independence_warseScore_list <- list()
  dependence_betterScore_list <- list()
  dependence_warseScore_list <- list()

  sample_data_index <- 1

  for (sample_data_size in sample_data_size_vec) {
    print(paste("data size : ", sample_data_size))
    sample_data <- getSampleData(try_size, data, data_names, sample_data_size)
    sample_table_data <- sample_data[[1]]
    sample_row_data <- sample_data[[2]]
    for (i in 1:length(sample_row_data)) {
      independence_betterScore <- 0
      independence_warseScore <- 0
      dependence_betterScore <- 0
      dependence_warseScore <- 0
      plot_index <- 0
      print(paste("i : ", i))
      for (j in 1:length(sample_row_data[[i]])) {
        # print(paste('i : ', i, 'j : ', j))
        score <- getScore(sample_row_data[[i]][[j]])
        pre_score <- score[[1]]
        pro_score <- score[[2]]
        res_fisher <- fisher.test(sample_table_data[[i]][[j]])
        if (plot_index == 0) {
          x <- c(pre_score - pro_score)
          y <- c(res_fisher$p.value)
        } else {
          x <- append(x, pre_score - pro_score)
          y <- append(y, res_fisher$p.value)
        }
        plot_index <- plot_index + 1

        if ((pre_score - pro_score < 0) && res_fisher$p.value > 0.025 &&
          res_fisher$p.value < 0.975) {
          #
          independence_warseScore <- independence_warseScore + 1
        } else if ((pre_score - pro_score >= 0) && res_fisher$p.value > 0.025 &&
          res_fisher$p.value < 0.975) {
          #
          independence_betterScore <- independence_betterScore + 1
        } else if ((pre_score - pro_score < 0) && (res_fisher$p.value <= 0.025 ||
          res_fisher$p.value >= 0.975)) {
          #
          dependence_warseScore <- dependence_warseScore + 1
        } else {
          #
          dependence_betterScore <- dependence_betterScore + 1
        }

      }

      if (sample_data_index == 1) {
        independence_betterScore_list <- c(independence_betterScore_list,
          list(list()))
        independence_warseScore_list <- c(independence_warseScore_list, list(list()))
        dependence_betterScore_list <- c(dependence_betterScore_list, list(list()))
        dependence_warseScore_list <- c(dependence_warseScore_list, list(list()))

        independence_betterScore_list[[i]] <- c(independence_betterScore_list[[i]],
                                                list(100 * independence_betterScore/try_size))

        independence_warseScore_list[[i]] <- c(independence_warseScore_list[[i]],
                                               list(100 * independence_warseScore/try_size))
        dependence_betterScore_list[[i]] <- c(dependence_betterScore_list[[i]],
                                              list(100 * dependence_betterScore/try_size))
        dependence_warseScore_list[[i]] <- c(dependence_warseScore_list[[i]],
                                             list(100 * dependence_warseScore/try_size))
      } else {
        independence_betterScore_list[[i]] <- c(independence_betterScore_list[[i]],
                                                list(100 * independence_betterScore/try_size))
        independence_warseScore_list[[i]] = c(independence_warseScore_list[[i]],
          list(100 * independence_warseScore/try_size))
        dependence_betterScore_list[[i]] = c(dependence_betterScore_list[[i]],
          list(100 * dependence_betterScore/try_size))
        dependence_warseScore_list[[i]] = c(dependence_warseScore_list[[i]],
          list(100 * dependence_warseScore/try_size))
      }
      printResultbyTable(independence_betterScore, independence_warseScore,
        dependence_betterScore, dependence_warseScore)
      printResultbyLine(independence_betterScore, independence_warseScore,
        dependence_betterScore, dependence_warseScore)
      plotResult(x, y, 0.05, "BIC score diff", "p-value", paste("try size : ",
        try_size, "Data Size : ", sample_data_size), paste("set : ", i))
    }
    sample_data_index <- sample_data_index + 1
  }
  for (s in 1:28) {
    print(independence_betterScore_list[[s]])
    for (t in 1:length(independence_betterScore_list[[s]])) {
      if (t == 1) {
        independence_betterScore_vec <- c(independence_betterScore_list[[s]][[t]])
        independence_warseScore_vec <- c(independence_warseScore_list[[s]][[t]])
        dependence_betterScore_vec <- c(dependence_betterScore_list[[s]][[t]])
        dependence_warseScore_vec <- c(dependence_warseScore_list[[s]][[t]])
      } else {
        independence_betterScore_vec <- append(independence_betterScore_vec,
          independence_betterScore_list[[s]][[t]])
        independence_warseScore_vec <- append(independence_warseScore_vec,
          independence_warseScore_list[[s]][[t]])
        dependence_betterScore_vec <- append(dependence_betterScore_vec, dependence_betterScore_list[[s]][[t]])
        dependence_warseScore_vec <- append(dependence_warseScore_vec, dependence_warseScore_list[[s]][[t]])
      }
    }
    plotData(sample_data_size_vec, independence_betterScore_vec,
             "", "", "red", "", "start")
    plotData(sample_data_size_vec, independence_warseScore_vec,
             "", "", "blue", "")
    plotData(sample_data_size_vec, dependence_betterScore_vec,
             "", "", "black", "")
    plotData(sample_data_size_vec, dependence_warseScore_vec, "Data Size", "parcentage",
      "yellow", paste("patern : ", s), "end")
    par(new = F)
  }
}

data(asia)
main(asia, c(10, 20, 30, 40, 50, 60, 70), 50)
