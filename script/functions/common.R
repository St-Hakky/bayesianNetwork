library(bnlearn)
library(ggplot2)

getTableData <- function(data) {
  data1 <- data[, 1]
  data2 <- data[, 2]
  
  data_vec <- c()
  for (i in 1:length(levels(data1))) {
    for (j in 1:length(levels(data2))) {
      len <- length(data[data[, 1] == levels(data1)[i] &
                           data[, 2] == levels(data2)[j], ][, 1])
      data_vec <- append(data_vec, len)
    }
  }
  data_table <- matrix(
    data_vec,
    nrow = length(levels(data1)),
    byrow = T,
    dimnames = list(levels(data1), levels(data2))
  )
  return(data_table)
}


getSampleData <- function(data, data_length, random_length) {
  random_index <- sort(floor(runif(random_length) * data_length))
  random_val <- data[random_index,]
  return(random_val)
}

getPvalueWithArcs <- function(data, arcs) {
  arcs_p_value_vec = c()
  for (i in 1:length(arcs[, 1])) {
    data_fisher = getTableData(data[arcs[i,]])
    res_fisher = fisher.test(data_fisher)
    arcs_p_value_vec = append(arcs_p_value_vec, res_fisher$p_value)
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
  data_boxplot <- list()
  index <- 1
  for (data_size in data_size_vec) {
    tp_p_value_vec <- c()
    fp_p_value_vec <- c()
    for (i in 1:try_size) {
      sample_data <- getSampleData(data, length(data[, 1]), data_size)
      res_learn <- hc(sample_data)
      
      diff_table <- compare(target = res_correct,
                            current = res_learn,
                            arcs = TRUE)
      if (length(diff_table$tp[, 1]) != 0) {
        tp_p_value_vec <- append(tp_p_value_vec,
                                 getPvalueWithArcs(sample_data, diff_table$tp))
      }
      
      if (length(diff_table$fp[, 1]) != 0) {
        fp_p_value_vec <- append(fp_p_value_vec,
                                 getPvalueWithArcs(sample_data, diff_table$fp))
      }
    }
    data_boxplot <- c(data_boxplot, list(list()))
    data_boxplot[[index]] <-
      c(data_boxplot[[index]], list(tp_p_value_vec))
    data_boxplot[[index]] <-
      c(data_boxplot[[index]], list(fp_p_value_vec))
    index <- index + 1
    print(index)
  }
  return(data_boxplot)
}
