library(bnlearn)
library(ggplot2)
library(reshape2)
RESULT_DIR_PATH <- file.path(getwd(), "report", "script", "experiments", "check-bic-vs-independence")
dir.create(file.path(getwd(), "report"), showWarnings = FALSE)
dir.create(file.path(getwd(), "report", "script"), showWarnings = FALSE)
dir.create(file.path(getwd(), "report", "script", "experiments"), showWarnings = FALSE)
dir.create(file.path(getwd(), "report", "script", "experiments", "check-bic-vs-independence"), showWarnings = FALSE)


plotData <- function(data_size_vec,
                     independence_betterScore_vec,
                     independence_warseScore_vec,
                     dependence_betterScore_vec,
                     dependence_warseScore_vec,
                     xlab, ylab, col=col){
  data <- data.frame(id = data_size_vec,
                     independence_betterScore = independence_betterScore_vec,
                     independence_warseScore = independence_warseScore_vec,
                     dependence_betterScore = dependence_betterScore_vec,
                     dependence_warseScore = dependence_warseScore_vec)
  data <- melt(data, id_var = c("id"))
  g <- ggplot(
    data,
    aes(
      x = id,
      y = value,
      group = variable,
      colour = variable
    )
  )
  g <- g + geom_line()
  g <- g + coord_cartesian(xlim = c(0, max(data_size_vec)), ylim=c(0, 100))
  g <- g + theme_bw()
  g <- g + theme(axis.text=element_text(size=24),
                 axis.title=element_text(size=28),
                 plot.title=element_text(size=32))
  plot(g)
  ggsave(file.path(RESULT_DIR_PATH, paste("all", ".wmf")), g)

}

plotResult <- function(x, y, p_value, xlab, ylab, data_size_num, p_line_col="blue", BIC_eq_col="red"){
  data <- data.frame(x = x, y = y)
  g <- ggplot(
    data,
    aes (
      x = x,
      y = y
    )
  )

  g <- g + geom_point()
  g <- g + xlab(xlab) + ylab(ylab) + ggtitle(paste("Data size :" , data_size_num))
  g <- g + coord_cartesian(xlim = c(min(x), max(x)), ylim=c(0, 1))
  g <- g + geom_hline(yintercept=p_value, linetype="dashed" ,colour="red")
  g <- g + geom_vline(xintercept=0,linetype="dashed" , colour="red")
  g <- g + theme_bw()
  g <- g + theme(axis.text=element_text(size=24),
          axis.title=element_text(size=28),
          plot.title=element_text(size=32))
  plot(g)
  ggsave(file.path(RESULT_DIR_PATH, paste("Data size", data_size_num, ".wmf")), g)

#  for(i in 1:length(x)){
#    plot(x[i],y[i],xlab=xlab, ylab=ylab,xlim=c(-5,5), ylim=c(0,1),type="n")
#    par(new=T)
#    title(paste(title_main, i))
#    abline(h = p_value/2, col=p_line_col)
#    abline(h = 1-p_value/2, col=p_line_col)
#    abline(v = 0, col=BIC_eq_col)
#    text(x[i],y[i], i)
#    par(new=F)
#  }
}

printResultbyLine <- function(independence_betterScore, independence_warseScore,
                              dependence_betterScore, dependence_warseScore){
  total <- independence_betterScore + independence_warseScore + dependence_betterScore + dependence_warseScore
  print (paste("independence is true and BIC score is better : ", independence_betterScore, "(", 100*independence_betterScore/total," parcent)"))
  print (paste("independence is true and BIC score is warse : ", independence_warseScore, "(", 100*independence_warseScore/total," parcent)"))
  print (paste("independence is false and BIC score is better : ", dependence_betterScore, "(", 100*dependence_betterScore/total," parcent)"))
  print (paste("independence is false and BIC score is warse : ", dependence_warseScore, "(", 100*dependence_warseScore/total," parcent)"))
}


printResultbyTable <- function(independence_betterScore, independence_warseScore,
                               dependence_betterScore,dependence_warseScore,
                               colnames = c("independence", "dependence"),
                               rownames = c("better score", "warse score")){
  total <- independence_betterScore +
           independence_warseScore +
           dependence_betterScore +
           dependence_warseScore

  result_table <- matrix(0,2,2)
  result_table[1,] <- c(independence_betterScore, dependence_betterScore)
  result_table[2,] <- c(independence_warseScore, dependence_warseScore)

  colnames(result_table) <- colnames
  rownames(result_table) <- rownames
  print (result_table)

  result_table[1,] <- c(100*independence_betterScore/total, 100*dependence_betterScore/total)
  result_table[2,] <- c(100*independence_warseScore/total, 100*dependence_warseScore/total)
  print (result_table)
}

getDataFrame <- function(a_sum, b_sum, c_sum, d_sum){
  a <- c(rep(0, a_sum), rep(1, b_sum), rep(0, c_sum), rep(1, d_sum))
  b <- c(rep(0, a_sum), rep(0, b_sum), rep(1, c_sum), rep(1, d_sum))
  data <- data.frame(A=a, B=b)
  return(data)
}

getScore <- function(type, a_sum_i, b_sum_i,c_sum_i,d_sum_i){
  if(type == "bic" || type == "aic"){
    # dataFrame
    data <- getDataFrame(a_sum_i,b_sum_i,c_sum_i,d_sum_i)
    data$A <- as.factor(data$A)
    data$B <- as.factor(data$B)

    # pre_score
    res <- empty.graph(names(data))
    #pre_score <- -2 * score(res, data, type=type)
    pre_score <- 2 * score(res, data, type=type)

    # pro_score from A to B
    res <- set.arc(res,"A","B")
    #pro_score_fromAtoB <- -2 * score(res, data, type=type)
    pro_score_fromAtoB <- 2 * score(res, data, type=type)

    # pro_score from B to A
    res <- empty.graph(names(data))
    res <- set.arc(res,"B","A")
    #pro_score_fromBtoA = -2 * score(res, data, type=type)
    pro_score_fromBtoA <- 2 * score(res, data, type=type)

    # the smaller score is better
    if(pro_score_fromAtoB > pro_score_fromBtoA){
      pro_score <- pro_score_fromAtoB
    }else{
      pro_score <- pro_score_fromBtoA
    }
  }
  return (list(pre_score, pro_score))
}


setAllPaternDataInBoolean <- function(sum_val=10, min_val=1){
  index <- 0;
  for(i in min_val:sum_val){
    for(j in min_val:sum_val){
      for(k in min_val:sum_val){
        for(l in min_val:sum_val){
          if(i + j + k + l == sum_val){
            if(index == 0){
              a_sum <- c(i)
              b_sum <- c(j)
              c_sum <- c(k)
              d_sum <- c(l)
            }else{
              a_sum <- append(a_sum,i)
              b_sum <- append(b_sum,j)
              c_sum <- append(c_sum,k)
              d_sum <- append(d_sum,l)
            }
            index <- index + 1
          }
        }
      }
    }
  }
  return (list(a_sum,b_sum,c_sum,d_sum))
}

getCsvData <- function(data_size_num_vec,
                       independence_betterScore_vec,
                       independence_warseScore_vec,
                       dependence_betterScore_vec,
                       dependence_warseScore_vec){
  data_vec <- c(independence_betterScore_vec,
                independence_warseScore_vec,
                dependence_betterScore_vec,
                dependence_warseScore_vec)
  print(data_vec)
  data_csv_matrix <- matrix(data_vec, nrow=length(data_vec)/4, ncol=4)
  print(data_csv_matrix)
  colnames(data_csv_matrix) <- c("independence_betterScore_vec",
                                 "independence_warseScore_vec",
                                 "dependence_betterScore_vec",
                                 "dependence_warseScore_vec")
  rownames(data_csv_matrix) <- data_size_num_vec
  return(data_csv_matrix)
}

main <- function(data_size_num_vec = c(10), p_value= 0.05, two_side = FALSE){
  data_index <- 0
  for(data_size_num in data_size_num_vec){
    ptm <- proc.time()
    plot_index <- 0
    table_value <- setAllPaternDataInBoolean(data_size_num)

    a_sum <- table_value[[1]]
    b_sum <- table_value[[2]]
    c_sum <- table_value[[3]]
    d_sum <- table_value[[4]]

    independence_betterScore <- 0
    independence_warseScore <- 0
    dependence_betterScore <- 0
    dependence_warseScore <- 0
    for(i in 1:length(a_sum)){

      if(a_sum[i] != 0 && b_sum[i] != 0 && c_sum[i] != 0 && d_sum[i]){
        #      row_value = paste(a_sum[i], "," ,b_sum[i], "," ,c_sum[i], "," ,d_sum[i])

        # fisher
        data_fisher <- matrix(c(a_sum[i],b_sum[i],c_sum[i],d_sum[i]),
                              nrow=2,
                              byrow=T,
                              dimnames=list(c("B_0","B_1"), c("A_0","A_1")))
        if(two_side){
          res_fisher <- fisher.test(data_fisher)
        }else{
          res_fisher_less <- fisher.test(data_fisher, alternative="l")
          res_fisher_greater <- fisher.test(data_fisher, alternative="g")
          if(res_fisher_less$p.value > res_fisher_greater$p.value){
            res_fisher <- res_fisher_greater
          }else{
            res_fisher <- res_fisher_less
          }
        }

        score <- getScore("bic", a_sum[i], b_sum[i], c_sum[i], d_sum[i])
        pre_score <- score[[1]]
        pro_score <- score[[2]]
        if(plot_index == 0){
          data_fisher_list <- list(data_fisher)
          x <- c(pre_score - pro_score)
          y <- c(res_fisher$p.value)
        }else{
          data_fisher_list <- c(data_fisher_list, list(data_fisher))
          x <- append(x, pre_score - pro_score)
          y <- append(y, res_fisher$p.value)
        }
        plot_index <- plot_index + 1


        if((pre_score > pro_score) && res_fisher$p.value > 0.05){
          independence_warseScore <- independence_warseScore + 1
        }else if((pre_score <= pro_score) && res_fisher$p.value > 0.05){
          independence_betterScore <- independence_betterScore + 1
        }else if((pre_score > pro_score) && (res_fisher$p.value <= 0.05)){ #
          dependence_warseScore <- dependence_warseScore + 1
        }else{                                                        #
          dependence_betterScore <- dependence_betterScore + 1
        }
      }
    }
    total <- independence_betterScore +
             independence_warseScore +
             dependence_betterScore +
             dependence_warseScore
    if(data_index == 0){
      independence_betterScore_vec <- c(100*independence_betterScore/total)
      independence_warseScore_vec <- c(100*independence_warseScore/total)
      dependence_betterScore_vec <- c(100*dependence_betterScore/total)
      dependence_warseScore_vec <- c(100*dependence_warseScore/total)
    }else{
      independence_betterScore_vec <- append(independence_betterScore_vec, 100*independence_betterScore/total)
      independence_warseScore_vec <- append(independence_warseScore_vec, 100*independence_warseScore/total)
      dependence_betterScore_vec <- append(dependence_betterScore_vec, 100*dependence_betterScore/total)
      dependence_warseScore_vec <- append(dependence_warseScore_vec, 100*dependence_warseScore/total)
    }
    data_index <- data_index + 1
    plotResult(x,y, p_value,"BIC score diff", "p value",data_size_num)
    print(paste("Data Size : ", data_size_num))
    printResultbyTable(independence_betterScore, independence_warseScore, dependence_betterScore,dependence_warseScore)
    printResultbyLine(independence_betterScore, independence_warseScore, dependence_betterScore,dependence_warseScore)
    print(proc.time() - ptm)
  }
  plotData(data_size_num_vec, independence_betterScore_vec, independence_warseScore_vec, dependence_betterScore_vec, dependence_warseScore_vec)
  data_csv_matrix <- getCsvData(data_size_num_vec, independence_betterScore_vec, independence_warseScore_vec, dependence_betterScore_vec, dependence_warseScore_vec)
  write_csv(data_csv_matrix, "data_csv", quote=FALSE)
}

# Program Start
#test_data <- c(10,20,30,40,50,60,70,80,90,100)
test_data <- seq(10,50, by=5)
main(data_size_num = test_data, p_value= 0.05, two_side = TRUE)
