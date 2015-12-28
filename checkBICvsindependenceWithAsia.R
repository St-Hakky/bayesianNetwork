library(bnlearn)
library(dplyr)

getScore <- function(type, a.sum.i, b.sum.i,c.sum.i,d.sum.i){
  if(type == "bic" || type == "aic"){
    # dataFrame
    data = getDataFrame(a.sum.i,b.sum.i,c.sum.i,d.sum.i)
    data$A = as.factor(data$A)
    data$B = as.factor(data$B)
    
    # pre.score 
    res = empty.graph(names(data))
    pre.score = -2 * score(res, data, type=type)
    
    # pro.score from A to B
    res = set.arc(res,"A","B")
    pro.score.fromAtoB = -2 * score(res, data, type=type)
    
    # pro.score from B to A
    res = empty.graph(names(data))
    res = set.arc(res,"B","A")
    pro.score.fromBtoA = -2 * score(res, data, type=type)
    
    if(pro.score.fromAtoB < pro.score.fromBtoA){
      pro.score = pro.score.fromBtoA
    }else{
      pro.score = pro.score.fromAtoB
    }
  }
  return (list(pre.score, pro.score))
}


getRandomValue <- function(data, data.length, random.length){
  random.index = sort(round(runif(random.length) * data.length))
  random.val = asia[random.index, ]
  rownames(random.val) = c(1:random.length)
  return(random.val)
}

getRowData <- function(data1, data2, data1.name, data2.name){
  data.row = cbind(data1,data2)
  data.row[data.row == 1] = 0
  data.row[data.row == 2] = 1
  add.matrix = cbind(c(0,0,1,1), c(0,1,0,1))
  data.row = rbind(data.row, add.matrix)
  colnames(data.row) = c(data1.name, data2.name)
  return(data.row)
}


getTableData <- function(data1, data2){
  data.length = length(data1)
  a.sum = 1
  b.sum = 1
  c.sum = 1
  d.sum = 1
  for(i in 1:data.length){
    if(data1[i] == "yes" && data2[i] == "no"){
      a.sum = a.sum + 1
    }else if(data1[i] == "no" && data2[i] == "no"){
      b.sum = b.sum + 1
    }else if(data1[i] == "yes" && data2[i] == "yes"){
      c.sum = c.sum + 1
    }else{
      d.sum = d.sum + 1
    }
  }
  data.table = matrix(c(a.sum, b.sum, c.sum, d.sum),nrow=2, byrow=T, dimnames=list(c("B.0","B.1"),c("A.1","A.0")))
  return (data.table)
}

main <- function(data, sample.data.size = 100, try.size = 10){
  data.names = names(data)
  print(paste("data.names : ", data.names))
  sample.table.data = list()
  sample.row.data = list()
  data.length = length(data[, data.names[1]])
  print(paste("data.length : ", data.length))
  for(try.num in 1:try.size){
    random.val = getRandomValue(asia, data.length, sample.data.size-4)
    index = 1
    for(i in 1:length(data)){
      for(j in i:length(data)){
        if(i != j){
          data.table = getTableData(random.val[,i], random.val[,j])
          random.val[,i]
          data.row = getRowData(random.val[,i], random.val[,j], data.names[i], data.names[j])
          if(try.num == 1){
            sample.table.data = c(sample.table.data, list(list()))
            sample.row.data = c(sample.row.data, list(list()))
            #          print(sample.table.data)
            sample.table.data[[index]] = c(sample.table.data[[index]], list(data.table))
            sample.row.data[[index]] = c(sample.row.data[[index]], list(data.row))
          }else{
            sample.table.data[[index]] = c(sample.table.data[[index]], list(data.table))
            sample.row.data[[index]] = c(sample.row.data[[index]], list(data.row))
          }
          index = index + 1
        }
      }
    }
  }
  print(sample.table.data)
  print(sample.row.data)
  
}

data(asia)
main(asia, 200)

