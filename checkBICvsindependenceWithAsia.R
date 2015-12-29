library(bnlearn)
library(dplyr)

printResultbyLine <- function(tp,fp,fn,tn){
  print (paste("independence is true and BIC score is increase : ", tp, "(", 100*tp/(tp + fp + fn + tn)," parcent)"))
  print (paste("independence is true and BIC score is decrease : ", fp, "(", 100*fp/(tp + fp + fn + tn)," parcent)"))
  print (paste("independence is false and BIC score is increase : ", fn, "(", 100*fn/(tp + fp + fn + tn)," parcent)"))
  print (paste("independence is false and BIC score is decrease : ", tn, "(", 100*tn/(tp + fp + fn + tn)," parcent)"))
}

plotResult <- function(x, y, p.value, xlab, ylab,title.main,p.line.col="blue", BIC.eq.col="red"){
  plot(x,y,xlab=xlab, ylab=ylab, ylim=c(0,1))
  par(new=T)
  title(title.main)
  abline(h = p.value, col=p.line.col)
  abline(v = 0, col=BIC.eq.col)
  par(new=F)
}


printResultbyTable <- function(tp,fp,fn,tn,colnames = c("independence", "dependence"),rownames = c("increase", "decrease")){
  result.table = matrix(0,2,2)
  result.table[1,] = c(tp, fp)
  result.table[2,] = c(fn, tn)
  colnames(result.table) = colnames
  rownames(result.table) = rownames
  print (result.table)
  result.table[1,] = c(100*tp/(tp + fp + fn + tn), 100*fp/(tp + fp + fn + tn))
  result.table[2,] = c(100*fn/(tp + fp + fn + tn), 100*tn/(tp + fp + fn + tn))
  print (result.table)
}


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

getSampleData <- function(try.size, data, data.names,sample.data.size){
  sample.table.data = list()
  sample.row.data = list()
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
  return(list(sample.table.data, sample.row.data))  
}

getDataFrame <- function(data){
  a = data[,1]
  b = data[,2]
  data = data.frame(A=a, B=b)
  return(data)
}

getScore <- function(data, type="bic"){
  data = getDataFrame(data)
  data[,1] = as.factor(data[,1])
  data[,2] = as.factor(data[,2])
  
  # pre.score
  res = empty.graph(colnames(data))
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
  return (list(pre.score, pro.score))
  
}

main <- function(data, sample.data.size.vec = c(10), try.size = 10){
  data.names = names(data)
  data.length = length(data[, data.names[1]])
  independence.inc = list()
  independence.dec = list()
  dependence.inc = list()
  dependence.dec = list()
  sample.data.index = 1
  for(sample.data.size in sample.data.size.vec){
    sample.data = getSampleData(try.size, data,data.names,sample.data.size)
    sample.table.data = sample.data[[1]]
    sample.row.data = sample.data[[2]]
    for(i in 1:length(sample.row.data)){
      tp.val = 0
      tn.val = 0
      fp.val = 0
      fn.val = 0
      plot.index = 0
      for(j in 1:length(sample.row.data[[i]])){
        print(paste("i : ", i, "j : ", j))
        score = getScore(sample.row.data[[i]][[j]])
        pre.score = score[[1]]
        pro.score = score[[2]]
        res.fisher = fisher.test(sample.table.data[[i]][[j]])
        if(plot.index == 0){
          x = c(pro.score - pre.score)
          y = c(res.fisher$p.value)
        }else{
          x = append(x, pro.score - pre.score)
          y = append(y, res.fisher$p.value)
        }
        plot.index = plot.index + 1
        
        if(pre.score < pro.score && res.fisher$p.value > 0.05){        # TP
          tp.val = tp.val + 1
        }else if(pre.score >= pro.score && res.fisher$p.value > 0.05){ # FP
          fp.val = fp.val + 1 
        }else if(pre.score < pro.score && res.fisher$p.value <= 0.05){ # FN
          fn.val = fn.val + 1
        }else{                                                        # TN
          tn.val = tn.val + 1
        }
      }
      if(sample.data.index == 1){
        independence.inc = c(independence.inc, list(list()))
        independence.dec = c(independence.dec, list(list()))
        dependence.inc = c(dependence.inc, list(list()))
        dependence.dec = c(dependence.dec, list(list()))
        independence.inc[[i]] = c(independence.inc[[i]], list(tp.val/try.size))
        independence.dec[[i]] = c(independence.dec[[i]], list(fp.val/try.size))
        dependence.inc[[i]] = c(dependence.inc[[i]], list(fn.val/try.size))
        dependence.dec[[i]] = c(dependence.dec[[i]], list(tn.val/try.size))
      }else{
        independence.inc[[i]] = c(independence.inc[[i]], list(tp.val/try.size))
        independence.dec[[i]] = c(independence.dec[[i]], list(fp.val/try.size))
        dependence.inc[[i]] = c(dependence.inc[[i]], list(fn.val/try.size))
        dependence.dec[[i]] = c(dependence.dec[[i]], list(tn.val/try.size))
      }
      printResultbyTable(tp.val, fp.val, fn.val,tn.val)
      printResultbyLine(tp.val, fp.val, fn.val,tn.val)
      plotResult(x,y, 0.05,"BIC score diff", "p-value",paste("try size : ", try.size))
    }
    sample.data.index = sample.data.index + 1
  }
  
}

data(asia)
main(asia, c(10,20))

