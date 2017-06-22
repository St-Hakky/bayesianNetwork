library(bnlearn)
library(dplyr)

printResultbyLine <- function(independence.betterScore, independence.warseScore, dependence.betterScore, dependence.warseScore){
  total = independence.betterScore + independence.warseScore + dependence.betterScore + dependence.warseScore
  print (paste("independence is true and BIC score is better : ", independence.betterScore, "(", 100*independence.betterScore/total," parcent)"))
  print (paste("independence is true and BIC score is warse : ", independence.warseScore, "(", 100*independence.warseScore/total," parcent)"))
  print (paste("independence is false and BIC score is better : ", dependence.betterScore, "(", 100*dependence.betterScore/total," parcent)"))
  print (paste("independence is false and BIC score is warse : ", dependence.warseScore, "(", 100*dependence.warseScore/total," parcent)"))
}

printResultbyTable <- function(independence.betterScore,independence.warseScore,dependence.betterScore,dependence.warseScore,colnames = c("independence", "dependence"),rownames = c("better score", "warse score")){
  total = independence.betterScore + independence.warseScore + dependence.betterScore + dependence.warseScore
  result.table = matrix(0,2,2)
  result.table[1,] = c(independence.betterScore, independence.warseScore)
  result.table[2,] = c(dependence.betterScore, dependence.warseScore)
  colnames(result.table) = colnames
  rownames(result.table) = rownames
  print (result.table)
  result.table[1,] = c(100*independence.betterScore/total, 100*independence.warseScore/total)
  result.table[2,] = c(100*dependence.betterScore/total, 100*dependence.warseScore/total)
  print (result.table)
}

plotResult <- function(x, y, p.value, xlab, ylab,title.main,title.sub, p.line.col="blue", BIC.eq.col="red"){
  plot(x,y,xlab=xlab, ylab=ylab, ylim=c(0,1))
  par(new=T)
  title(title.main)
  abline(h = p.value/2, col=p.line.col)
  abline(h = 1-p.value/2, col=p.line.col)
  abline(v = 0, col=BIC.eq.col)
  par(new=F)
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
    
    # the smaller score is better
    if(pro.score.fromAtoB < pro.score.fromBtoA){
      pro.score = pro.score.fromAtoB
    }else{
      pro.score = pro.score.fromBtoA
    }
    
  }
  return (list(pre.score, pro.score))
}


plotData <- function(x,y,xlab,ylab,col=col, title, save="false"){
  plot(x, y, xlab=xlab, ylab=ylab, col=col, xlim=c(0,140), ylim=c(0,100), type="b")
  par(new=T)
  title(title)
  par(new=T)
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
    random.val = getRandomValue(asia, length(data[,1]), sample.data.size-4)
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

main <- function(data, sample.data.size.vec = c(10), try.size = 20){
  data.names = names(data)
  data.length = length(data[, data.names[1]])
  independence.betterScore.list = list()
  independence.warseScore.list = list()
  dependence.betterScore.list = list()
  dependence.warseScore.list = list()
  sample.data.index = 1
  for(sample.data.size in sample.data.size.vec){
    print(paste("data size : ", sample.data.size))
    sample.data = getSampleData(try.size, data,data.names,sample.data.size)
    sample.table.data = sample.data[[1]]
    sample.row.data = sample.data[[2]]
    for(i in 1:length(sample.row.data)){
      independence.betterScore = 0
      independence.warseScore = 0
      dependence.betterScore = 0
      dependence.warseScore = 0
      plot.index = 0
      print(paste("i : ", i))
      for(j in 1:length(sample.row.data[[i]])){
#        print(paste("i : ", i, "j : ", j))
        score = getScore(sample.row.data[[i]][[j]])
        pre.score = score[[1]]
        pro.score = score[[2]]
        res.fisher = fisher.test(sample.table.data[[i]][[j]])
        if(plot.index == 0){
          x = c(pre.score - pro.score)
          y = c(res.fisher$p.value)
        }else{
          x = append(x, pre.score - pro.score)
          y = append(y, res.fisher$p.value)
        }
        plot.index = plot.index + 1
        
        if((pre.score - pro.score < 0) && res.fisher$p.value > 0.025 && res.fisher$p.value < 0.975){        # 
          independence.warseScore = independence.warseScore + 1
        }else if((pre.score - pro.score >= 0) && res.fisher$p.value > 0.025  && res.fisher$p.value < 0.975){ # 
          independence.betterScore = independence.betterScore + 1 
        }else if((pre.score - pro.score < 0) && (res.fisher$p.value <= 0.025  || res.fisher$p.value >= 0.975)){ # 
          dependence.warseScore = dependence.warseScore + 1
        }else{                                                        # 
          dependence.betterScore = dependence.betterScore + 1
        }
        
      }
      
      if(sample.data.index == 1){
        independence.betterScore.list = c(independence.betterScore.list, list(list()))
        independence.warseScore.list = c(independence.warseScore.list, list(list()))
        dependence.betterScore.list = c(dependence.betterScore.list, list(list()))
        dependence.warseScore.list = c(dependence.warseScore.list, list(list()))
        independence.betterScore.list[[i]] = c(independence.betterScore.list[[i]], list(100*independence.betterScore/try.size))
        independence.warseScore.list[[i]] = c(independence.warseScore.list[[i]], list(100*independence.warseScore/try.size))
        dependence.betterScore.list[[i]] = c(dependence.betterScore.list[[i]], list(100*dependence.betterScore/try.size))
        dependence.warseScore.list[[i]] = c(dependence.warseScore.list[[i]], list(100*dependence.warseScore/try.size))
      }else{
        independence.betterScore.list[[i]] = c(independence.betterScore.list[[i]], list(100*independence.betterScore/try.size))
        independence.warseScore.list[[i]] = c(independence.warseScore.list[[i]], list(100*independence.warseScore/try.size))
        dependence.betterScore.list[[i]] = c(dependence.betterScore.list[[i]], list(100*dependence.betterScore/try.size))
        dependence.warseScore.list[[i]] = c(dependence.warseScore.list[[i]], list(100*dependence.warseScore/try.size))
      }
      printResultbyTable(independence.betterScore, independence.warseScore, dependence.betterScore,dependence.warseScore)
      printResultbyLine(independence.betterScore, independence.warseScore, dependence.betterScore,dependence.warseScore)
      plotResult(x,y, 0.05,"BIC score diff", "p-value",paste("try size : ", try.size, "Data Size : ", sample.data.size), paste("set : ", i))
    }
    sample.data.index = sample.data.index + 1
  }
  for(s in 1:28){
    print(independence.betterScore.list[[s]])
    for(t in 1:length(independence.betterScore.list[[s]])){
      if(t == 1){
        independence.betterScore.vec = c(independence.betterScore.list[[s]][[t]])
        independence.warseScore.vec  = c(independence.warseScore.list[[s]][[t]])
        dependence.betterScore.vec   = c(dependence.betterScore.list[[s]][[t]])
        dependence.warseScore.vec    = c(dependence.warseScore.list[[s]][[t]])
      }else{
        independence.betterScore.vec = append(independence.betterScore.vec, independence.betterScore.list[[s]][[t]])
        independence.warseScore.vec  = append(independence.warseScore.vec, independence.warseScore.list[[s]][[t]])
        dependence.betterScore.vec   = append(dependence.betterScore.vec, dependence.betterScore.list[[s]][[t]])
        dependence.warseScore.vec    = append(dependence.warseScore.vec, dependence.warseScore.list[[s]][[t]])
      }
    }
    plotData(sample.data.size.vec, independence.betterScore.vec, "", "","red", "", "start")
    plotData(sample.data.size.vec, independence.warseScore.vec, "", "", "blue", "")
    plotData(sample.data.size.vec, dependence.betterScore.vec, "", "", "black", "")
    plotData(sample.data.size.vec, dependence.warseScore.vec, "Data Size", "parcentage", "yellow", paste("patern : ", s), "end")
    par(new=F)
  }
}

data(asia)
main(asia, c(10,20,30,40,50,60,70), 50)

