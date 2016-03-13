library(bnlearn)

getDataFrame <- function(data.A, data.B){
  data = data.frame(A=data.A, B=data.B)
  return(data)
}

plotResult <- function(x, y, p.value, xlab, ylab,title.main,p.line.col="blue", BIC.eq.col="red"){
  plot(x,y,xlab=xlab, ylab=ylab,ylim=c(0,1))
  par(new=T)
  title(title.main)
  abline(h = p.value, col=p.line.col)
  abline(v = 0, col=BIC.eq.col)
  par(new=F)
}

getSampleData <- function(data, data.length, random.length){
  random.index = sort(floor(runif(random.length) * data.length))
  random.val = data[random.index,]
  return(random.val)
}

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
  result.table[1,] = c(independence.betterScore, dependence.betterScore)
  result.table[2,] = c(independence.warseScore, dependence.warseScore)
  colnames(result.table) = colnames
  rownames(result.table) = rownames
  print (result.table)
  result.table[1,] = c(100*independence.betterScore/total, 100*dependence.betterScore/total)
  result.table[2,] = c(100*independence.warseScore/total, 100*dependence.warseScore/total)
  print (result.table)
}

plotData <- function(x,y,xlab,ylab,col=col){
  plot(x, y, xlab=xlab, ylab=ylab, col=col, xlim=c(0,max(x)), ylim=c(0,100), type="b")
  par(new=T)
}

getCsvData <- function(data.size.num.vec, independence.betterScore.vec, independence.warseScore.vec, dependence.betterScore.vec, dependence.warseScore.vec){
  data.vec = c(independence.betterScore.vec, independence.warseScore.vec, dependence.betterScore.vec, dependence.warseScore.vec)
  print(data.vec)
  data.csv.matrix = matrix(data.vec, nrow=length(data.vec)/4, ncol=4)
  print(data.csv.matrix)
  colnames(data.csv.matrix) = c("fp.independence.betterScore.vec", "fp.independence.warseScore.vec", "fp.dependence.betterScore.vec", "fp.dependence.warseScore.vec")
  rownames(data.csv.matrix) = data.size.num.vec
  return(data.csv.matrix)
}


getScore <- function(type, data){
  if(type == "bic" || type == "aic"){
    # dataFrame
    data = getDataFrame(data[,1], data[,2])
    data$A = as.factor(data$A)
    data$B = as.factor(data$B)
    
    # pre.score 
    res = empty.graph(names(data))
    pre.score = -2 * score(res, data, type=type)
    
    # pro.score from A to B
    res = set.arc(res,"A","B")
    pro.score = -2 * score(res, data, type=type)
  }
  return (list(pre.score, pro.score))
}

getTableData <- function(data, val.0 = "no", val.1 = "yes"){
  data1 = data[,1]
  data2 = data[,2]
  data.length = length(data[,1])
  a.sum = 0
  b.sum = 0
  c.sum = 0
  d.sum = 0
  for(i in 1:data.length){
    if(data1[i] == val.1 && data2[i] == val.0){
      a.sum = a.sum + 1
    }else if(data1[i] == val.0 && data2[i] == val.0){
      b.sum = b.sum + 1
    }else if(data1[i] == val.1 && data2[i] == val.1){
      c.sum = c.sum + 1
    }else{
      d.sum = d.sum + 1
    }
  }
  data.table = matrix(c(a.sum, b.sum, c.sum, d.sum),nrow=2, byrow=T, dimnames=list(c("B.0","B.1"),c("A.1","A.0")))
  #print(data.table)
  return (data.table)
}

compareBICandFisher <- function(data, data.size.vec, try.size, bn.graph.correct, data.name, skeleton, two.side=FALSE){
  data.index = 0
  for(data.size in data.size.vec){
    plot.index = 0
    
    # fp
    fp.independence.betterScore = 0
    fp.independence.warseScore = 0
    fp.dependence.betterScore = 0
    fp.dependence.warseScore = 0

    print(paste("data.size : ", data.size))
    for(i in 1:try.size){
      sample.data = getSampleData(data, length(data[,1]), data.size)
      bn.graph.learn = hc(sample.data)
      #graphviz.plot(bn.graph.learn)
      if(skeleton){
        bn.graph.learn = skeleton(bn.graph.learn)
        #graphviz.plot(bn.graph.learn)
      }
      
      # get tp, fp, fn arcs
      diff.table = compare(target=bn.graph.correct, current=bn.graph.learn, arcs=TRUE)
      #print("start : fp")
      if(length(diff.table$fp[,1]) != 0){
        #print(diff.table$fp)
        for(i in 1:length(diff.table$fp[,1])){
          fp.data = sample.data[diff.table$fp[i,]]
          score = getScore("bic",fp.data)
          pre.score = score[[1]]
          pro.score = score[[2]]
          
          # fisher
          data.fisher = getTableData(fp.data)
          if(two.side){
            res.fisher = fisher.test(data.fisher)
          }else{
            res.fisher.less = fisher.test(data.fisher,alternative="l")
            res.fisher.greater = fisher.test(data.fisher, alternative="g")
            if(res.fisher.less$p.value > res.fisher.greater$p.value){
              res.fisher = res.fisher.greater
            }else{
              res.fisher = res.fisher.less
            }
          }
          #print(paste("res.fisher.less.p.value : ", res.fisher.less$p.value))
          #print(paste("res.fisher.greater.p.value : ", res.fisher.greater$p.value))
          #print(paste("res.fisher : ", res.fisher$p.value))
          
          if(plot.index == 0){
            x = c(pre.score - pro.score)
            y = c(res.fisher$p.value)
          }else{
            x = append(x, pre.score - pro.score)
            y = append(y, res.fisher$p.value)
          }
          plot.index = plot.index + 1
          
          if((pre.score - pro.score < 0) && res.fisher$p.value > 0.05){        # 
            fp.independence.warseScore = fp.independence.warseScore + 1
            #print(paste("fp.independence.warseScore : ",diff.table$fp[i,]))
          }else if((pre.score - pro.score >= 0) && res.fisher$p.value > 0.05){ # 
            fp.independence.betterScore = fp.independence.betterScore + 1 
            #print(paste("fp.independence.betterScore : ", diff.table$fp[i,]))
          }else if((pre.score - pro.score < 0) && (res.fisher$p.value <= 0.05)){ # 
            fp.dependence.warseScore = fp.dependence.warseScore + 1
            #print(paste("fp.dependence.warseScore : ", diff.table$fp[i,]))
          }else{                                                        # 
            fp.dependence.betterScore = fp.dependence.betterScore + 1
            #print(paste("fp.dependence.betterScore : ", diff.table$fp[i,]))
          }
        }
      }
    }
    
    total = fp.independence.betterScore + fp.independence.warseScore + fp.dependence.betterScore + fp.dependence.warseScore
    if(data.index == 0){
      fp.independence.betterScore.vec = c(100*fp.independence.betterScore/total)
      fp.independence.warseScore.vec = c(100*fp.independence.warseScore/total)
      fp.dependence.betterScore.vec = c(100*fp.dependence.betterScore/total)
      fp.dependence.warseScore.vec = c(100*fp.dependence.warseScore/total)
    }else{
      fp.independence.betterScore.vec = append(fp.independence.betterScore.vec, 100*fp.independence.betterScore/total)
      fp.independence.warseScore.vec = append(fp.independence.warseScore.vec, 100*fp.independence.warseScore/total)
      fp.dependence.betterScore.vec = append(fp.dependence.betterScore.vec, 100*fp.dependence.betterScore/total)
      fp.dependence.warseScore.vec = append(fp.dependence.warseScore.vec, 100*fp.dependence.warseScore/total)
    }
    data.index = data.index + 1
    png(paste("FP two side ", two.side ," Data Size ", data.size ,".png"))
    if(two.side){
      plotResult(x,y, 0.05,"BIC score diff", "p-value",paste("fp two.side Data size : ", data.size))
    }else{
      plotResult(x,y, 0.05,"BIC score diff", "p-value",paste("fp one.side Data size : ", data.size))
    }
    dev.off()
    print(paste("Data Size : ", data.size))
    printResultbyTable(fp.independence.betterScore, fp.independence.warseScore, fp.dependence.betterScore, fp.dependence.warseScore)
    printResultbyLine(fp.independence.betterScore, fp.independence.warseScore, fp.dependence.betterScore,fp.dependence.warseScore)
  }
  png("all.png")
  plotData(data.size.vec, fp.independence.betterScore.vec, "Data Size", "independence and better Score","red")
  plotData(data.size.vec, fp.independence.warseScore.vec, "Data Size", "independence and warse Score", "blue")
  plotData(data.size.vec, fp.dependence.betterScore.vec, "Data Size", "dependence and better Score", "black")
  plotData(data.size.vec, fp.dependence.warseScore.vec, "Data Size", "dependence and warseScore", "green")
  par(new=F)
  dev.off()
  data.csv.matrix = getCsvData(data.size.vec, fp.independence.betterScore.vec, fp.independence.warseScore.vec, fp.dependence.betterScore.vec, fp.dependence.warseScore.vec)
  print(data.csv.matrix)
  write.csv(data.csv.matrix, "data.csv", quote=FALSE)
  
  #return (list(tp.parcent.vec, fp.parcent.vec, fn.parcent.vec))
}

checkData <- function(data, data.size.vec, try.size, correct.graph.string, data.name, skeleton = FALSE, two.side=FALSE){
  print(data.name)
  # correct graph data
  res.correct = empty.graph(names(data))
  modelstring(res.correct) = correct.graph.string
  if(skeleton){
    res.correct = skeleton(res.correct)
  }
  graphviz.plot(res.correct)
  
  result = compareBICandFisher(data, data.size.vec, try.size, res.correct, data.name, skeleton, two.side)
  return(result)
}

main <- function(try.size=50, two.side=FALSE){
  data(asia)
  #asia.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
  asia.size.vec = seq(10, 500, by=5)
  asia.correct.graph.string = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
  asia.result.directed = checkData(asia, asia.size.vec, try.size, asia.correct.graph.string, "asia", skeleton="FALSE",two.side)
  #asia.result.undirected = checkData(asia, asia.size.vec, try.size, asia.correct.graph.string, "asia", skeleton="TRUE",two.side)
}

main(two.side=TRUE)

