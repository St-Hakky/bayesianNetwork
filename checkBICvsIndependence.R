library(bnlearn)


plotData <- function(x,y,xlab,ylab,col=col){
  plot(x, y, xlab="", ylab="", col=col, xlim=c(0,140), ylim=c(0,100), type="b")
  par(new=T)
}

plotResult <- function(x, y, p.value, xlab, ylab,title.main,p.line.col="blue", BIC.eq.col="red"){
  plot(x,y,xlab=xlab, ylab=ylab)
  par(new=T)
  title(title.main)
  abline(h = p.value/2, col=p.line.col)
  abline(h = 1-p.value/2, col=p.line.col)
  abline(v = 0, col=BIC.eq.col)
  par(new=F)
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
  result.table[1,] = c(independence.betterScore, independence.warseScore)
  result.table[2,] = c(dependence.betterScore, dependence.warseScore)
  colnames(result.table) = colnames
  rownames(result.table) = rownames
  print (result.table)
  result.table[1,] = c(100*independence.betterScore/total, 100*independence.warseScore/total)
  result.table[2,] = c(100*dependence.betterScore/total, 100*dependence.warseScore/total)
  print (result.table)
}

getDataFrame <- function(a.sum,b.sum,c.sum,d.sum){
  a = c(rep(0,a.sum), rep(1, b.sum), rep(0,c.sum), rep(1, d.sum))
  b = c(rep(0,a.sum), rep(0, b.sum), rep(1,c.sum), rep(1, d.sum))
  data = data.frame(A=a, B=b)
  return(data)
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


setAllPaternDataInBoolean <- function(sum.val=10, min.val=1){
  index = 0;
  for(i in min.val:sum.val){
    for(j in min.val:sum.val){
      for(k in min.val:sum.val){
        for(l in min.val:sum.val){
          if(i + j + k + l == sum.val){
            if(index == 0){
              a.sum = c(i)
              b.sum = c(j)
              c.sum = c(k)
              d.sum = c(l)
            }else{
              a.sum = append(a.sum,i)
              b.sum = append(b.sum,j)
              c.sum = append(c.sum,k)
              d.sum = append(d.sum,l)
            }
            index = index + 1
          }  
        }
      }
    }
  }
  return (list(a.sum,b.sum,c.sum,d.sum))
}
  
main <- function(data.size.num.vec = c(10), p.value= 0.05){
  data.index = 0
  for(data.size.num in data.size.num.vec){
    ptm = proc.time()
    plot.index = 0
    table.value = setAllPaternDataInBoolean(data.size.num)
    #  print(table.value)
    a.sum = table.value[[1]]
    b.sum = table.value[[2]]
    c.sum = table.value[[3]]
    d.sum = table.value[[4]]
    independence.betterScore = 0
    independence.warseScore = 0
    dependence.betterScore = 0
    dependence.warseScore = 0
    for(i in 1:length(a.sum)){
      
      if(a.sum[i] != 0 && b.sum[i] != 0 && c.sum[i] != 0 && d.sum[i]){
        #      row.value = paste(a.sum[i], "," ,b.sum[i], "," ,c.sum[i], "," ,d.sum[i])
        
        # fisher
        data.fisher = matrix(c(a.sum[i],b.sum[i],c.sum[i],d.sum[i]),nrow=2, byrow=T, dimnames=list(c("B.0","B.1"),c("A.0","A.1")))
        res.fisher = fisher.test(data.fisher)
        
        score = getScore("bic", a.sum[i], b.sum[i], c.sum[i], d.sum[i])
        pre.score = score[[1]]
        pro.score = score[[2]]
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
    }
    if(data.index == 0){
      independence.betterScore.vec = c(independence.betterScore/data.size.num)
      independence.warseScore.vec = c(independence.warseScore/data.size.num)
      dependence.betterScore.vec = c(dependence.betterScore/data.size.num)
      dependence.warseScore.vec = c(dependence.warseScore/data.size.num)
    }else{
      independence.betterScore.vec = append(independence.betterScore.vec, independence.betterScore/data.size.num)
      independence.warseScore.vec = append(independence.warseScore.vec, independence.warseScore/data.size.num)
      dependence.betterScore.vec = append(dependence.betterScore.vec, dependence.betterScore/data.size.num)
      dependence.warseScore.vec = append(dependence.warseScore.vec, dependence.warseScore/data.size.num)
    }
    data.index = data.index + 1
#    plotResult(x,y, p.value,"BIC score diff", "p-value",paste("Data size : ", data.size.num))
#    print(paste("Data Size : ", data.size.num))
#    printResultbyTable(independence.betterScore, independence.warseScore, dependence.betterScore,dependence.warseScore)
#    printResultbyLine(independence.betterScore, independence.warseScore, dependence.betterScore,dependence.warseScore)
    print(proc.time() - ptm)
  }
  plotData(data.size.num.vec, independence.betterScore.vec, "Data Size", "independence and better Score","red")
  plotData(data.size.num.vec, independence.warseScore.vec, "Data Size", "independence and warse Score", "blue")
  plotData(data.size.num.vec, dependence.betterScore.vec, "Data Size", "dependence and better Score", "black")
  plotData(data.size.num.vec, dependence.warseScore.vec, "Data Size", "dependence and warseScore", "yellow")
  par(new=F)
}

# Program Start
test.data = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140)
#main(data.size.num = test.data)
independence.betterScore.vec = c(11.90476, 19.19505, 15.92775, 12.14575, 9.650456, 8.237719, 6.905371, 5.556469, 4.806101, 4.089283, 3.473473, 2.970575, 2.514993, 2.170374)
independence.warseScore.vec  = c(0, 11.14551,23.15271, 32.58562, 39.318281, 44.147774, 48.127648, 51.599034, 54.221408, 56.648114, 58.670820, 60.427509, 62.003868, 63.358212)
dependence.betterScore.vec   = c(28.57143, 43.13725, 42.69294, 41.12047, 39.94789, 38.493956, 36.748483, 35.933687, 34.729316, 33.564766, 32.632161, 31.921087, 31.021676, 30.429075)
dependence.warseScore.vec    = c(59.52381, 26.52219, 18.22660, 14.14816, 11.08337, 9.120551, 8.218498, 6.910811, 6.243176, 5.697837, 5.223546, 4.680829, 4.459463, 4.042339)

plotData(test.data, independence.betterScore.vec, "Data Size", "independence and better Score","red")
plotData(test.data, independence.warseScore.vec, "Data Size", "independence and warse Score", "blue")
plotData(test.data, dependence.betterScore.vec, "Data Size", "dependence and better Score", "black")
plotData(test.data, dependence.warseScore.vec, "Data Size", "dependence and warseScore", "yellow")

