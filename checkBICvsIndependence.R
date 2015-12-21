library(bnlearn)


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
    if(pro.score.fromAtoB < pro.score.fromBtoA){
      pro.score = pro.score.fromBtoA
    }else{
      pro.score = pro.score.fromAtoB
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
  
main <- function(){
  plot.index = 0
  table.value = setAllPaternDataInBoolean()
#  print(table.value)
  a.sum = table.value[[1]]
  b.sum = table.value[[2]]
  c.sum = table.value[[3]]
  d.sum = table.value[[4]]
  tp.val = 0
  tn.val = 0
  fp.val = 0
  fn.val = 0
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
        x = c(pro.score - pre.score)
        y = c(res.fisher$p.value)
      }else{
        x = append(x, pro.score - pre.score)
        y = append(y, res.fisher$p.value)
      }
      plot.index = plot.index + 1
      
      
#      if(pre.score < pro.score && res.fisher$p.value > 0.05){        # TP
#        tp.val = tp.val + 1
#      }else if(pre.score >= pro.score && res.fisher$p.value > 0.05){ # FP
#        fp.val = fp.val + 1 
#      }else if(pre.score < pro.score && res.fisher$p.value <= 0.05){ # FN
#        fn.val = fn.val + 1
#      }else{                                                        # TN
#        tn.val = tn.val + 1
#      }
#      print(paste("p-value : ", res.fisher$p.value))
#      print(paste("There is not edge from A to B : ",pre.score))
#      print(paste("There is an edge from A to B : ",pro.score))
    }
  }
plot(x,y,xlab="BIC score diff", ylab="p-value")
par(new=T)
abline(h = 0.5, col="blue")
#  print (paste("independence is true and BIC score is increase : ", tp.val))
#  print (paste("independence is true and BIC score is decrease : ", fp.val))
#  print (paste("independence is false and BIC score is increase : ", fn.val))
#  print (paste("independence is false and BIC score is decrease : ", tn.val))
}

main()


