library(bnlearn)

getScoreValue <- function(data, type="bic"){
  
}


getDataFrame <- function(a.sum,b.sum,c.sum,d.sum){
  a = c(rep(0,a.sum), rep(1, b.sum), rep(0,c.sum), rep(1, d.sum))
  b = c(rep(0,a.sum), rep(0, b.sum), rep(1,c.sum), rep(1, d.sum))
  data = data.frame(A=a, B=b)
  return(data)
}

setAllPaternDataInBoolean <- function(sum.val=90, min.val=1){
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
  
main = function(){
  table.value = setAllPaternDataInBoolean()
  print(table.value)
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
      row.value = paste(a.sum[i], "," ,b.sum[i], "," ,c.sum[i], "," ,d.sum[i])
      data.fisher = matrix(c(a.sum[i],b.sum[i],c.sum[i],d.sum[i]),nrow=2, byrow=T, dimnames=list(c("B.0","B.1"),c("A.0","A.1")))
      res.fisher = fisher.test(data.fisher)
      data = getDataFrame(a.sum[i],b.sum[i],c.sum[i],d.sum[i])
      data$A = as.factor(data$A)
      data$B = as.factor(data$B)
      res = empty.graph(names(data))
      pre.score = -2 * score(res, data, type="bic")
      res = set.arc(res,"A","B")
      pro.score = -2 * score(res, data, type="bic")
      if(pre.score < pro.score && res.fisher$p.value > 0.5){      # TP
        tp.val = tp.val + 1
      }else if(pre.score >= pro.score && res.fisher$p.value > 0.5){ # FP
        fp.val = fp.val + 1 
      }else if(pre.score < pro.score && res.fisher$p.value <= 0.5){ # FN
        fn.val = fn.val + 1
      }else{  # TN
        tn.val = tn.val + 1
      }
      print(paste("p-value : ", res.fisher$p.value))
      print(paste("There is not edge from A to B : ",pre.score))
      print(paste("There is an edge from A to B : ",pro.score))
    }
  }
  print (paste("TP : ", tp.val, ", TN : ", tn.val, ", FP : ", fp.val, ", FN : ", fn.val))
}

main()



