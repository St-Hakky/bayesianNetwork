library(bnlearn)

getSampleData <- function(data, data.length, random.length){
  random.index = sort(floor(runif(random.length) * data.length))
  print(length(random.index))
  print(random.length)
  random.val = asia[random.index,]
#  rownames(random.val) = c(1:random.length)
  return(random.val)
}

plotResult <- function(x, y, xlab, ylab){
  plot(x,y,xlab=xlab, ylab=ylab, type="b")
}


main <- function(sample.size.vec = c(10), try.size = 50){
  # correct graph data
  
  res.correct = empty.graph(names(asia))
  modelstring(res.correct) = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
  plot(res.correct)
  
  index = 0
  for(sample.size in sample.size.vec){
    tp.sum = 0
    fp.sum = 0
    fn.sum = 0
    print(paste("sample.size : ", sample.size))
    for(i in 1:try.size){
      sample.data = getSampleData(asia, length(asia[,1]), sample.size)
      res.learn = hc(sample.data)
      plot(res.learn)
      diff.table = compare(target=res.learn, current=res.correct, arcs=TRUE)
      tp.sum = tp.sum + length(diff.table$tp[,1])
      fp.sum = fp.sum + length(diff.table$fp[,1])
      fn.sum = fn.sum + length(diff.table$fn[,1])
    }
    tp.parcent = tp.sum / try.size
    fp.parcent = fp.sum / try.size
    fn.parcent = fn.sum / try.size
    print(paste("the tp's parcent is ", tp.parcent))
    print(paste("the fp's parcent is ", fp.parcent))
    print(paste("the fn's parcent is ", fn.parcent))
    if(index == 0){
      tp.parcent.vec = c(tp.parcent)
      fp.parcent.vec = c(fp.parcent)
      fn.parcent.vec = c(fn.parcent)
    }else{
      tp.parcent.vec = append(tp.parcent.vec, tp.parcent)
      fp.parcent.vec = append(fp.parcent.vec, fp.parcent)
      fn.parcent.vec = append(fn.parcent.vec, fn.parcent)
    }
    index = index + 1
  }
  print(paste("tp.parcent.vec : ", tp.parcent.vec))
  print(paste("fp.parcent.vec : ", fp.parcent.vec))
  print(paste("fn.parcent.vec : ", fn.parcent.vec))
  plotResult(sample.size.vec, tp.parcent.vec, "data size", "tp")
  plotResult(sample.size.vec, fp.parcent.vec, "data size", "fp")
  plotResult(sample.size.vec, fn.parcent.vec, "data size", "fn")
}


data(asia)
sample.size.vec = c(10,50,100,500,1000,2000,3000,4000,5000)
main(sample.size.vec = sample.size.vec, try.size = 10)
#main()