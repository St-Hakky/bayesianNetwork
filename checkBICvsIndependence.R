library()





setAllPaternDataInBoolean <- function(sum.val=10, min.val=0){
  index = 0;
  for(i in min.val:sum.val){
    for(j in min.val:sum.val){
      for(k in min.val:sum.val){
        for(l in min.val:sum.val){
          if(i + j + k + l == sum.val){
            if(index == 0){
              a.0 = c(i)
              a.1 = c(j)
              b.0 = c(k)
              b.1 = c(l)
            }else{
              a.0 = append(a.0,i)
              a.1 = append(a.1,j)
              b.0 = append(b.0,k)
              b.1 = append(b.1,l)
            }
            index = index + 1
          }  
        }
      }
    }
  }
  return (list(a.0,a.1,b.0,b.1))
}

  
temp = setAllPaternDataInBoolean()
print(temp)
