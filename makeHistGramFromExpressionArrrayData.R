library(ggplot2)

makeHistGram <- function(data, max.data){
  data = data[data$samples <= max.data, ]
  g = ggplot(data=data,aes(data$samples)) +
      geom_histogram(breaks=seq(0, max.data, by=5),
      col = "black",
      fill = "black",
      alpha = .2) +
      labs(title="") +
      labs(x="", y="")
  g = g + theme_bw()
  g = g + theme(axis.text = element_text(size=14))
  plot(g)
}

main <- function(){
  setwd("/Users/St_Hakky/Google ドライブ/work/num_samples")
  
  # gds_result_ExpArray_till20160218
  gds_result = read.csv("gds_result_ExpArray_till20160218.csv", header = TRUE)
  makeHistGram(gds_result, 100)

  # gds_result_human_ExpressionArray
  gds_result_human = read.csv("gds_result_human_ExpressionArray.csv", header = TRUE)
  makeHistGram(gds_result_human, 100)
  
  # gds_result_human_ExpArray_20160101-20160218
  gds_result_human_20160101_20160218 = read.csv("gds_result_human_ExpArray_20160101-20160218.csv", header = TRUE)
  makeHistGram(gds_result_human_20160101_20160218, 100)
  
}

main()



