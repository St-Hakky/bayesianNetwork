library(ggplot2)
library(dplyr)

makeHistGram <- function(data, max.data) {
  print(count(data, samples > 100))
  print(paste(
    "parcentage of the samples less than",
    max.data,
    ":",
    100 * count(data, samples > 100)[1, 2] / (count(data, samples > 100)[1, 2] + count(data, samples >
                                                                                         100)[2, 2])
  ))
  print(paste(
    "parcentage of the samples larger than",
    max.data,
    ":",
    100 * count(data, samples > 100)[2, 2] / (count(data, samples > 100)[1, 2] + count(data, samples >
                                                                                         100)[2, 2])
  ))
  data <- data[data$samples <= max.data, ]
  g <- ggplot(data = data, aes(data$samples)) +
    geom_histogram(
      breaks = seq(0, max.data, by = 5),
      col = "black",
      fill = "black",
      alpha = .2
    ) +
    labs(title = "") +
    labs(x = "", y = "")
  g <- g + theme_bw()
  g <- g + theme(axis.text = element_text(size = 14))
  plot(g)
}

main <- function() {
  # gds_result
  file_path <-
    file.path(
      getwd(),
      "data",
      "geo_samples",
      "processed",
      "gds_result_ExpArray_till20160218.csv"
    )
  gds_result <- read.csv(file_path, header = TRUE)
  makeHistGram(gds_result, 100)
  if (TRUE) {
    # gds_result_ExpArray_till20160218
    file_path <-
      file.path(
        getwd(),
        "data",
        "geo_samples",
        "processed",
        "gds_result_ExpArray_till20160218.csv"
      )
    gds_result <- read.csv(file_path, header = TRUE)
    makeHistGram(gds_result, 100)
    
    # gds_result_human_ExpressionArray
    file_path <-
      file.path(
        getwd(),
        "data",
        "geo_samples",
        "processed",
        "gds_result_human_ExpressionArray.csv"
      )
    gds_result_human <- read.csv(file_path, header = TRUE)
    makeHistGram(gds_result_human, 100)
    
    # gds_result_human_ExpArray_20160101-20160218
    file_path <-
      file.path(
        getwd(),
        "data",
        "geo_samples",
        "processed",
        "gds_result_human_ExpArray_20160101-20160218.csv"
      )
    gds_result_human_20160101_20160218 <- read.csv(file_path,
                                                   header = TRUE)
    makeHistGram(gds_result_human_20160101_20160218, 100)
  }
}

main()
