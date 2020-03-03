corr <- function(directory, threshold = 0) {
  temp <- list.files(directory)
  cortotal <- numeric()
  for(i in temp) {
    csv <- paste(directory, "/", i, sep = "")
    newdata <- read.csv(csv)
    good <- complete.cases(newdata)
    data <- newdata[good,]
    if(nrow(data) >= threshold) {
      correlation <- cor(data["sulfate"], data["nitrate"])
      cortotal <- c(cortotal, correlation)
    }
  }
    cortotal
}