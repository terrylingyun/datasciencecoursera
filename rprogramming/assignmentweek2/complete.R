complete <- function(directory, id = 1:332) {
  nodstotal <- c()
  idnumber <- c()
  for(i in id) {
    if(i < 10) {
      i <- paste("00", i, sep = "")
    } else if(i < 100) {
      i <- paste("0", i, sep = "")
    }
    csv <- paste(directory, "/", i, ".csv", sep = "")
    newdata <- read.csv(csv)
    good <- complete.cases(newdata)
    data <- newdata[good,]
    nods <- nrow(data)
    nodstotal <- c(nodstotal, nods)
  }
  count <- data.frame(id, nodstotal)
  colnames(count) <- c("id", "nobs")
  count
}