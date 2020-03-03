pollutantmean <- function(directory, pollutant, id = 1:332) {
  totalmean <- 0
  totaldata <- NULL
  for(i in id) {
    if(i < 10) {
      i <- paste("00", i, sep = "")
    } else if(i < 100) {
      i <- paste("0", i, sep = "")
    }
    csv <- paste(directory, "/", i, ".csv", sep = "")
    newdata <- read.csv(csv)
    data <- newdata[, pollutant]
    bad <- is.na(data)
    clean <- data[!bad]
    if( is.null(totaldata) ) {
      totaldata <- clean
    } else {
      totaldata <- c(totaldata, clean)
    }
  }
  newmean <- mean(totaldata)
  newmean
}