newlines <- c()
results <- c()
nums <- c()
lines <- lines[5:length(lines)]
for (line in lines) {
  line <- strsplit(line, "     ")
  newlines <- c(newlines, line)
}
for (newline in newlines) {
  newline <- newline[3]
  results <- c(results, newline)
}
for (result in results) {
  if (grepl(" ", result)) {
    result <- strsplit(result, " ")
    result <- result[[1]][1]
    result <- as.numeric(result)
    nums <- c(nums, result)
  }
  if (grepl("-", result)) {
    result <- strsplit(result, "-")
    result <- result[[1]][1]
    result <- as.numeric(result)
    nums <- c(nums, result)
  }
}