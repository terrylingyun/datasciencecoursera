library(data.table)
library(dplyr)

# Download and unzip the dataset if it does not exist
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "UCI HAR Dataset.zip"
dest <- "UCI HAR Dataset"
if (!dir.exists(dest)) {
  download.file(url, file, method = "curl")
  print("Data downloaded.")
  unzip(file)
  print("Data unzipped.")
} else {
  print("Data exist.")
}
# Read all the relevant data labels and features
actlab <- fread(paste(dest, "activity_labels.txt", sep = "/"))
feature <- fread(paste(dest, "features.txt", sep="/"))

# Subset the variables containing only the means and standard deviations
features <- feature[grepl("mean", feature$V2) | grepl("std", feature$V2), ]

# Read all the subject, the actual dataset, and label info for TEST
testsub <- fread(paste(dest, "test", "subject_test.txt", sep = "/"))
testset <- fread(paste(dest, "test", "X_test.txt", sep = "/"))
testlab <- fread(paste(dest, "test", "y_test.txt", sep = "/"))

# Subset the dataset with only means and standard deviations
testset <- testset[, features$V1, with = FALSE]

# Read all the subject, the actual dataset, and label info for TRAIN
trainsub <- fread(paste(dest, "train", "subject_train.txt", sep = "/"))
trainset <- fread(paste(dest, "train", "X_train.txt", sep = "/"))
trainlab <- fread(paste(dest, "train", "y_train.txt", sep = "/"))

# Subset the dataset with only means and standard deviations
trainset <- trainset[, features$V1, with = FALSE]

# Read column names from features
colnames <- features$V2

# Combine TEST data and set column names
testdat <- cbind(testsub, testlab, testset)
colnames(testdat) <- c("Subject", "Activity", colnames)

# Combine TRAIN data and set column names
traindat <- cbind(trainsub, trainlab, trainset)
colnames(traindat) <- c("Subject", "Activity", colnames)

# Combine TEST and TRAIN data
dat <- rbind(testdat, traindat)

# Merge to use descriptive measurement for the different activities
dattemp <- merge(dat, actlab, by.x = "Activity", by.y = "V1")

# Reorder the data set columns, reset the column names, and reorder the rows
finaldat <- setcolorder(dattemp[, 2:82], c(1, 81, 2:80))
colnames(finaldat) <- c("Subject", "Activity", colnames)
finaldat <- arrange(finaldat, Subject)

# Summarize the mean for each variable for each activity and each subject
newdat <- dat %>% group_by(Subject, Activity) %>% summarize_all("mean")

# Merge to use descriptive measurement for the different activities
newdattemp <- merge(newdat, actlab, by.x = "Activity", by.y = "V1")

# Reorder the data set columns, reset the column names, and reorder the rows
newfinaldat <- setcolorder(newdattemp[, 2:82], c(1, 81, 2:80))
colnames(newfinaldat) <- c("Subject", "Activity", colnames)
newfinaldat <- arrange(newfinaldat, Subject)

# Write the final tidy data into files
fwrite(finaldat, file = "TidyData.txt", quote = FALSE)
fwrite(newfinaldat, file = "TidyData.csv")

# Required for submission
write.table(newfinaldat, file = "TidyDataUpload.txt", row.name=FALSE)