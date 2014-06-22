# Set working dir.

setwd("C:/Users/Jg/Desktop/UCI_HAR_ Dataset")

# Step1. Merges the training and the test sets to create one data set.

trainX <- read.table("train/X_train.txt")

trainY <- read.table("train/y_train.txt")
table(trainY)

trainSubject <- read.table("train/subject_train.txt")

testX <- read.table("test/X_test.txt")

testY <- read.table("test/y_test.txt") 
table(testY) 

testSubject <- read.table("test/subject_test.txt")

joinData <- rbind(trainX, testX)

joinLabel <- rbind(trainY, testY)

joinSubject <- rbind(trainSubject, testSubject)

# Step2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")

workingData <- grep("mean\\(\\)|std\\(\\)", features[, 2])

joinData <- joinData[, workingData]

# remove brackets
names(joinData) <- gsub("\\(\\)", "", features[workingData, 2]) 
# remove -
names(joinData) <- gsub("-", "", names(joinData))  
# capitalize M
names(joinData) <- gsub("mean", "Mean", names(joinData))
# capitalize S
names(joinData) <- gsub("std", "Std", names(joinData))

# Step3. Uses descriptive activity names to name the activities in the data set

activity <- read.table("activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity names. 

names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)

#write.table(cleanedData, "result_data.txt") 

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject.

# Calculate lenghts
subjectLen <- length(table(joinSubject)) 
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]

# I want a data frame
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)

# Calculate average of vars.
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}

write.table(result, "result_tidy_data.txt") 
