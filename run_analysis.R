# The purpose of this project is to demonstrate ability to collect, work with, and clean a data set. 
# The goal is to prepare tidy data that can be used for later analysis.

library(dplyr);

# Step 0 - Read data from file

dataset <- "UCI HAR Dataset"
testDir <- "test"
trainDir <- "train"

# Activies 
activityTest <- read.table(file.path(dataset, testDir , "Y_test.txt" ), header = FALSE)
activityTrain <- read.table(file.path(dataset, trainDir, "Y_train.txt"), header = FALSE)
# Subject
subjectTest <- read.table(file.path(dataset, testDir, "subject_test.txt"), header = FALSE)
subjectTrain <- read.table(file.path(dataset, trainDir, "subject_train.txt"), header = FALSE)
# Features
featuresTest <- read.table(file.path(dataset, testDir, "X_test.txt" ), header = FALSE)
featuresTrain <- read.table(file.path(dataset, trainDir, "X_train.txt"), header = FALSE)

# Step 1 - Merges the training and the test sets to create one data set.

# Concatenation of dataset
activity <- rbind(activityTrain, activityTest)
subject <- rbind(subjectTrain, subjectTest)
features <- rbind(featuresTrain, featuresTest)
# Set names of variables
names(activity) <- c("activity")
names(subject) <- c("subject")
featuresNames <- read.table(file.path(dataset, "features.txt"), header = FALSE)
names(features) <- featuresNames$V2
# Create one dataset
combine <- cbind(subject, activity)
totalData <- cbind(features, combine)

# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.

subFeaturesNames <- featuresNames$V2[grep("mean\\(\\)|std\\(\\)", featuresNames$V2)]
# Remove extra information
selNames <- c(as.character(subFeaturesNames), "subject", "activity")
totalData <- subset(totalData, select = selNames)

# Step 3 - Uses descriptive activity names to name the activities in the data set

activityLabels <- read.table(file.path(dataset, "activity_labels.txt"), header = FALSE)
totalData$activity <- factor(totalData$activity, levels = activityLabels[, 1], labels = activityLabels[, 2])

# Step 4 - Appropriately labels the data set with descriptive variable names. 

names(totalData) <- gsub("^t", "time", names(totalData))
names(totalData) <- gsub("^f", "frequency", names(totalData))
names(totalData) <- gsub("Acc", "Accelerometer", names(totalData))
names(totalData) <- gsub("Gyro", "Gyroscope", names(totalData))
names(totalData) <- gsub("Mag", "Magnitude", names(totalData))
names(totalData) <- gsub("BodyBody", "Body", names(totalData))

# Step 5 - From the data set in step 4, creates a second, independent tidy 
#          data set with the average of each variable for each activity and each subject.

tidyData <- aggregate(. ~subject + activity, totalData, mean)
tidyData <- arrange(tidyData, subject, activity)
write.table(tidyData, file = "tidy_data.txt", row.name=FALSE)