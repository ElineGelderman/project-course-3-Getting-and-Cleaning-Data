# Getting and Cleaning Data Project 

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load Packages and get the Data
#install.packages("data.table") if not installed in R
#install.packages("reshape2") if not installed in R
library(data.table)
library(reshape2)

sapply(c("data.table", "reshape2"), require, character.only=TRUE, quietly=TRUE)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(getwd(), "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

# Load activity labels + features
activityLabels <- fread(file.path(getwd(), "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
features <- fread(file.path(getwd(), "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))

#Extracts only the measurements on the mean and standard deviation for each measurement (2)
featuresExtracted <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresExtracted, featureNames]
measurements <- gsub('[()]', '', measurements)


# Load train data set
trainingData <- fread(file.path(getwd(), "UCI HAR Dataset/train/X_train.txt"))[, featuresExtracted, with = FALSE]
data.table::setnames(trainingData, colnames(trainingData), measurements)
trainingActivities <- fread(file.path(getwd(), "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainingSubjects <- fread(file.path(getwd(), "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNumber"))
training <- cbind(trainingSubjects, trainingActivities, trainingData)

# Load test data set
testData <- fread(file.path(getwd(), "UCI HAR Dataset/test/X_test.txt"))[, featuresExtracted, with = FALSE]
data.table::setnames(testData, colnames(testData), measurements)
testActivities <- fread(file.path(getwd(), "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- fread(file.path(getwd(), "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNumber"))
testing <- cbind(testSubjects, testActivities, testData)

# Merges the training and the test sets to create one data set (1)
mergedData <- rbind(training, testing)


# Give descriptively activity names to name the activities in the data set (3)
mergedData[["Activity"]] <- factor(mergedData[, Activity]
                                 , levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])

mergedData[["SubjectNumber"]] <- as.factor(mergedData[, SubjectNumber])

#Creates independent tidy data set with the average of each variable for each activity and each subject (5)
mergedData <- reshape2::melt(data = mergedData, id = c("SubjectNumber", "Activity"))
mergedData <- reshape2::dcast(data = mergedData, SubjectNumber + Activity ~ variable, fun.aggregate = mean)
data.table::fwrite(x = mergedData, file = "tidyData.txt", quote = FALSE)



