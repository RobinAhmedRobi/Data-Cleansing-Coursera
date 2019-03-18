#clean workspace and set the working directory
rm(list = ls())
setwd("D:/R")

#download dataset and unzip
filename <- "dataset.zip"
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename)
}  
if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}

#Load activity labels and features
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
features_need <- features[grep("mean|std", features[,2]), ]


#load test and train data, and select the needed features
data_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
data_train_need <- data_train[, as.numeric(features_need[,1])]
label_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

data_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
data_test_need <- data_test[, as.numeric(features_need[,1])]
label_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#combine test and train data with labels
train <- cbind(subject_train, label_train, data_train_need)
test <- cbind(subject_test, label_test, data_test_need)

#merge the two datasets (test and train) into one
mergeddata <- rbind(train, test)

#clean the label names, then rename merged data with descriptive variable names
features_need[,2]<- gsub("-","_", features_need[,2])
features_need[,2]<- gsub("[()]","", features_need[,2]) #need to add []
colnames(mergeddata) <- c("subject", "activity", features_need[,2]) 

#Use descriptive activity names to name the activities in the data set
mergeddata$activity <- factor(mergeddata$activity, levels = activity_labels[, 1], labels = activity_labels[, 2])

#creates a tidy data set with the average of each variable for each activity and each subject.
summary_data <-aggregate(mergeddata[, 3:ncol(mergeddata)], by=list(mergeddata$subject,mergeddata$activity), FUN=mean, na.rm=TRUE)
colnames(summary_data) [1:2]<- c("subject", "activity") 
write.table(summary_data, file="summary_data.txt", row.names = FALSE)
