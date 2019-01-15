library(dplyr) 
 

## Data url and file name 
  

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
filename <- "Dataset.zip" 
 

  if(!file.exists(filename)) { 
     download.file(fileurl,filename, mode="wb") 
    } else {print("Already downloaded")}   

datapath <- "UCI HAR Dataset" 
 if(!file.exists(datapath)){ 
    unzip(filename) 
  } else {print("Already unzipped")} 
  
 ## Read data 

  trainingSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt") 
  trainingValues <- read.table("UCI HAR Dataset/train/X_train.txt") 
  trainingLabels <- read.table("UCI HAR Dataset/train/y_train.txt") 
  
  testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt") 
  testValues <- read.table("UCI HAR Dataset/test/X_test.txt") 
  testLabels <- read.table("UCI HAR Dataset/test/y_test.txt") 
  

  ## Read the features and activity labels 
  
  features <- read.table("UCI HAR Dataset/features.txt", as.is = TRUE) 
 
  activities <- read.table("UCI HAR Dataset/activity_labels.txt") 
 

  ## All preparatory data has been read 
  ## Now start tidying the disparate sets of data into a single tidy set 
  ## First, merge the training and test data sets 
  

  mergedDataset <- rbind( 
   cbind(trainingSubjects, trainingValues, trainingLabels), 
   cbind(testSubjects, testValues, testLabels) 
  ) 

43  ## Next, need to assign column names to this merged data set 
44  ## source for these names should be from 
45  ## (a) Subjects column can just be named as "Subjects" 
46  ## (b) 561 variable names should come from the features dataset 
47  ## (c) Labels are the different activities detected which should come  
48  ## the second column of activities data set, called V2 
49  

50  colnames(mergedDataset) <- c("Subject", features[,2], "Activity") 
51  

52  ## Second point in assignment 
53  ## Extract only the columns which have mean and std dev values 
54  ## need to do a pattern search through the column names 
55  ## We still want the Subject and Activity columns of course 
56  

57  subsetColumns <- grepl("subject|activity|mean|std", colnames(mergedDataset), 
                           58                         ignore.case = TRUE) 
59  

60  ## use this subset to trim the merged data set 
61  

62  mergedDataset <- mergedDataset[,subsetColumns] 
63  

64  ## Third point in assignment 
65  ## Replace the activity identifiers with the descriptive activity names 
66  ## This should be from the activities data set 
67  

68  mergedDataset$Activity <- factor(mergedDataset$Activity, labels = activities[,2]) 
69  

70  

71  ## Fourth point in assignment 
72  ## Appropriately label the data set with descriptive variable names 
73  ## One clear need is to remove special characters -() etc from the column names 
74  

75  # first fetch the col names 
76  datacols <- colnames(mergedDataset) 
77  

78  # Remove special characters 
79  datacols <- gsub("[\\(\\)-]","", datacols) 
80  

81  #Expand some abbreviations 
82  datacols <- gsub("^t","time", datacols) 
83  datacols <- gsub("^f","freq", datacols) 
84  datacols <- gsub("Acc","Accelerometer", datacols) 
85  datacols <- gsub("Gyro","Gyroscope", datacols) 
86  datacols <- gsub("mean","Mean", datacols) 
87  datacols <- gsub("std","StdDev", datacols) 
88  datacols <- gsub("Mag","Magnitude", datacols) 
89  datacols <- gsub("BodyBody","Body", datacols) 
90  

91  # Set the cleaned names back in the data set 
92  colnames(mergedDataset) <- datacols 
93  

94  ## Fifth task in assignment 
95  ## Create another tidy data set with the average of each variable 
96  ## for each activity and each subject 
97  

98  mergedAverages <- mergedDataset %>% 
  99    group_by(Subject, Activity) %>% 
  100    summarise_each(funs(mean)) 
101  

102  ## Write this dataset to a txt file 
103  write.table(file = "tidy_data.txt", mergedAverages, row.names = FALSE, quote = FALSE) 
