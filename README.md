# Data-Cleansing-Coursera

Getting-and-Cleaning-Data-ProjectWeek 4 Project Of Getting And Cleaning Data Course (3rd)

Wording:

You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set. Extracts only the measurements on the mean and standard deviation for each measurement. Uses descriptive activity names to name the activities in the data set Appropriately labels the data set with descriptive variable names. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. Data descriptionThe data which is used is split into two folders, the "train" folder which is the training set on which the machine learning algorithm is trained to make predictions and the other is the test data on which it will be tested for its effectiveness. The data files in those two folders consist of two types of data, the X data which contains the measures of the sensor signals from the waist-mounted smartphones of 30 subjects and the Y data which indicates the activity type during the recording.

Code explainationThe code in the file "run_analysis.R", if run properly with the required libraries, will download the original data in a zip format, it will unzip the data into a folder and it will combine the X-data and Y-data from the test and train folders into a new single dataset based on the variables which contain the words "mean" and "std". The new dataset will be mutated into a dataset with the correct readable column names (variables) and will contain the average of each activity type for all subjects and it will store the result into a text file.

New datasetThe new generated and stored data assuming that the source script was succesfully run, will contain a tidy data which means that each column will contain a readable label with one variable and each row will contain a full observation and of course each table within the new data contains a full observation, each cell will contain the average of each variable for each activity type.
