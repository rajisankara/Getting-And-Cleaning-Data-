## GNCD_Wk4_Project  

##**********************************************************************************************************************
## Data for project is downladed from
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip and stored in local directory
##path is set to that location
## Files that will be used for the project are from that location

##***********************************************************************************************************************
##1..SUBJECT FILES
      ##.test/subject_test.txt
      ##.train/subject_train.txt
##2.ACTIVITY FILES
      ##.test/X_test.txt
      ##.train/X_train.txt
##3.DATA FILES
      ##.test/y_test.txt
      ##.train/y_train.txt

##4.features.txt - Names of column variables in the dataTable


##5.activity_labels.txt - Links the class labels with their activity name.

##**************************************************************************************************************************

## Step 1 - Read test and training data and Merges the training and the test sets to create one data set.

##**************************************************************************************************************************

##******Add needed libraries ***********************************************************************************************

library(dplyr)
library(data.table)
library(plyr)

##**************************************************************************************************************************
setwd("C:/Raji Data/Coursera DataScience/Getting And Cleaning Data/Week4_Project/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
x_train <- read.table("train/X_train.txt") 
y_train <- read.table("train/y_train.txt") 
subject_train <- read.table("train/subject_train.txt") 


x_test <- read.table("test/X_test.txt") 
y_test <- read.table("test/y_test.txt") 
subject_test <- read.table("test/subject_test.txt") 

##3 Read Features file 
features <- read.table("features.txt") 

##All Activity Data
all_activity_data <- rbind(y_train, y_test)

##All Subject Data
all_subject_data <- rbind(subject_train, subject_test)
colnames(all_subject_data ) = "subjectid"
##All X Data
all_x_data <- rbind(x_train, x_test)
colnames(all_x_data) =  features[,2]

## Give Meaningful column Names
colnames(all_activity_data) =  "activityid"
colnames(all_subject_data ) = "subjectid"
colnames(all_x_data) =  features[,2]


### Merge all files in to one single for processing
mergedData <- cbind(all_activity_data, all_subject_data, all_x_data)

##*********************************************************************************************************
## Step 2 
## Extract only the measurements on the mean and standard deviation for each measurement 
##*********************************************************************************************************

## get only columns with mean() or std() in their names 
## mean_and_std_col_from_features <- grep("-(mean|std)\\(\\)", features[, 2]) 
 
 mean_and_std_col_from_features <- grep("mean\\(\\)|std\\(\\)", features[, 2], value=TRUE)
 
## Add subject_id and activity_id, cols with mean and cols with std to the vector
 
col_subj_activity_mean_std <- union(c("subjectid","activityid"), mean_and_std_col_from_features)
##print(col_subj_activity_mean_std)

## subset the desired columns 
mergedData <- mergedData[, col_subj_activity_mean_std]


##*********************************************************************************************************
## Step 3 
## Assign descriptive activity labels to activities in the mergedData 
##*********************************************************************************************************


activities <- read.table("activity_labels.txt")
print(colnames(activities))
setnames(activities, names(activities), c("activityid","activityName"))
##print(colnames(activities))

mergedData <- merge(activities, mergedData , by="activityid", all.x=TRUE)
mergedData$activityName <- as.character(mergedData$activityName)
mergedData$subjectid <- as.character(mergedData$subjectid )
##print(colnames(mergedData))

##*********************************************************************************************************
## Step 4 
##  Compute averages by subjectid and activityname and sort them by subjectid and activityname using plyr package
##*********************************************************************************************************
##aggregated_data <- select(aggregated_data, -activityid)
aggregated_data <- ddply(mergedData, .(subjectid, activityName),function(x) colMeans(x[, 4:69]))
##print(nrow(aggregated_data))
##sort the data based on subjectid and activityName
aggregated_data <- arrange(aggregated_data, subjectid,activityName)
##str(aggregated_data)

##*********************************************************************************************************
## Step 5 
## Give meaning ful column names to the columns in aggregated_data and 
##  write the aggregated data into tidydata file
##*********************************************************************************************************

names(aggregated_data)<-gsub("std()", "SD", names(aggregated_data))
names(aggregated_data)<-gsub("mean()", "MEAN", names(aggregated_data))
names(aggregated_data)<-gsub("^t", "time", names(aggregated_data))
names(aggregated_data)<-gsub("^f", "frequency", names(aggregated_data))
names(aggregated_data)<-gsub("Acc", "Accelerometer", names(aggregated_data))
names(aggregated_data)<-gsub("Gyro", "Gyroscope", names(aggregated_data))
names(aggregated_data)<-gsub("Mag", "Magnitude", names(aggregated_data))
names(aggregated_data)<-gsub("BodyBody", "Body", names(aggregated_data))

##print(colnames(aggregated_data))
print(str(aggregated_data))
write.table(aggregated_data, "tidydata.txt", row.name=FALSE)

