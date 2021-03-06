##Human Activity Recognition Using Smartphones Dataset Version 1.0

Data set: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip   

The experiment was carried out with a group of 30 volunteers within an age bracket of 19-48 years.  
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments were video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

For each record the following info are recorded:  
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.  
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.  

The dataset includes the following files:  

- 'README.txt'    

- 'features_info.txt': Shows information about the variables used on the feature vector.  

- 'features.txt': List of all features.  

- 'activity_labels.txt': Links the class labels with their activity name.  

- 'train/X_train.txt': Training set.  

- 'train/y_train.txt': Training labels.  

- 'test/X_test.txt': Test set.  

- 'test/y_test.txt': Test labels.  

The following files are available for the train and test data. Their descriptions are equivalent.   

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

### Steps followed to create the tidy data set
1.Data from test, train and subject were read and merged into one single dataset called mergedData  

2.Meaningful activity names were given by merging the data read from activity labels. Extracted only the measurements on the mean and standard deviation for each measurement     

3.Averages were computed by subjectid and activityname and sortes by subjectid and activityname using plyr package.  
Resulting data was stored in the variable aggregated_data

4.ColumnNames in the aggregated dataset were cleaned up by giving meaningful names  

5.The cleaned up data was written into file tidydata.txt 

6.tidydata has 180 Obs and 68 Variables

### Structure of tidydata
  subjectid                                     : chr    
  activityName                                  : chr      
  timeBodyAccelerometer-MEAN()-X                : num         
  timeBodyAccelerometer-MEAN()-Y                : num         
  timeBodyAccelerometer-MEAN()-Z                : num      
  timeBodyAccelerometer-SD()-X                  : num        
  timeBodyAccelerometer-SD()-Y                  : num            
  timeBodyAccelerometer-SD()-Z                  : num       
  timeGravityAccelerometer-MEAN()-X             : num      
  timeGravityAccelerometer-MEAN()-Y             : num        
  timeGravityAccelerometer-MEAN()-Z             : num           
  timeGravityAccelerometer-SD()-X               : num        
  timeGravityAccelerometer-SD()-Y               : num          
  timeGravityAccelerometer-SD()-Z               : num        
  timeBodyAccelerometerJerk-MEAN()-X            : num      
  timeBodyAccelerometerJerk-MEAN()-Y            : num          
  timeBodyAccelerometerJerk-MEAN()-Z            : num        
  timeBodyAccelerometerJerk-SD()-X              : num    
  timeBodyAccelerometerJerk-SD()-Y              : num       
  timeBodyAccelerometerJerk-SD()-Z              : num     
  timeBodyGyroscope-MEAN()-X                    : num      
  timeBodyGyroscope-MEAN()-Y                    : num     
  timeBodyGyroscope-MEAN()-Z                    : num             
  timeBodyGyroscope-SD()-X                      : num        
  timeBodyGyroscope-SD()-Y                      : num    
  timeBodyGyroscope-SD()-Z                      : num    
  timeBodyGyroscopeJerk-MEAN()-X                : num    
  timeBodyGyroscopeJerk-MEAN()-Y                : num    
  timeBodyGyroscopeJerk-MEAN()-Z                : num    
  timeBodyGyroscopeJerk-SD()-X                  : num    
  timeBodyGyroscopeJerk-SD()-Y                  : num    
  timeBodyGyroscopeJerk-SD()-Z                  : num    
  timeBodyAccelerometerMagnitude-MEAN()         : num     
  timeBodyAccelerometerMagnitude-SD()           : num    
  timeGravityAccelerometerMagnitude-MEAN()      : num    
  timeGravityAccelerometerMagnitude-SD()        : num      
  timeBodyAccelerometerJerkMagnitude-MEAN()     : num  
  timeBodyAccelerometerJerkMagnitude-SD()       : num     
  timeBodyGyroscopeMagnitude-MEAN()             : num      
  timeBodyGyroscopeMagnitude-SD()               : num   
  timeBodyGyroscopeJerkMagnitude-MEAN()         : num   
  timeBodyGyroscopeJerkMagnitude-SD()           : num    
  frequencyBodyAccelerometer-MEAN()-X           : num    
  frequencyBodyAccelerometer-MEAN()-Y           : num    
  frequencyBodyAccelerometer-MEAN()-Z           : num    
  frequencyBodyAccelerometer-SD()-X             : num    
  frequencyBodyAccelerometer-SD()-Y             : num   
  frequencyBodyAccelerometer-SD()-Z             : num    
  frequencyBodyAccelerometerJerk-MEAN()-X       : num    
  frequencyBodyAccelerometerJerk-MEAN()-Y       : num   
  frequencyBodyAccelerometerJerk-MEAN()-Z       : num   
  frequencyBodyAccelerometerJerk-SD()-X         : num      
  frequencyBodyAccelerometerJerk-SD()-Y         : num      
  frequencyBodyAccelerometerJerk-SD()-Z         : num   
  frequencyBodyGyroscope-MEAN()-X               : num   
  frequencyBodyGyroscope-MEAN()-Y               : num    
  frequencyBodyGyroscope-MEAN()-Z               : num    
  frequencyBodyGyroscope-SD()-X                 : num   
  frequencyBodyGyroscope-SD()-Y                 : num    
  frequencyBodyGyroscope-SD()-Z                 : num            
  frequencyBodyAccelerometerMagnitude-MEAN()    : num    
  frequencyBodyAccelerometerMagnitude-SD()      : num    
  frequencyBodyAccelerometerJerkMagnitude-MEAN(): num     
  frequencyBodyAccelerometerJerkMagnitude-SD()  : num   
  frequencyBodyGyroscopeMagnitude-MEAN()        : num   
  frequencyBodyGyroscopeMagnitude-SD()          : num      
  frequencyBodyGyroscopeJerkMagnitude-MEAN()    : num    
  frequencyBodyGyroscopeJerkMagnitude-SD()      : num    

 
