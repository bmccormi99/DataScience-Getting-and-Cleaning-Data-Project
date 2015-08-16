
# CodeBook
## Coursera Data Specialization
## Course Project:  Getting and Cleaning data

### The purpose of the project was to collect, work with, clean, organize, and finally create a tidy data set summary from it.

#### Create one R script called run_analysis.R that performs these tasks.  These tasks are detailed below, and via the linked documentaion.

###Collection of the raw data
From the project assignment:
"One of the most exciting areas in all of data science right now is wearable computing - see for example (http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/) . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained":

(http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

###The original (raw) data 
(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

###Creating the tidy datafile
After re-reading the assignment carefully (and the grading criteria), I determined that the R script was not to download and unzip the source input data files.  Reading through the discussion forums, I assumed this was due to the variations of operating systems and methods to download and unzip files.
"The code should have a file run_analysis.R in the main directory that can be run as long as the Samsung data is in your working directory."
After unzipping the data files described above, you will have a folder called "UCI HAR DATASET".  In this folder with be a README.TXT for information about the data files and additional details on the collection project.  

I have also placed this additional readme file on my github repo.  (https://github.com/bmccormi99/DataScience-Getting-and-Cleaning-Data-Project/blob/master/DATA/README.txt)

###README.MD 
The prior readme.txt is about the data colleciton project used in the assignment.  This file (README.MD) provides instructions for download, unzip, and placement of the data files along with the execution of the run_Analysis.R script to create the tidy output summary data file.  (https://github.com/bmccormi99/DataScience-Getting-and-Cleaning-Data-Project/blob/master/README.MD)

###Cleaning of the data
To ingest, clean, and eventually tidy the input data files the script will perform five tasks:

1.  Merges the training and the test sets to create one data set.  (all_combine)
2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
3.  Uses descriptive activity names to name the activities in the data set
4.  Appropriately labels the data set with descriptive variable names. 
5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. (tidyData)

####After step 1, all_combine will be this size:
```{r}
dim(all_combine)
[1] 10299   563
```
####After step 2, all_df will be this size (only those variable columns with mean or std in the name):
```{r}
dim(all_df)
[1] 10299   68
```
####During step 3, these activityIds will be replaced by these activityLabels will in the df:
```{r}
activityLabels
  activityId       activityType
1          1            WALKING
2          2   WALKING_UPSTAIRS
3          3 WALKING_DOWNSTAIRS
4          4            SITTING
5          5           STANDING
6          6             LAYING
```
####Step 4 simply does a series of gsub() to appropriately label the variables (see the variable section below for the final names chosen)

####Step 5.  Creating the tidyData 
After step 5, there is one row per activityType and Subject (30 subjects and 6 for 180 rows, and 68 columns.)
This is accomplished with an aggregate() funtion

Column 1 will be the activityType and Column 2 will be the Subject (a number representing the person #1-30).
The activityType was created in Step 3 of the assignment.  

For each of the mean and standard deviation columns extracted in Step 2, columns 3-68 will correspond to the mean of those data from the study.   There are a series of "time" and then "freq" columns (the first part of the name).  X,Y, and Z in the namesre
present the three readings for that variable type.
 
The final data frame is called "tidyData" and will be used in the write.data step to output the final txt file. 

The source code is well documented and you will be able to read along with the comments for the steps used to complete the assignment.

```{r}
dim(tidyData)
[1] 180   68
```

###Variables in the output dataset - number refers to the column number
```{r}
colnames(tidyData)
```
 [1] "activityType"                               
 [2] "Subject"                             
 [3] "timeBodyAccelerometer-mean-X"           
 [4] "timeBodyAccelerometer-mean-Y"           
 [5] "timeBodyAccelerometer-mean-Z"           
 [6] "timeBodyAccelerometer-std-X"            
 [7] "timeBodyAccelerometer-std-Y"            
 [8] "timeBodyAccelerometer-std-Z"            
 [9] "timeGravityAccelerometer-mean-X"        
[10] "timeGravityAccelerometer-mean-Y"        
[11] "timeGravityAccelerometer-mean-Z"        
[12] "timeGravityAccelerometer-std-X"         
[13] "timeGravityAccelerometer-std-Y"         
[14] "timeGravityAccelerometer-std-Z"         
[15] "timeBodyAccelerometerJerk-mean-X"       
[16] "timeBodyAccelerometerJerk-mean-Y"       
[17] "timeBodyAccelerometerJerk-mean-Z"       
[18] "timeBodyAccelerometerJerk-std-X"        
[19] "timeBodyAccelerometerJerk-std-Y"        
[20] "timeBodyAccelerometerJerk-std-Z"        
[21] "timeBodyGyroscope-mean-X"               
[22] "timeBodyGyroscope-mean-Y"               
[23] "timeBodyGyroscope-mean-Z"               
[24] "timeBodyGyroscope-std-X"                
[25] "timeBodyGyroscope-std-Y"                
[26] "timeBodyGyroscope-std-Z"                
[27] "timeBodyGyroscopeJerk-mean-X"           
[28] "timeBodyGyroscopeJerk-mean-Y"           
[29] "timeBodyGyroscopeJerk-mean-Z"           
[30] "timeBodyGyroscopeJerk-std-X"            
[31] "timeBodyGyroscopeJerk-std-Y"            
[32] "timeBodyGyroscopeJerk-std-Z"            
[33] "timeBodyAccelerometerMagnitude-mean"    
[34] "timeBodyAccelerometerMagnitude-std"     
[35] "timeGravityAccelerometerMagnitude-mean"         
[36] "timeGravityAccelerometerMagnitude-std"    
[37] "timeBodyAccelerometerJerkMagnitude-mean"     
[38] "timeBodyAccelerometerJerkMagnitude-std"     
[39] "timeBodyGyroscopeMagnitude-mean"     
[40] "timeBodyGyroscopeMagnitude-std"         
[41] "timeBodyGyroscopeJerkMagnitude-mean"    
[42] "timeBodyGyroscopeJerkMagnitude-std"     
[43] "freqBodyAccelerometer-mean-X"           
[44] "freqBodyAccelerometer-mean-Y"           
[45] "freqBodyAccelerometer-mean-Z"           
[46] "freqBodyAccelerometer-std-X"            
[47] "freqBodyAccelerometer-std-Y"            
[48] "freqBodyAccelerometer-std-Z"            
[49] "freqBodyAccelerometerJerk-mean-X"       
[50] "freqBodyAccelerometerJerk-mean-Y"       
[51] "freqBodyAccelerometerJerk-mean-Z"       
[52] "freqBodyAccelerometerJerk-std-X"        
[53] "freqBodyAccelerometerJerk-std-Y"        
[54] "freqBodyAccelerometerJerk-std-Z"        
[55] "freqBodyGyroscope-mean-X"               
[56] "freqBodyGyroscope-mean-Y"               
[57] "freqBodyGyroscope-mean-Z"               
[58] "freqBodyGyroscope-std-X"   
[59] "freqBodyGyroscope-std-Y"                
[60] "freqBodyGyroscope-std-Z"                
[61] "freqBodyAccelerometerMagnitude-mean"        
[62] "freqBodyAccelerometerMagnitude-std"        
[63] "freqBodyAccelerometerJerkMagnitude-mean"        
[64] "freqBodyAccelerometerJerkMagnitude-std"        
[65] "freqBodyGyroscopeMagnitude-mean"        
[66] "freqBodyGyroscopeMagnitude-std"        
[67] "freqBodyGyroscopeJerkMagnitude-mean"    
[68] "freqBodyGyroscopeJerkMagnitude-std"

##Sources
Much insight was gained from reading through fellow students' issues via the discussion forumns.  The hints from the TA's were also a great source of information.  Numerous web searches also assisted with refinement for this assignment.  Credits to all, I learned much in the process.

