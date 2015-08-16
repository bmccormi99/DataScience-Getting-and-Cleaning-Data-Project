## Load necessary libraries
library(dplyr)
library(plyr)

## this is needed in my windows env - had trouble with the download until I
## researched and found this
## if you have followed the readme.md file, you already have the file
## so, the file will exist and skip the download/unzip steps
setInternet2(use = TRUE)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
zipfilename <- "getdata_projectfiles_UCI HAR Dataset.zip"
destdir <- "UCI HAR Dataset"

## if the un zipped file does not exist - download it and unzip it
## at the end jof thie you will have a subfolder (from your working dir)
## called "UCI HAR Dataset"
if (!file.exists(destdir)) {
    if(!file.exists(zipfilename)) {
        ## Download the file
       download.file(fileURL, destfile=zipfilename, method="internal" , mode="wb")
    }
  ## Unzip the file
     unzip(zipfilename)
}

## 1. Merges the training and the test sets to create one data set:
## Read in the _train and _test files for the X, Y, and Subject sets
  x_train <- read.table(paste("./",destdir,"/train/X_train.txt", sep = ''), header = FALSE)
  X_test <- read.table(paste("./",destdir,"/test/X_test.txt", sep = ''), header = FALSE)

  y_train <- read.table(paste("./",destdir,"/train/y_train.txt",sep = ''), header = FALSE)
  y_test <- read.table(paste("./",destdir,"/test/y_test.txt",sep = ''), header = FALSE)

  subject_train <- read.table(paste("./",destdir,"/train/subject_train.txt", sep = ''),header = FALSE)
  subject_test <- read.table(paste("./",destdir,"/test/subject_test.txt",sep = ''), header = FALSE)

# Combines data tables (train and test, by rows)
  x_combine <- rbind(x_train, X_test)
  y_combine <- rbind(y_train, y_test)
  s_combine <- rbind(subject_train, subject_test)
  ## now combine, by columns, s, y, and x
  all_combine <- cbind(s_combine, y_combine, x_combine)

# Read features.txt for the labels
  featureLabels <- read.table(paste("./",destdir,"/features.txt",sep = ''))
# Read activity.txt for the activity row descriptions
  activityLabels <- read.table(paste("./",destdir,"/activity_labels.txt",sep = ''))

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## Put the combined data into dplyr data table
all_df <- tbl_df(all_combine)

### mean() and std() column list, need to add up 2 (Subject, Activity)
## these grep functions will take any column with Mean, mean, Std, or std 

collist <- grep("[Mm]ean\\()|[Ss]td\\()", featureLabels$V2)
colnames <- grep("[Mm]ean\\()|[Ss]td\\()", featureLabels$V2, value=TRUE)
collist <- collist + 2
collist <- c(1, 2, collist)	
colnames <- c("Subject", "activityId", colnames)

## Select columns
## use the collist as a logical vector to get the desired columns
all_df <- all_df[, c(collist)]

## these activity labels need the names so that it can be merged later
colnames(activityLabels)  = c('activityId','activityType')


## 3. Uses descriptive activity names to name the activities in the data set

## apply the column name data to the combined dataset
names(all_df) <- colnames

## clean up the col names
## do a series to gsub to substibute text in the column labels into a more readable label
## removed (), leading t and f to time and freq, Mag to Magnitude, etc.
names(all_df) <- gsub('\\(|\\)',"",names(all_df)) 
names(all_df) <- gsub("^(t)","time",names(all_df)) 
names(all_df) <- gsub("^(f)","freq",names(all_df)) 
names(all_df) <- gsub("Mag","Magnitude",names(all_df)) 
names(all_df) <- gsub("Gyro","Gyroscope",names(all_df)) 
names(all_df) <- gsub("Acc","Accelerometer",names(all_df)) 
names(all_df) <- gsub("BodyBody","Body",names(all_df)) 

## 4. Appropriately labels the data set with descriptive variable names. 

## Join (merge) the activity labels data with the combined data set, by activityid
activity2 <- join(activityLabels, all_df, by = "activityId")

## after that join, you end up with the activityId in column 1 which is not needed
## it was replaced by activityType column- this will drop the first column
activity2 <- activity2[,-1] 

## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

## this aggregate will summarize down to one row be ativityType and Subject
tidyData <- aggregate(activity2[,names(activity2) != c('activityType','Subject')],by=list(activityType=activity2$activityType,Subject = activity2$Subject),mean); 

## setup the output file folder/name
tidyDataFileOutTxt <- "tidyData.txt"
tidyDataFileOutTxt  <- paste(destdir,"/" ,tidyDataFileOutTxt , sep="")

## write out the output file
## it will be a tab delimited file
write.table(tidyData, tidyDataFileOutTxt,row.names=FALSE,sep='\t')

print("D O N E ... tidy data file is here:  ")
print (tidyDataFileOutTxt)
## the final tidy data file should be in:  UCI HAR Dataset/tidyData.txt
