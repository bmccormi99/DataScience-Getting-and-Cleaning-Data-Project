# Getting and Cleaning Data Course Project
## README.MD
### These are the steps necessary to execute the R script to complete the assignment for the course project.
##  
**This assumes you can download the source zip file, place it in a folder, unzip and follow these instructions. These instructions were performed on Win8 and Win10 machines.  Those with other operating systems are assumed to be able to make the appropriate adjustments.**

    1.  Download the source data from this URL into a folder on your local drive, 
        For example:  "C:\Users\yourname\Documents\R\"
<http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip> 
    
    2.  When you unzip, it will create a folder structure called "UCI HAR Dataset" 
        under this .\R Folder
    
    3.  Put run_analysis.R into this same .\R folder as step 1. 
    
    4.  From the R Console:
    
```{r}
          setwd("C:\Users\yourname\Documents\R\")
          source("run_Analysis.R")
```
    5.  This will run the R script, complete the 5 steps of the project, eventually 
        writing a file called:  ./UCI HAR Dataset/tidyData.txt
        
I decided to make it a tab delimited txt file - many in my circles would import this into excel to test and tab delimited works well for this.

The output file will the results of a data frame with 180 rows and 68 columns.  The script took about 30 to execute on my machine - your actual results may vary.    
 
**To learn more about the project assignment, the dataset used, and the necessary steps to complete, please see the CODEBOOK.MD file.**