# GettingAndCleaningData

## My submission for week 4's assignment - "Tidy Data"


### Summary: load, merge, filter, tidy and summarize "messy" data about human mobility recognition

> You should create one R script called run_analysis.R that does the following...

> ...Merges the training and the test sets to create one data set.

Import all necessary data (train and test data, activity labels and feature labels) via read.table
Use rbind and cbind to merge data

> ...Extracts only the measurements on the mean and standard deviation for each measurement.

Find and select (dplyr) columns with "mean" and "std" in their names via grep

> ...Uses descriptive activity names to name the activities in the data set

Replace activity IDs by activity labels via join- and select-function

> ...Appropriately labels the data set with descriptive variable names.

Make sure "." is the only special character in the column names via gsub and regular expressions

> ...From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

use group_by and summarize_all
