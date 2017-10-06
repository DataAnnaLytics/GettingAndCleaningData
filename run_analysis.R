#load, merge, filter, tidy and summarize "messy" data about human mobility recognition
#thanks a lot to https://drive.google.com/file/d/0B1r70tGT37UxYzhNQWdXS19CN1U/view
run_analysis <- function(){
        
        #1 - Merge test and train data into one data set
        
                #define paths and filenames of required data
                inputfiles_test<- c("test/subject_test.txt", "test/y_test.txt", "test/X_test.txt") 
                inputfiles_train<-c("train/subject_train.txt", "train/y_train.txt", "train/X_train.txt")
                inputfiles_colnames<-c("features.txt") # column names for step 2
                inputfiles_activity_names<-c("activity_labels.txt") # activity labels for step 3
                
                #check if file "UCI HAR Dataset" is the working directory or a subfolder of the working directory
                if(!file.exists(inputfiles_train[1])){
                        inputfiles_test<-paste("UCI HAR Dataset/", inputfiles_test, sep="")
                        inputfiles_train<-paste("UCI HAR Dataset/", inputfiles_train, sep="")
                        inputfiles_colnames<-paste("UCI HAR Dataset/", inputfiles_colnames, sep="")
                        inputfiles_activity_names<-paste("UCI HAR Dataset/", inputfiles_activity_names, sep="")
                }
                
                #import data via read.table
                subject_train<-read.table(inputfiles_train[1], col.names = "Subject.ID")
                subject_test<-read.table(inputfiles_test[1], col.names = "Subject.ID")
                y_train<-read.table(inputfiles_train[2], col.names = "Activity.ID")
                y_test<-read.table(inputfiles_test[2], col.names = "Activity.ID")
                X_train<-read.table(inputfiles_train[3])
                X_test<-read.table(inputfiles_test[3]) 
                
                X_colnames<-read.table(inputfiles_colnames[1]) #Column names for X-data, see step 2
                activity_names<-read.table(inputfiles_activity_names[1], col.names = c("Activity.ID", "Activity")) #activity names for y-data, see step 3
                        # I prefer the Activity-labels to be lower case and without "_", so I change the labels right here
                        activity_names[,2] <- tolower(activity_names[, 2])
                        activity_names[,2] <- gsub("[_]", " " , activity_names[, 2])
                
                #merge data - columnwise
                train<-cbind(subject_train, y_train, X_train)
                test<-cbind(subject_test, y_test, X_test)
                
                #merge data - combine train and test data
                newdata<-rbind(train, test)
                        #newdata now contains ...
                        # - the subject-ID (1 to 30) of the participants in column 1
                        # - the activity-ID (1 to 6) for standing, walking etc. in column 2
                        # - a lot of feature-information in columns 3 to 563
                        # both for the training- and testing-group.
                
                #clear some unnecessary input data from workspace
                rm(train, test, subject_train ,subject_test ,y_train,y_test, X_train, X_test)
        
        #2 - "Extract only the measurements on the mean and standard deviation for each measurement" 
        # --> look for columns with "mean" and "std" in the column names
        
                #load dplyr
                library(dplyr)
                
                #look for column names containing "std" or "mean"; get back indices and add "2", as the first
                # two columns are "Subject.ID" and "Activity.ID"
                correct_cols<- grep("mean|std", X_colnames$V2 ,  ignore.case = TRUE)+2
                
                #select the columns with "mean" and "std" (plus the first two columns) in it
                newdata<-select(newdata, c(1,2,correct_cols))

                
        #3 "Use descriptive activity names to name the activities in the data set"
                #--> replace activity IDs by joining newdata and the activity labels
                
                newdata<-left_join(newdata, activity_names)
                #newdata now has a new column (to the very right) containing labels like "walking" etc. 
                
                #we don't need the activity-ID anymore and rearrange our data (leave out column 2, take the last column to the front)
                newdata<- select(newdata, 1, ncol(newdata), 3:(ncol(newdata)-1))
          
        #4 "Appropriately labels the data set with descriptive variable names"
                
                #the first two columns have appropriate names already. 
                #Let's get the labes of the other columns (by searching for "std" and "mean" again, but this time we get back
                # the values, not the indices)
                correct_cols<- grep("mean|std", X_colnames$V2, value  = TRUE,  ignore.case = TRUE)
                
                #let's replace "()", "_", "-" etc. by "." 
                correct_cols<-gsub("[(|)|_|-|,|-]", ".", correct_cols)
                #remove "..."
                correct_cols<-gsub("\\.*\\.", ".", correct_cols)
                #remove "." at the end of a column name
                correct_cols<-gsub("[.]$", "", correct_cols)
                #replace "BodyBody" by "Body"
                correct_cols<-gsub("[Bb]ody[Bb]ody", "Body", correct_cols)
                #replace "t" by "Time", "f" by "Freq"
                correct_cols<-gsub("^f", "Freq", correct_cols)
                correct_cols<-gsub("^t", "Time", correct_cols)
                
                #apply column names
                colnames(newdata)<-c("Subject.ID", "Activity", correct_cols)
                
                
        #5 "From the data set in step 4, creates a second, independent tidy data set 
        #   with the average of each variable for each activity and each subject."
                
                #group by and summarize
                newdata_mean<-group_by(newdata, Subject.ID, Activity)
                newdata_mean<-summarize_all(newdata_mean,mean, na.rm=TRUE)
                #finished - we now have 30*6 rows of a lot of means and stds
                
#        #optional: export result
#                write.table(newdata_mean, "TidyData.txt", row.name=FALSE)
                
       return(newdata_mean)     
}
        
