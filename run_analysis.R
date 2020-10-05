#---How to use?---

#- 1. Source this function--
#- 2. Find the data file location
#- 3. Input "Getting_and_Cleaning_Data('YOUT FILE LOCATION') in the console
#- 4. Example "Getting_and_Cleaning_Data("D:/Program Files/RStudio")"  (My data file in this directory)

Getting_and_Cleaning_Data <- function(x){
        
        file_dir <- x
        print("file_dir=")
        print(file_dir)
        
        setwd(file_dir)
        print("--------------------------------------------------------")
        print("Change work_dir to...")
        wd <- getwd()
        print(wd)
        print("--------------------------------------------------------")
        
        dir <- dir()
        if(length(which (dir == "getdata_projectfiles_UCI HAR Dataset")) !=1){
                print("Coun't find the file")
       }else{
        print("Start data cleaning...")
        
        
       #1.Merges the training and the test sets to create one data set.
        print("1.Merges the training and the test sets to create one data set..")
        x_train <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
        y_train <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
        subject_train <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
        x_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
        y_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
        subject_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
        features <- read.table('./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt')
        activityLabels <- read.table('./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt')
        
        colnames(x_train) <- features[,2]
        colnames(y_train) <-"activityId"
        colnames(subject_train)<- "subjectId"
        
        colnames(x_test) <- features[,2] 
        colnames(y_test) <- "activityId"
        colnames(subject_test) <- "subjectId"
        
        colnames(activityLabels) <- c('activityId','activityType')
        
        mrg_train <- cbind(y_train, subject_train, x_train)
        mrg_test <- cbind(y_test, subject_test, x_test)
        setAllInOne <- rbind(mrg_train, mrg_test)
        
        #2.Extracts only the measurements on the mean and standard deviation for each measurement.
        print("2.Extracts only the measurements on the mean and standard deviation for each measurement...")
        
        colNames <- colnames(setAllInOne)
        
        mean_and_std <- (grepl("activityId" , colNames) | 
                                 grepl("subjectId" , colNames) | 
                                 grepl("mean.." , colNames) | 
                                 grepl("std.." , colNames) 
        )
        
        setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]
        
        #3. Uses descriptive activity names to name the activities in the data set
        
        print("3. Uses descriptive activity names to name the activities in the data set...")
        
        setWithActivityNames <- merge(setForMeanAndStd, activityLabels,
                                      by='activityId',
                                      all.x=TRUE)
        
        print("4.Appropriately labels the data set with descriptive variable names...")
        
        print("5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject...")
        
        secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
        secTidySet <<- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]
        write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
        
               }
        
}