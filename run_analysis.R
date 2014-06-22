

library("data.table", lib.loc="C:/Program Files/R/R-3.0.2/library")
library("plyr", lib.loc="C:/Program Files/R/R-3.0.2/library")

##########################################################################################
##
## Funtion that locates the file to be read
##
##########################################################################################

getHARFileLocation <- function (HARfileToRead) { 

        current_wd <- getwd()
                
        if (HARfileToRead == "activity_labels.txt") {
                
            filePath <- "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/"
            
        } 
        else if (HARfileToRead == "features.txt") {
              
            filePath <- "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/"
                
        }
        else if (HARfileToRead == "X_test.txt") {
                
            filePath <- "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/"
                
        }
        else if (HARfileToRead == "y_test.txt") {
                
            filePath <- "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/"
                
        }
        else if (HARfileToRead == "subject_test.txt") {
                
            filePath <- "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/"
                
        }
        else if (HARfileToRead == "X_train.txt") {
                
                filePath <- "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/"
                
        }
        else if (HARfileToRead == "y_train.txt") {
                
                filePath <- "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/"
                
        }
        else if (HARfileToRead == "subject_train.txt") {
                
                filePath <- "/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/"
                
        }
        
        resultingFileLocation<-paste(c(current_wd,filePath,HARfileToRead),collapse="")
        
}


##########################################################################################
##
## Funtion that reads the file and loads data into table object
##
##########################################################################################


readHARFile <- function (HARfileToRead,HARfileLocation) {
        
        
        if (HARfileToRead == "activity_labels.txt") {
                
                resultingTable <- read.table(HARfileLocation,sep=" ")                
                
        } 
        else if (HARfileToRead == "features.txt") {
                
                resultingTable <- read.table(HARfileLocation,sep=" ")
                
        }
        else if (HARfileToRead == "X_test.txt") {
                
                resultingTable <- read.table(HARfileLocation,sep="",quote="\'")
                
        }
        else if (HARfileToRead == "y_test.txt") {
                
                resultingTable <- read.table(HARfileLocation,sep="")
                
        }
        else if (HARfileToRead == "subject_test.txt") {
                
                resultingTable <- read.table(HARfileLocation,sep=" ")
                
        }
        else if (HARfileToRead == "X_train.txt") {
                
                resultingTable <- read.table(HARfileLocation,sep="",quote="\'")
                
        }
        else if (HARfileToRead == "y_train.txt") {
                
                resultingTable <- read.table(HARfileLocation,sep="")
                
        }
        else if (HARfileToRead == "subject_train.txt") {
                
                resultingTable <- read.table(HARfileLocation,sep=" ")
                
        }
        

}



##########################################################################################
##
## Main funtion. Generates Tidy data
##
##########################################################################################


generate_tidy_data <- function () {
        
        
        activity_labels_fileLocation <- getHARFileLocation("activity_labels.txt")
        print(paste("Reading file: ",activity_labels_fileLocation))
        activity_labels_table <- readHARFile("activity_labels.txt",activity_labels_fileLocation)

        features_fileLocation <- getHARFileLocation("features.txt")
        print(paste("Reading file: ",features_fileLocation))
        features_table <- readHARFile("features.txt",features_fileLocation)
        
        X_test_fileLocation <- getHARFileLocation("X_test.txt")
        print(paste("Reading file: ",X_test_fileLocation))
        X_test_table <- readHARFile("X_test.txt",X_test_fileLocation)
        
        y_test_fileLocation <- getHARFileLocation("y_test.txt")
        print(paste("Reading file: ",y_test_fileLocation))
        y_test_table <- readHARFile("y_test.txt",y_test_fileLocation)
        
        subject_test_fileLocation <- getHARFileLocation("subject_test.txt")
        print(paste("Reading file: ",subject_test_fileLocation))
        subject_test_table <- readHARFile("subject_test.txt",subject_test_fileLocation)
        
        X_train_fileLocation <- getHARFileLocation("X_train.txt")
        print(paste("Reading file: ",X_train_fileLocation))
        X_train_table <- readHARFile("X_train.txt",X_train_fileLocation)
        
        y_train_fileLocation <- getHARFileLocation("y_train.txt")
        print(paste("Reading file: ",y_train_fileLocation))
        y_train_table <- readHARFile("y_train.txt",y_train_fileLocation)
        
        subject_train_fileLocation <- getHARFileLocation("subject_train.txt")
        print(paste("Reading file: ",subject_train_fileLocation))
        subject_train_table <- readHARFile("subject_train.txt",subject_train_fileLocation)
        
         


        ## Append subject and activity columns to measurements 
        test_table <- cbind(X_test_table,V562=subject_test_table$V1)
        test_table <- cbind(test_table,V563=y_test_table$V1)
        
        train_table <- cbind(X_train_table,V562=subject_train_table$V1)
        train_table <- cbind(train_table,V563=y_train_table$V1)
      

        ## 1.Merges the training and the test sets to create one data set.
        merged_table <- rbind(test_table,train_table)
        
        
        ## Create vector with variable names for merged data
        dtSubject <- data.table(V1=562,V2="Subject")
        dtActivity <- data.table(V1=563,V2="Activity")
        features_table <- rbind(features_table,dtSubject)
        features_table <- rbind(features_table,dtActivity)

        ## Create a logic vector to use as index for colum selection
        selectMeancolumnnames <- grepl("*[Mm][e][a][n]*", features_table$V2)
        selectStdcolumnnames <- grepl("*[s][t][d]*", features_table$V2)
        selectSubjectcolumnnames <- grepl("*[S][u][b][j]*", features_table$V2)
        selectActivitycolumnnames <- grepl("*[A][c][t][i][v]*", features_table$V2)
        
        selectedcolumns <- selectMeancolumnnames | selectStdcolumnnames | selectSubjectcolumnnames | selectActivitycolumnnames
                
        ## 4.Appropriately labels the data set with descriptive variable names. 
        colnames(merged_table) <- features_table$V2

        ## 2.Extracts only the measurements on the mean and standard deviation for each measurement.
        merged_table_mean_std <- merged_table[,selectedcolumns]
        
        ## Add column names to activity_labels_table
        colnames(activity_labels_table) <- c("Activity","ActivityLabel")
        
        ## 3.Uses descriptive activity names to name the activities in the data set
        merged_table_with_activity_names <- merge(merged_table_mean_std,activity_labels_table,by.x="Activity",by.y="Activity")
        
        ## Write tidy file 1
        write.table(merged_table_with_activity_names,"tidyfile1.txt",sep=" ",quote=FALSE,row.names=FALSE)     

        ## Create tidy file 2 datatable
        tidyfile2_datatable <- ddply(merged_table_with_activity_names,.(Activity,Subject),summarize,Average=ave(c(2:87),FUN=ave))
        
        
        ## 5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
        write.table(tidyfile2_datatable,"tidyfile2.txt",sep=" ",quote=FALSE,row.names=FALSE)
        
        
}

