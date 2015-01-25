# read and set different text files from train and test folders into variables 
# in order to test my R code. please replace THE work folder below with the set folder of your computer  
setwd("C:/Users/TAHALILE/Desktop/Coursera/cleaning data/UCI HAR Dataset")
# read all train files x,y, subject from respective files
train_HB = read.table("C:/Users/TAHALILE/Desktop/Coursera/cleaning data/UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
train_HB[,562] =  read.table("C:/Users/TAHALILE/Desktop/Coursera/cleaning data/UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
train_HB[,563] =  read.table("C:/Users/TAHALILE/Desktop/Coursera/cleaning data/UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

#  read all test files x,y, subject from respectve files
test_HB =  read.table("C:/Users/TAHALILE/Desktop/Coursera/cleaning data/UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
test_HB[,562] =  read.table("C:/Users/TAHALILE/Desktop/Coursera/cleaning data/UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
test_HB[,563] =  read.table("C:/Users/TAHALILE/Desktop/Coursera/cleaning data/UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

# read activity lables from activity_labels.txt file
activityLbl_HB = read.table("C:/Users/TAHALILE/Desktop/Coursera/cleaning data/UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# Read features from features.txt file
feat_HB =  read.table("C:/Users/TAHALILE/Desktop/Coursera/cleaning data/UCI HAR Dataset/features.txt", sep="", header=FALSE)
# set means, std deviation 
feat_HB[,2] = gsub('-mean', 'Mean', feat_HB[,2])
feat_HB[,2] = gsub('-std', 'Std', feat_HB[,2])
feat_HB[,2] = gsub('[-()]', '', feat_HB[,2])

# 1- Merges the training and the test sets to create one data set.
MergeData = rbind(train_HB, test_HB)

# 2- Extracts only the measurements on the mean and standard deviation for each measurement. 
Get_M_ST_DEV <- grep(".*Mean.*|.*Std.*", feat_HB[,2])
# reducing features Data
feat_HB <- feat_HB[Get_M_ST_DEV,]
# Add the columns subject , activity
Get_M_ST_DEV <- c(Get_M_ST_DEV, 562, 563)
# remove the unwanted columns from MergeData
MergeData <- MergeData[,Get_M_ST_DEV]
# Add the column names (features) to MergeData
colnames(MergeData) <- c(feat_HB$V2, "Activity", "Subject")
colnames(MergeData) <- tolower(colnames(MergeData))

# Uses descriptive activity names to name the activities in the data set
ActivityVariable = 1
for (currentActivityLabel in activityLbl_HB$V2) {
        MergeData$activity <- gsub(ActivityVariable, currentActivityLabel, MergeData$activity)
        ActivityVariable <- ActivityVariable + 1
}

MergeData$activity <- as.factor(MergeData$activity)
MergeData$subject <- as.factor(MergeData$subject)

# 4- Appropriately labels the data set with descriptive variable names.
Tidyset = aggregate(MergeData, by=list(activity = MergeData$activity, subject=MergeData$subject), mean)

# Remove the subject and activity columns
Tidyset[,90] = NULL
Tidyset[,89] = NULL

# 5- from the data set in step 4, creates a second, independent tidy data set with the average of each variable
# for each activity and each subject.

write.table(Tidyset, "tidy_data_set.txt", sep="\t",row.name=FALSE)


