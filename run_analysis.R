# Load used libraries
library(data.table)
library(dplyr)
library(tidyr)

# File Url
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Zip Folder Name
zipFolder <- "UCI HAR Dataset.zip"

# Folder Destination
destFile <- paste("./", zipFolder, sep = "")

# Downloading the data if not done yet
if (!file.exists(zipFolder)) {
    
    # File Download
    download.file(fileUrl, destfile=destFile, method = "curl")
    
}

# Unzipping folder if not done yet

if (!dir.exists(destFile)) {
    
    # Extract File from zip folder
    unzip(zipFolder)
    
}

# Load Data into R

features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activityId","activityLabel"))

# Training dataset

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

# Test dataset

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

# Integrate the three datasets

train <- cbind(subject_train, y_train, X_train)
test <- cbind(subject_test, y_test, X_test)

# 1. Merge the training and the test sets to create one data set.

dataset <- merge(train, test, all = TRUE)

# 4. Appropriately label the data set with descriptive variable names.

labels <- c("subject", "activityId", features[,2])

names(dataset) <- labels

# 2. Extract only the measurements on the mean and standard deviation for each measurement.

column_index <- grep("subject|activityId|mean\\(\\)|std\\(\\)",labels)

dataset <- dataset[,column_index]

# 3. Use descriptive activity names to name the activities in the data set.

dataset <- merge(dataset, activity_labels)

# 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydataset <- tbl_df(dataset) %>% 
    group_by(subject, activityLabel) %>%
    summarize_each(funs(mean)) %>%
    arrange(activityId)

# Save tidy dataset in txt format

write.table(tidydataset, file = "tidydataset.txt", row.name=FALSE)
