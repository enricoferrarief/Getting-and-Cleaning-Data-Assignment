# Data

The dataset used to perform this analysis contains experiments that have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

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

Notes: 

- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

# Cleaning performed on the dataset

After loading the required libraries such as data.table, dplyr and tidyr, the following datasets are loaded into R:
- features.txt
- activity_labels.txt
- y_train.txt
- X_train.txt
- subject_train.txt
- y_test.txt
- X_test.txt
- subject_test.txt

The script then performs the following changes to the dataset:

1. Merges the training and the test sets to create one data set. -> First y, X and subject are binded into one single dataframe and then the test and train dataset are merged together
2. Extracts only the measurements on the mean and standard deviation for each measurement. -> Using a regular expression, the labels containing the strings we need are indexed
3. Uses descriptive activity names to name the activities in the data set. -> The activity names are then merged by using the activity ids as keys
4. Appropriately labels the data set with descriptive variable names. -> The labels are assigned to the dataset as column names
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. -> The data is grouped by subject and activity and then all column means are calculated




