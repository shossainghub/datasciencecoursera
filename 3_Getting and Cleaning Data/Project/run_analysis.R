# Getting and Cleaning Data Project
# Author: Shahadat Hossain

# Objective:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



# Downloading data for the project

url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = url, destfile = "projectfile.zip")
unzip(zipfile = "projectfile.zip", overwrite = TRUE)


# Loading packages and data
library(tidyverse)
library(data.table)


# Loading activity labels and features

activitylabels <- fread(file.path("UCI HAR Dataset/activity_labels.txt"),
                        col.names = c("classLabels", "activityName"))
features <- fread(file.path("UCI HAR Dataset/features.txt"), 
                  col.names = c("index", "featureNames"))

featnames <- features %>% 
  mutate(rownumber = row_number()) %>%
  filter(grepl("(mean|std)\\(\\)", featureNames) == TRUE) %>% 
  mutate(featureNames = gsub("[()]", "", featureNames),
         featureNames = gsub("-", "_", featureNames))

# Loading training data

train <- fread(input = "UCI HAR Dataset/train/X_train.txt") %>% 
  select(c(featnames$rownumber)) # Selecting relevant columns

names(train) <- featnames$featureNames # Rename columns

trainActivities <- fread(input = "UCI HAR Dataset/train/Y_train.txt", 
                         col.names = c("Activity"))

trainSubjects <- fread(input = "UCI HAR Dataset/train/subject_train.txt",
                       col.names = c("SubjectNum"))

train <- cbind(trainSubjects, trainActivities, train)


# Loading test data

test <- fread(input = "UCI HAR Dataset/test/X_test.txt") %>% 
  select(c(featnames$rownumber)) # Selecting relevant columns

names(test) <- featnames$featureNames # Rename columns

testActivities <- fread(input = "UCI HAR Dataset/test/Y_test.txt",
                        col.names = c("Activity"))

testSubjects <- fread(input = "UCI HAR Dataset/test/subject_test.txt", 
                      col.names = c("SubjectNum"))

test <- cbind(testSubjects, testActivities, test)

# Merge datasets
combined <- rbind(train, test)

# Relebeling variable
# Making wide by 

combined <- combined %>% 
  mutate(Activity = factor(Activity, 
                           levels = activitylabels$classLabels,
                           labels = activitylabels$activityName),
         SubjectNum = as.factor(SubjectNum)) %>% 
  gather(variable, value, -c(Activity, SubjectNum)) %>% 
  group_by(Activity, SubjectNum, variable) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  spread(variable, value)

# Exporting to text
write.table(x = combined, file = "tidyData.txt", row.names = FALSE, quote = FALSE)