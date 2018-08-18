## This file describes variables, datas and transformation of run_analysis.R

### Set the working directory where the dataset file is located

### Read activity labels and features

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

features <- read.table("./UCI HAR Dataset/features.txt")

- activity_labels: Labels for activities
- features: Labels for features

### Loading packages

library(dplyr)

### Test set

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features[,2])

y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "activity_number

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

tag_test <- data.frame(tag = rep("test",length = nrow(subject_test)))

test_set <- data.frame(tag_test,subject_test, y_test, X_test)

- X_test: Test set
- y_test: Test labels
- subject_test: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
- tag_test: Tag for test set
- test_set: data frame for test set

### Trauin set

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features[,2])

y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "activity_number")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

tag_train <- data.frame(tag = rep("train",length = nrow(subject_train)))

train_set <- data.frame(tag_train,subject_train, y_train, X_train)

- X_train: Train set
- y_train: Train labels
- subject_train: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
- tag_train: Tag for train set
- train_set: data frame for train set

### Merge test and training datas

mergeDF <- merge(test_set, train_set, all = TRUE)

### Adding activity labels

mergeDF <- mergeDF %>% 
  mutate(activity_name = factor(activity_number, 
                                levels = activity_labels[,1],
                                labels = activity_labels[,2]))

-mergeDF: data frame of test set and train set

### Loading mesurements (Test set)

test_txt_list <- list.files(path = "./UCI HAR Dataset/test/Inertial Signals", full.name = TRUE)

test_txt_name <- list.files(path = "./UCI HAR Dataset/test/Inertial Signals", full.name = FALSE)

test_txt_name <- gsub(".txt$","",test_txt_name)

test_list <- tapply(test_txt_list, list(test_txt_name), read.table)

- test_txt_list: File paths of test set mesurements
- test_txt_name: File names of test set mesurements
- test_list: List of all mesurements of test set

### Creating functions

CalculateMeanByRow <- function(df) apply(df, 1, mean)

CalculateSDByRow <- function(df) apply(df, 1, sd)

- CalculateMeanByRow: Function which calculates the mean of each column
- CalculateSDByRow: Function which calculates the sd of each column

### Calculating mean of each mesurement (Test set)

mean_test_matrix <- sapply(test_list, CalculateMeanByRow)

mean_test <- as.data.frame(mean_test_matrix)

- mean_test: data frame of the means of test set mesurements 

### Calculating standard deviation of each mesurement (Test set)

sd_test_matrix <- sapply(test_list, CalculateSDByRow)

sd_test <- as.data.frame(sd_test_matrix)

- sd_test: data frame of the sds of test set mesurements

### Loading mesurements (Train set)

train_txt_list <- list.files(path = "./UCI HAR Dataset/train/Inertial Signals", full.name = TRUE)

train_txt_name <- list.files(path = "./UCI HAR Dataset/train/Inertial Signals", full.name = FALSE)

train_txt_name <- gsub(".txt$","",train_txt_name)

train_list <- tapply(train_txt_list, list(train_txt_name), read.table)

- train_txt_list: File paths of train set mesurements
- train_txt_name: File names of train set mesurements
- train_list: List of all mesurements of train set

### Calculating mean of each mesurement (Train set)

mean_train_matrix <- sapply(train_list, CalculateMeanByRow)

mean_train <- as.data.frame(mean_train_matrix)

- mean_train: data frame of the means of train set mesurements 

### Calculating standard deviation of each mesurement (Train set)

sd_train_matrix <- sapply(train_list, CalculateSDByRow)

sd_train <- as.data.frame(sd_train_matrix)

- sd_train: data frame of the sds of train set mesurements

### Binding all mesurements

Creating lables for mesurements

mesurement_labels <- gsub("_test$","",test_txt_name)

mean_labels <- paste("mean_", mesurement_labels, sep = "")

sd_labels <- paste("sd_", mesurement_labels, sep = "")

### Adding names to mesurements 

colnames(mean_test) <- mean_labels

colnames(sd_test) <- sd_labels

colnames(mean_train) <- mean_labels

colnames(sd_train) <- sd_labels

### Binding mean and sd data set

mean_mesurements <- rbind(mean_test, mean_train)

sd_mesurements <- rbind(sd_test, sd_train)

- mean_mesurements: Data set of the means of all mesurements from test and train set
- sd_mesurements: Data set of the sds of all mesurements from test and train set

### Create a dataset with all dataframes

dataset <- cbind(mergeDF,mean_mesurements,sd_mesurements)

- dataset: Complete data set

### Tidy data set with the average of each variable for each activity and each subject

tidydataset <- dataset %>% 
  group_by(activity_name, subject) %>%
  summarise_if(is.numeric, funs(mean))
  
- tidydataset: Tidy data set according to the average of each activity and each subject. More information see README.md

### Save the dataset as txt file 

write.table(tidydataset, file = "tidydataset.txt", row.names = FALSE)


