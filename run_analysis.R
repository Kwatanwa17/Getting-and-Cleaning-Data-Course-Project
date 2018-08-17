#Set the working directory where the dataset file is located

#Read activity labels and features
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

#Loading packages
library(dplyr)

#Test set
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features[,2])
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "activity_number")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
tag_test <- data.frame(tag = rep("test",length = nrow(subject_test)))
test_set <- data.frame(tag_test,subject_test, y_test, X_test)

#Trauin set
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features[,2])
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "activity_number")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
tag_train <- data.frame(tag = rep("train",length = nrow(subject_train)))
train_set <- data.frame(tag_train,subject_train, y_train, X_train)

#Merge test and training datas
mergeDF <- merge(test_set, train_set, all = TRUE)

#Adding activity labels
mergeDF <- mergeDF %>% 
  mutate(activity_name = factor(activity_number, 
                                levels = activity_labels[,1],
                                labels = activity_labels[,2]))

#Loading mesurements (Test set)
test_txt_list <- list.files(path = "./UCI HAR Dataset/test/Inertial Signals", full.name = TRUE)
test_txt_name <- list.files(path = "./UCI HAR Dataset/test/Inertial Signals", full.name = FALSE)
test_txt_name <- gsub(".txt$","",test_txt_name)
test_list <- tapply(test_txt_list, list(test_txt_name), read.table)

CalculateMeanByRow <- function(df) apply(df, 1, mean)
CalculateSDByRow <- function(df) apply(df, 1, sd)

#Calculating mean of each mesurement (Test set)
mean_test_matrix <- sapply(test_list, CalculateMeanByRow)
mean_test <- as.data.frame(mean_test_matrix)

#Calculating standard deviation of each mesurement (Test set)
sd_test_matrix <- sapply(test_list, CalculateSDByRow)
sd_test <- as.data.frame(sd_test_matrix)

#Loading mesurements (Train set)
train_txt_list <- list.files(path = "./UCI HAR Dataset/train/Inertial Signals", full.name = TRUE)
train_txt_name <- list.files(path = "./UCI HAR Dataset/train/Inertial Signals", full.name = FALSE)
train_txt_name <- gsub(".txt$","",train_txt_name)
train_list <- tapply(train_txt_list, list(train_txt_name), read.table)

#Calculating mean of each mesurement (Test set)
mean_train_matrix <- sapply(train_list, CalculateMeanByRow)
mean_train <- as.data.frame(mean_train_matrix)

#Calculating standard deviation of each mesurement (Test set)
sd_train_matrix <- sapply(train_list, CalculateSDByRow)
sd_train <- as.data.frame(sd_train_matrix)

#Binding all mesurements
mesurement_labels <- gsub("_test$","",test_txt_name)
mean_labels <- paste("mean_", mesurement_labels, sep = "")
sd_labels <- paste("sd_", mesurement_labels, sep = "")

colnames(mean_test) <- mean_labels
colnames(sd_test) <- sd_labels
colnames(mean_train) <- mean_labels
colnames(sd_train) <- sd_labels

mean_mesurements <- rbind(mean_test, mean_train)
sd_mesurements <- rbind(sd_test, sd_train)

#Create a dataset with all dataframes
dataset <- cbind(mergeDF,mean_mesurements,sd_mesurements)

#Tidy data set with the average of each variable for each activity and each subject
tidydataset <- dataset %>% 
  group_by(activity_name, subject) %>%
  summarise_if(is.numeric, funs(mean))

#Save the dataset as txt file 
write.table(tidydataset, file = "tidydataset.txt", row.names = FALSE)
