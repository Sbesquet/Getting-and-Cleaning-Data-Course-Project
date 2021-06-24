#Getting and Cleaning Data Course Project

getwd()

#1st set - "Test" set

activity_labels <- read.csv("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
features <- read.table("./UCI HAR Dataset/features.txt", header=FALSE)
# features

test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test_activity <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
test_data <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
names(test_data) <- features[,2]
names(test_subject) <- "Subject"
names(test_activity) <- "Activity"

test_dataframe = cbind(Subject = test_subject, Activity = test_activity, Data = test_data)
library(dplyr)
# class(test_dataframe)
test_dataframe$Group="Test"
test_dataframe <- test_dataframe %>% relocate(Group, .after = Subject)

test_dataframe$Activity <- gsub("1", "Walking", test_dataframe$Activity)
test_dataframe$Activity <- gsub("2", "Walking Upstairs", test_dataframe$Activity)
test_dataframe$Activity <- gsub("3", "Walking Downstairs", test_dataframe$Activity)
test_dataframe$Activity <- gsub("4", "Sitting", test_dataframe$Activity)
test_dataframe$Activity <- gsub("5", "Standing", test_dataframe$Activity)
test_dataframe$Activity <- gsub("6", "Laying", test_dataframe$Activity)

#2nd set - "Train" set

# activity_labels <- read.csv("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
# features <- read.table("./UCI HAR Dataset/features.txt", header=FALSE)


train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train_activity <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
train_data <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
names(train_data) <- features[,2]
names(train_subject) <- "Subject"
names(train_activity) <- "Activity"

train_dataframe = cbind(Subject = train_subject, Activity = train_activity, Data = train_data)
# class(train_dataframe)
train_dataframe$Group="Train"
train_dataframe <- train_dataframe %>% relocate(Group, .after = Subject)

train_dataframe$Activity <- gsub("1", "Walking", train_dataframe$Activity)
train_dataframe$Activity <- gsub("2", "Walking Upstairs", train_dataframe$Activity)
train_dataframe$Activity <- gsub("3", "Walking Downstairs", train_dataframe$Activity)
train_dataframe$Activity <- gsub("4", "Sitting", train_dataframe$Activity)
train_dataframe$Activity <- gsub("5", "Standing", train_dataframe$Activity)
train_dataframe$Activity <- gsub("6", "Laying", train_dataframe$Activity)

merged_df <- merge(test_dataframe,train_dataframe, all=TRUE)

# ?select
# contains std
selected_df <- select(merged_df, 1:3, contains("mean"), contains("std"))

f1 <- as.factor(selected_df$Activity)
f2 <- as.factor(selected_df$Subject)
# fcomb <- interaction(f1,f2)

names <- colnames(selected_df)
names <- names[-2:-3]
names[1] <- "Variable of Interest"


matsub <- data.frame(NA)
for(i in 4:89){
  a <- tapply(selected_df[,i], f2, mean) #*******
  matsub <- cbind(matsub,a)
}
matsub[,1] <- 1:30
names(matsub) <- names
for (i in 1:30){
  matsub[i,1] <- paste("Subject", i, sep = " ")  
}


matact <- data.frame(NA)
for(i in 4:89){
  a <- tapply(selected_df[,i], f1, mean) #*******
  matact <- cbind(matact,a)
}
matact[,1] <- c("Laying", "Sitting", "Standing", "Walking", "Walking Downstairs", "Walking Upstairs")
names(matact) <- names

# ?merge
merged_means <- merge(matsub, matact, all = TRUE)

for(i in 2:87) {
  names(merged_means)[i] <- paste("MEAN", names(merged_means[i]), sep = " ")
}

rm(list=setdiff(ls(), c("selected_df", "merged_means")))
#?setdiff

#rm(list=ls())
