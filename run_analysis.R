#This is the code for the final programming project of
#Data Gathering and Cleaning 

#Below is the code called run_analysis.R
#it has the following functions
#   1. Merges the training and test sets to create one data set
#   2. Extracts only the measurements on the mean and standard deviation 
#      for each measurement
#   3. Uses descriptive activity names to name the activities in the dataset
#   4. Appropriately labels the data set with descriptive variable names
#   5. From the data set in step 4, creates a second, independent tidy dataset
#      with the average of each variable for each activity and each subject

#set the working directory to the downloaded folder
#of UCI HAR Dataset
setwd ("/Users/FinalProgrammingProject/UCI HAR Dataset")

#load dyplyr
library (dplyr)

#read features.txt for the variable names
names <- read.table ("features.txt")

#set the wd to the test folder within the UCI folder
dir <- getwd()
new_dir <- file.path (dir, "test")
setwd (new_dir)

#read the test measurement,activity label, and subject label 
#assign the columns with their appropriate names
test_res <- read.table ("X_test.txt", col.names = names [,2] )
test_act <- read.table ("y_test.txt", col.names = "activity")
test_sub <- read.table ("subject_test.txt", col.names = "subject")

#bind the test vectors and df into one df (test_data)
test_data <- cbind (test_sub, test_act, test_res)

#set the wd to the train folder within the UCI folder
new_dir <- file.path (dir, "train")
setwd (new_dir)

#read the train measurement,activity label, and subject label 
#assign the columns with their appropriate names
train_res <- read.table ("X_train.txt", col.names = names [,2])
train_act <- read.table ("y_train.txt", col.names = "activity")
train_sub <- read.table ("subject_train.txt", col.names = "subject")

#bind the train vectors and df into one df (train_data)
train_data <- cbind (train_sub, train_act, train_res)

#merge the the two dfs (test_data & train_data) into one df (join)
join <- merge (test_data, train_data, 
               intersect (names(test_data), names (train_data)), 
               all = TRUE)

#Extract the col.s containing the mean and std of each measurement
#identify the wanted col.s by their names
#save the logical result in a variable "good"
good <- grepl (pattern = "mean[[:punct:]]{2}|std",
                 names (join))

#extract the col.s with mean and std together with the first two
#col.s of df join
#convert the activity col. from integer to character, in preparation
#for replacing the labels with verbal descriptions
ext <- join [,1:2]
ext <- bind_cols (ext, join [,good])
ext$activity<- as.character(ext$activity)

  
#replacing the numeric labels of activities with  descriptive names 
for (i in 1:nrow (ext)) {
  
  if (ext$activity [i] == "1") {
    ext$activity[i] <- sub ("1", "Walking", ext$activity[i])
  }
  
  else if (ext$activity [i] == "2") {
    ext$activity[i] <- sub ("2", "Walking_up", ext$activity[i])
  }
  
  else if (ext$activity [i] == "3") {
    ext$activity[i] <- sub ("3", "Walking_down", ext$activity[i])
  }
  
  else if (ext$activity [i] == "4") {
    ext$activity[i] <- sub ("4", "Sitting", ext$activity[i])
  }
  
  else if (ext$activity [i] == "5") {
    ext$activity[i] <- sub ("5", "Standing", ext$activity[i])
  }
  
  else {
    ext$activity[i] <- sub ("6", "Laying", ext$activity[i])
  }
}

#Appropriately labels the data set with descriptive variable names
#by taking out the redundant dots/parenthesis

colnames (ext) <- gsub (pattern = "[[:punct:]]{2}$", 
                        replacement = "",
                        colnames (ext))

colnames (ext) <- gsub (pattern = "[[:punct:]]{3}",
                        replacement = "",
                        colnames (ext))

#From the extracted df ext, create a second, independent tidy df, avg_ext
#the new df contains the average of each variable for each activity and each subject

ext %>%
  group_by (activity, subject) %>%
  summarize_at (vars(3:66), mean, na.rm= TRUE) -> avg_ext

#change the col. names of the new df avg_ext with more descriptive names
colnames (avg_ext) [3:66] <- sapply (names (avg_ext) [3:66], 
                                     function (x) {paste0 ("avg_", x)})

#reset the wd and save the output in output.txt
#under the UCI folder
setwd ("/Users/FinalProgrammingProject/UCI HAR Dataset")
write.table(avg_ext, "output.txt", row.names = FALSE)