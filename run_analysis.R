#Retrieve data for project and define working directory.

#Start removing all variables
rm(list=ls())

Cwd = getwd()

pro_Folder <- "Course project"

dir.create(file.path(Cwd, pro_Folder), showWarnings = FALSE)

wd <- paste(Cwd,pro_Folder, sep="/")

setwd(wd)


download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "samsung.zip")

list.files()

#lets see what is inside the zip file and store it as variable a
a=unzip("samsung.zip")

a

list.files()

# saving all the features or the names of measurements. There are 561 measurements 
fet <- read.table("./UCI HAR Dataset/features.txt")
head(fet)
dim(fet)

# Saving all the activity labels.
act_lab <- read.table("./UCI HAR Dataset/activity_labels.txt")
#Lets name the activity label 
colnames(act_lab)[1]<-"id"
colnames(act_lab)[2]<-"Activity"

# Know lets construct the data set 
# Read the measurements for the test set to a variable
data_test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")

# Read the activity label for the test set 
data_test_y <- read.table("./UCI HAR Dataset/test/y_test.txt")

# Read the subject_number for the test set
data_test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")


# lets explore the data 
dim(data_test_x)
dim(data_test_y)
dim(data_test_subject)
# all these data has 2947 rows so that is what we need to add to gather i.e the columns 
# for the data set for activity labels and the subject numer we also need to name the columns of the 
# measurementst start by doing that. 

names(data_test_x) <- fet$V2

# lest see if that worked 

head(data_test_x[,1:4])

# lets make index of all the data set to see when we join that each row was correct. 

data_test_x$row_index <- 1:dim(data_test_x[1])
data_test_y$row_index_y <- 1:dim(data_test_x[1])
data_test_subject$row_index_s <- 1:dim(data_test_x[1])

# lets see if that worked 
head(data_test_x[,560:562])
head(data_test_y)
head(data_test_subject)

#lets name the columns in y and subject 

colnames(data_test_y)[1]<-"Training_label"
colnames(data_test_subject)[1]<-"subject_number"

#Did it work 
head(data_test_subject)
head(data_test_y)

# Now lets only select columns we care about in the x data according to project
# told to use mean and std columns and therefore we are using the grep function to find where mean or std are
# all column that have the string mean(),std() or row_index that I names the index earlier 
# by using grep function I am allowing me te flexibility to not have to deatiled so we also   
# take columns that are named meanFreq()

col_index_test <- grep("mean()|std()|row_index",names(data_test_x))

# lets see what column names are taken
names(data_test_x[,grep("mean()|std()|row_index",names(data_test_x))])

# only select these columns
data_test_x2 <- data_test_x[,grep("mean()|std()|row_index",names(data_test_x))]

#lets column bind the data togheter 

data_test <- cbind(data_test_subject,data_test_y, data_test_x2)

#lets check all the columns we have now 
names(data_test)

# see all the row index and if the connected correctly 
subset(data_test, select=c("row_index","row_index_y","row_index_s"))

#test data ready before merging.

#------------------------------------------------------
#lest do the same as above for the training data set.
#------------------------------------------------------
# Read the activity label for the training set

data_train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
data_train_y <- read.table("./UCI HAR Dataset/train/y_train.txt")
data_train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")

names(data_train_x) <- fet$V2

data_train_x$row_index <- 1:dim(data_train_x[1])
data_train_y$row_index_y <- 1:dim(data_train_x[1])
data_train_subject$row_index_s <- 1:dim(data_train_x[1])

colnames(data_train_y)[1]<-"Training_label"
colnames(data_train_subject)[1]<-"subject_number"

col_index_train <- grep("mean()|std()|row_index",names(data_train_x))
names(data_train_x[,grep("mean()|std()|row_index",names(data_train_x))])

data_train_x2 <- data_train_x[,grep("mean()|std()|row_index",names(data_train_x))]
data_train <- cbind(data_train_subject,data_train_y, data_train_x2)
subset(data_train, select=c("row_index","row_index_y","row_index_s"))

#now lets define the source of the data

data_train$source <- "training"
data_test$source <- "test"

dim(data_train)
dim(data_test)

#------------------------------------------------------
#lest merge the 2 data sets together.
#------------------------------------------------------


all_data <- rbind(data_train,data_test)

#Lets rearrange the data

names(all_data)

all_data <- all_data[,c(84,85,1,3,5:83)]

#lets merge the activity name to the date and reselect the data

all_data_m <- merge(all_data,act_lab, by.x = "Training_label", by.y = "id", all = TRUE)

# rearrange the data

names(all_data_m)

#lets fix nams of the columns for the measurments variable.

new_names <- sub("()-Z","_Z",names(all_data_m))
new_names <- sub("()-X","_X",new_names)
new_names <- sub("()-Y","_Y",new_names)
new_names <- sub("-","_",new_names)
new_names <- sub(")","",new_names)
new_names <- sub("\\(","",new_names)

colnames(all_data_m) <- new_names

#rearrange the data 

all_data_m <- all_data_m[,c(2,1,84,4,3,5:83)]

# lest ses if the data is correct
all_data[all_data$row_index == 1,1:6]
all_data_m[all_data_m$row_index == 1,1:6]

# order the data based on variables if wanted and look at the data demos

head(all_data_m[order(all_data_m$subject_number),1:7])
tail(all_data_m[order(all_data_m$subject_number),1:7])
head(all_data_m[order(all_data_m$source,all_data_m$Training_label,all_data_m$row_index),1:7])

# Know calculate and create a new data set based on average of the measured columns that is tidy

#--------------------------
# create new tidy data.
#------------------------------

new_td_data <- all_data_m[,c(3,4,6:84)]

#lets look at the data.
head(new_td_data[,1:6])

#install.packages("data.table")
#library("data.table")

# adding library to use ddply.
library(reshape)
library(reshape2)
library(plyr)

# lest write the names to csv file to manipulate it in notepad++ for quicker use of ddply functions.
nam<- names(new_td_data)

write.csv(nam, file = "names.csv")

#summarize the data according to instructions and make tidy data set to upload to coursera project website.
tidy_data<- ddply(new_td_data, .(Activity, subject_number), summarise,
                  tBodyAcc_mean_X = mean(tBodyAcc_mean_X),
                  tBodyAcc_mean_Y = mean(tBodyAcc_mean_Y),
                  tBodyAcc_mean_Z = mean(tBodyAcc_mean_Z),
                  tBodyAcc_std_X = mean(tBodyAcc_std_X),
                  tBodyAcc_std_Y = mean(tBodyAcc_std_Y),
                  tBodyAcc_std_Z = mean(tBodyAcc_std_Z),
                  tGravityAcc_mean_X = mean(tGravityAcc_mean_X),
                  tGravityAcc_mean_Y = mean(tGravityAcc_mean_Y),
                  tGravityAcc_mean_Z = mean(tGravityAcc_mean_Z),
                  tGravityAcc_std_X = mean(tGravityAcc_std_X),
                  tGravityAcc_std_Y = mean(tGravityAcc_std_Y),
                  tGravityAcc_std_Z = mean(tGravityAcc_std_Z),
                  tBodyAccJerk_mean_X = mean(tBodyAccJerk_mean_X),
                  tBodyAccJerk_mean_Y = mean(tBodyAccJerk_mean_Y),
                  tBodyAccJerk_mean_Z = mean(tBodyAccJerk_mean_Z),
                  tBodyAccJerk_std_X = mean(tBodyAccJerk_std_X),
                  tBodyAccJerk_std_Y = mean(tBodyAccJerk_std_Y),
                  tBodyAccJerk_std_Z = mean(tBodyAccJerk_std_Z),
                  tBodyGyro_mean_X = mean(tBodyGyro_mean_X),
                  tBodyGyro_mean_Y = mean(tBodyGyro_mean_Y),
                  tBodyGyro_mean_Z = mean(tBodyGyro_mean_Z),
                  tBodyGyro_std_X = mean(tBodyGyro_std_X),
                  tBodyGyro_std_Y = mean(tBodyGyro_std_Y),
                  tBodyGyro_std_Z = mean(tBodyGyro_std_Z),
                  tBodyGyroJerk_mean_X = mean(tBodyGyroJerk_mean_X),
                  tBodyGyroJerk_mean_Y = mean(tBodyGyroJerk_mean_Y),
                  tBodyGyroJerk_mean_Z = mean(tBodyGyroJerk_mean_Z),
                  tBodyGyroJerk_std_X = mean(tBodyGyroJerk_std_X),
                  tBodyGyroJerk_std_Y = mean(tBodyGyroJerk_std_Y),
                  tBodyGyroJerk_std_Z = mean(tBodyGyroJerk_std_Z),
                  tBodyAccMag_mean = mean(tBodyAccMag_mean),
                  tBodyAccMag_std = mean(tBodyAccMag_std),
                  tGravityAccMag_mean = mean(tGravityAccMag_mean),
                  tGravityAccMag_std = mean(tGravityAccMag_std),
                  tBodyAccJerkMag_mean = mean(tBodyAccJerkMag_mean),
                  tBodyAccJerkMag_std = mean(tBodyAccJerkMag_std),
                  tBodyGyroMag_mean = mean(tBodyGyroMag_mean),
                  tBodyGyroMag_std = mean(tBodyGyroMag_std),
                  tBodyGyroJerkMag_mean = mean(tBodyGyroJerkMag_mean),
                  tBodyGyroJerkMag_std = mean(tBodyGyroJerkMag_std),
                  fBodyAcc_mean_X = mean(fBodyAcc_mean_X),
                  fBodyAcc_mean_Y = mean(fBodyAcc_mean_Y),
                  fBodyAcc_mean_Z = mean(fBodyAcc_mean_Z),
                  fBodyAcc_std_X = mean(fBodyAcc_std_X),
                  fBodyAcc_std_Y = mean(fBodyAcc_std_Y),
                  fBodyAcc_std_Z = mean(fBodyAcc_std_Z),
                  fBodyAcc_meanFreq_X = mean(fBodyAcc_meanFreq_X),
                  fBodyAcc_meanFreq_Y = mean(fBodyAcc_meanFreq_Y),
                  fBodyAcc_meanFreq_Z = mean(fBodyAcc_meanFreq_Z),
                  fBodyAccJerk_mean_X = mean(fBodyAccJerk_mean_X),
                  fBodyAccJerk_mean_Y = mean(fBodyAccJerk_mean_Y),
                  fBodyAccJerk_mean_Z = mean(fBodyAccJerk_mean_Z),
                  fBodyAccJerk_std_X = mean(fBodyAccJerk_std_X),
                  fBodyAccJerk_std_Y = mean(fBodyAccJerk_std_Y),
                  fBodyAccJerk_std_Z = mean(fBodyAccJerk_std_Z),
                  fBodyAccJerk_meanFreq_X = mean(fBodyAccJerk_meanFreq_X),
                  fBodyAccJerk_meanFreq_Y = mean(fBodyAccJerk_meanFreq_Y),
                  fBodyAccJerk_meanFreq_Z = mean(fBodyAccJerk_meanFreq_Z),
                  fBodyGyro_mean_X = mean(fBodyGyro_mean_X),
                  fBodyGyro_mean_Y = mean(fBodyGyro_mean_Y),
                  fBodyGyro_mean_Z = mean(fBodyGyro_mean_Z),
                  fBodyGyro_std_X = mean(fBodyGyro_std_X),
                  fBodyGyro_std_Y = mean(fBodyGyro_std_Y),
                  fBodyGyro_std_Z = mean(fBodyGyro_std_Z),
                  fBodyGyro_meanFreq_X = mean(fBodyGyro_meanFreq_X),
                  fBodyGyro_meanFreq_Y = mean(fBodyGyro_meanFreq_Y),
                  fBodyGyro_meanFreq_Z = mean(fBodyGyro_meanFreq_Z),
                  fBodyAccMag_mean = mean(fBodyAccMag_mean),
                  fBodyAccMag_std = mean(fBodyAccMag_std),
                  fBodyAccMag_meanFreq = mean(fBodyAccMag_meanFreq),
                  fBodyBodyAccJerkMag_mean = mean(fBodyBodyAccJerkMag_mean),
                  fBodyBodyAccJerkMag_std = mean(fBodyBodyAccJerkMag_std),
                  fBodyBodyAccJerkMag_meanFreq = mean(fBodyBodyAccJerkMag_meanFreq),
                  fBodyBodyGyroMag_mean = mean(fBodyBodyGyroMag_mean),
                  fBodyBodyGyroMag_std = mean(fBodyBodyGyroMag_std),
                  fBodyBodyGyroMag_meanFreq = mean(fBodyBodyGyroMag_meanFreq),
                  fBodyBodyGyroJerkMag_mean = mean(fBodyBodyGyroJerkMag_mean),
                  fBodyBodyGyroJerkMag_std = mean(fBodyBodyGyroJerkMag_std),
                  fBodyBodyGyroJerkMag_meanFreq = mean(fBodyBodyGyroJerkMag_meanFreq)
)


# save the file to upload to coursea as a txt file. 

write.table(tidy_data, file = "course_project.txt", row.names = FALSE )