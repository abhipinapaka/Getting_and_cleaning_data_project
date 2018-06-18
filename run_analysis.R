rm(list =l )
setwd("F:/R practice/Coursera/Getting Cleaner data/project")

# Load Packages and get the Data
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
#Unzip file
unzip(zipfile = "dataFiles.zip")
#Get the list of the files
files_path<-file.path(path,"UCI HAR Dataset" )
files<- list.files(files_path, recursive = TRUE)
files

#Read data from the files into the variables

#Read the Activity files
activity_test  <- read.table(file.path(files_path, "test" , "Y_test.txt" ),header = FALSE)
activity_train <- read.table(file.path(files_path, "train", "Y_train.txt"),header = FALSE)

#Read the Subject files
SubjectTrain <- read.table(file.path(files_path, "train", "subject_train.txt"),header = FALSE)
SubjectTest  <- read.table(file.path(files_path, "test" , "subject_test.txt"),header = FALSE)

#Read Fearures files
FeaturesTest  <- read.table(file.path(files_path, "test" , "X_test.txt" ),header = FALSE)
FeaturesTrain <- read.table(file.path(files_path, "train", "X_train.txt"),header = FALSE)

#Properties of all files
str(activity_test)
str(activity_train)
str(SubjectTrain)
str(SubjectTest)
str(FeaturesTest)
str(FeaturesTrain)

#Merge the training and the test sets to create one data set

#Concatenate the data tables by rows
dataSubject <- rbind(SubjectTrain, SubjectTest)
dataActivity<- rbind(activity_train, activity_test)
dataFeatures<- rbind(FeaturesTrain, FeaturesTest)

#set names to variables
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(files_path, "features.txt"),head=FALSE)
names(dataFeatures)<- dataFeaturesNames$V2

########       Q1      ########
#Merge columns to get the data frame Data for all data
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)

#######        Q2        #######
#Extracts only the measurements on the mean and standard deviation for each measurement

features <- read.table("./UCI HAR Dataset/features.txt")
dim(features)  # 561*2
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
joinData <- joinData[, meanStdIndices]
dim(joinData) # 10299*66
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 


########       Q3      ########
#Uses descriptive activity names to name the activities in the data set

activityLabels <- read.table(file.path(files_path, "activity_labels.txt"),header = FALSE)
activityLabels
#facorize Variale activity in the data frame Data using descriptive activity names
head(Data$activity,30)

########       Q4      ###########
#Appropriately labels the data set with descriptive variable names

#In the former part, variables activity and subject and names of the activities have been 
#labelled using descriptive names.In this part, 
#Names of Feteatures will labelled using descriptive variable names.

##prefix t is replaced by time
##Acc is replaced by Accelerometer
##Gyro is replaced by Gyroscope
##prefix f is replaced by frequency
##Mag is replaced by Magnitude
##BodyBody is replaced by Body

names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

names(Data)

#Creates a second,independent tidy data set and ouput it
##In this part,a second, independent tidy data set will be created with the average of each 
##variable for each activity and each subject based on the data set in step 4.

library(plyr);
Data2<-aggregate(. ~subject + activity, Data, mean)
Data2<-Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)



#Creating codebook.Rmd file
#install.packages("dataMaid")
library(dataMaid)
makeCodebook(Data2, reportTitle = "codebook.Rmd")
