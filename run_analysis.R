library(plyr)
library(dtplyr)
library(reshape2)
#library(data.table)

data_train <- read.table("UCI HAR Dataset/train/X_train.txt")
data_test <- read.table("UCI HAR Dataset/test/X_test.txt")
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt")
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
myfeatures <- read.table("UCI HAR Dataset/features.txt")
myactivity <- read.table("UCI HAR Dataset/activity_labels.txt")

myfeatureindex1 <- grep("mean",myfeatures$V2)
myfeatureindex2 <- grep("std",myfeatures$V2)

subject_test <- rename(subject_test, subject_number = V1)
subject_train <- rename(subject_train, subject_number = V1)
activity_test <- rename(activity_test, activity = V1)
activity_train <- rename(activity_train, activity = V1)

subject_train <- mutate(subject_train, dataset="training")
subject_test <- mutate(subject_test, dataset="test")

for (i in 1:length(activity_test[,1])) { activity_test[i,] = as.character(myactivity$V2[as.numeric(activity_test[i,])]) }
for (i in 1:length(activity_train[,1])) { activity_train[i,] = as.character(myactivity$V2[as.numeric(activity_train[i,])]) }

mytemptest <- cbind(subject_test, activity_test, 
                    data_test[myfeatureindex1], data_test[myfeatureindex2])
mytemptrain <- cbind(subject_train, activity_train, 
                     data_train[myfeatureindex1], data_train[myfeatureindex2])

cleaned_data <- rbind(mytemptest, mytemptrain)
cleaned_data <- arrange(cleaned_data, subject_number)

hold1 <- NULL
hold2 <- NULL
hold3 <- NULL
hold4 <- NULL
hold1 = as.character(hold1)
hold3 = as.character(hold3)
for (i in myfeatureindex1) {
  hold1 <- c(hold1, as.character(myfeatures$V2[i]))
  hold2 <- c(hold2, paste("V", i, sep=""))
}
for (i in myfeatureindex2) {
  hold3 <- c(hold3, as.character(myfeatures$V2[i]))
  hold4 <- c(hold4, paste("V", i, sep=""))
}

setnames(cleaned_data, old = hold2, new = hold1)
setnames(cleaned_data, old = hold4, new = hold3)

cleaned_data <- arrange(cleaned_data, subject_number, activity)

df1 <- factor(cleaned_data$subject_number)
df2 <- factor(cleaned_data$activity)
df <- interaction(df1, df2)

mynewdata <- NULL
for (i in 4:length(cleaned_data[1,])) {
  mynewdata <- cbind(mynewdata,tapply(cleaned_data[,i],df,mean))
}
mynewdata <- as.data.frame(mynewdata)
colnames(mynewdata) <- colnames(cleaned_data[4:82])

mynewrownames <- do.call(rbind,strsplit(rownames(mynewdata),"\\."))
mynewdata <- mutate(mynewdata, subject_number = mynewrownames[,1])
mynewdata <- mutate(mynewdata, activity = mynewrownames[,2])
mynewdata <- mynewdata[,c(80:81, 1:79)]
