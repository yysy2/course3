library(dplyr)
library(plyr)
library(reshape2)

data_train <- read.table("UCI HAR Dataset/train/X_train.txt")
data_test <- read.table("UCI HAR Dataset/test/X_test.txt")
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt")
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

subject_test <- rename(subject_test, subject_number = V1)
subject_train <- rename(subject_train, subject_number = V1)
activity_test <- rename(activity_test, activity = V1)
activity_train <- rename(activity_train, activity = V1)

subject_train <- mutate(subject_train, dataset="training")
subject_test <- mutate(subject_test, dataset="test")

for (i in 1:length(activity_test[,1])) {
  if (activity_test[i,] == 1) {activity_test[i,] = "walking"}
  else if (activity_test[i,] == 2) {activity_test[i,] = "walking_upstairs"}
  else if (activity_test[i,] == 3) {activity_test[i,] = "walking_downstairs"}
  else if (activity_test[i,] == 4) {activity_test[i,] = "sitting"}
  else if (activity_test[i,] == 5) {activity_test[i,] = "standing"}
  else if (activity_test[i,] == 6) {activity_test[i,] = "laying"}
}

for (i in 1:length(activity_train[,1])) {
  if (activity_train[i,] == 1) {activity_train[i,] = "walking"}
  else if (activity_train[i,] == 2) {activity_train[i,] = "walking_upstairs"}
  else if (activity_train[i,] == 3) {activity_train[i,] = "walking_downstairs"}
  else if (activity_train[i,] == 4) {activity_train[i,] = "sitting"}
  else if (activity_train[i,] == 5) {activity_train[i,] = "standing"}
  else if (activity_train[i,] == 6) {activity_train[i,] = "laying"}
}

mytemptest <- cbind(subject_test, activity_test, 
                    data_test[c(1:6, 41:46, 81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543)])
mytemptrain <- cbind(subject_train, activity_train, 
                     data_train[c(1:6, 41:46, 81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543)])
cleaned_data <- rbind(mytemptest, mytemptrain)
cleaned_data <- arrange(cleaned_data, subject_number)

cleaned_data <- rename(cleaned_data, tbodyacc_mean_x = V1, tbodyacc_mean_y = V2, 
                       tbodyacc_mean_z = V3, tbodyacc_std_x = V4,
                       tbodyacc_std_y = V5, tbodyacc_std_z = V6,
                       tgravityacc_mean_x = V41, tgravityacc_mean_y = V42,
                       tgravityacc_mean_z = V43, tgravityacc_std_x = V44,
                       tgravityacc_std_y = V45, tgravityacc_std_z = V46,
                       tbodyaccjerk_mean_x = V81, tbodyaccjerk_mean_y = V82,
                       tbodyaccjerk_mean_z = V83, tbodyaccjerk_std_x = V84,
                       tbodyaccjerk_std_y = V85, tbodyaccjerk_std_z = V86,
                       tbodygyro_mean_x = V121, tbodygyro_mean_y = V122,
                       tbodygyro_mean_z = V123, tbodygyro_std_x = V124,
                       tbodygyro_std_t = V125, tbodygyro_std_z = V126,
                       tbodygyrojerk_mean_x = V161, tbodygyrojerk_mean_y = V162,
                       tbodygyrojerk_mean_z = V163, tbodygyrojerk_std_x = V164,
                       tbodygyrojerk_std_y = V165, tbodygyrojerk_std_z = V166,
                       tbodyaccmag_mean = V201, tbodyaccmag_std = V202,
                       tgravityaccmag_mean = V214, tgravityaccmag_std = V215,
                       tbodyaccjerkmag_mean = V227, tbodyaccjerkmag_std = V228,
                       tbodygyromag_mean = V240, tbodygyromag_std = V241,
                       tbodygyrojerkmag_mean = V253, tbodygyrojerkmag_std = V254,
                       fbodyacc_mean_x = V266, fbodyacc_mean_y = V267,
                       fbodyacc_mean_z = V268, fbodyacc_std_x = V269,
                       fbodyacc_std_y = V270, fbodyacc_std_z = V271,
                       fbodyjerkacc_mean_x = V345, fbodyjerkacc_mean_y = V346,
                       fbodyjerkacc_mean_z = V347, fbodyjerkacc_std_x = V348,
                       fbodyjerkacc_std_y = V349, fbodyjerkacc_std_z = V350,
                       fbodygyro_mean_x = V424, fbodygyro_mean_y = V425,
                       fbodygyro_mean_z = V426, fbodygyro_std_x = V427,
                       fbodygyro_std_y = V428, fbodygyro_std_z = V429,
                       fbodyaccmag_mean = V503, fbodyaccmag_std = V504,
                       fbodybodyaccjerkmag_mean = V516, fbodybodyaccjerkmag_std = V517,
                       fbodybodygyromag_mean = V529, fbodybodygyromag_std = V530,
                       fbodybodygyrojerkmag_mean = V542, fbodybodygyrojerkmag_std = V543)

cleaned_data <- arrange(cleaned_data, subject_number, activity)
ddply(bb,.(subject_number, actitvity, summarize, mean))