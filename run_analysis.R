
###############################################
# (Done ) You should create one R script called run_analysis.R that does the following.

###############################################
# Merges the training and the test sets to create one data set.

library(reshape2)

#Import "X_train.txt", "y_train.txt", "subject_train.txt"
Dir<-"C:/Users/markk/Desktop/Coursera/Data Science (John Hopkins)/Getting and Cleaning Data/HW4/UCI HAR Dataset/train/"

File<-"X_train.txt"
x_train = read.table(paste0(Dir,File))

File<-"y_train.txt"
y_train = read.table(paste0(Dir,File))

File<-"subject_train.txt"
subject_train = read.table(paste0(Dir,File))



#Import "X_test.txt", "y_test.txt", "subject_test.txt"
Dir<-"C:/Users/markk/Desktop/Coursera/Data Science (John Hopkins)/Getting and Cleaning Data/HW4/UCI HAR Dataset/test/"

File<-"X_test.txt"
x_test = read.table(paste0(Dir,File))

File<-"y_test.txt"
y_test = read.table(paste0(Dir,File))

File<-"subject_test.txt"
subject_test = read.table(paste0(Dir,File))

#Import "X_test.txt"
Dir<-"C:/Users/markk/Desktop/Coursera/Data Science (John Hopkins)/Getting and Cleaning Data/HW4/UCI HAR Dataset/"

File<-"activity_labels.txt"
activity_labels= read.table(paste0(Dir,File))

File<-"features.txt"
features_labels= read.table(paste0(Dir,File))


x <- rbind(x_train, x_test)
colnames(x)<-features_labels[,2]

y <- rbind(y_train, y_test)
colnames(y)<-c("Activity")

subject<-rbind(subject_train,subject_test)
colnames(subject)<-c("Subject")

UntidyDataset<-cbind(y,subject,x)



###############################################
# Extracts only the measurements on the mean and standard deviation for each 
# measurement.

FilteredColNo <- grep("*mean*|*std*", features_labels[,2])
FilteredColNo<-FilteredColNo+2
FilteredColNo<-c(1,2,FilteredColNo)

TidyDataset<-UntidyDataset[,FilteredColNo]

################################################
# Uses descriptive activity names to name the activities in the data set

TidyDataset$Activity<-mapvalues(TidyDataset$Activity, from=c(1,2,3,4,5,6), to=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

################################################
# Appropriately labels the data set with descriptive variable names.
Names<-names(TidyDataset)
Names = gsub('^t', 'Time_', Names)
Names = gsub('^f', 'Frequency_', Names)
Names = gsub('mean', 'Mean_', Names)
Names = gsub('std', 'StdDev_', Names)
Names = gsub('Body', 'Body_', Names)
Names = gsub('Body_Body_', 'Body_', Names)
Names = gsub('Gravity', 'Gravity_', Names)
Names = gsub('Acc', 'Acceleration_', Names)
Names = gsub('Jerk', 'Jerk_', Names)
Names = gsub('Gyro', 'Gyro_', Names)
Names = gsub('()', '', Names)

colnames(TidyDataset)<-Names 

################################################
# From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.

TidyDataset2 <- melt(TidyDataset, id=c("Activity","Subject"))
TidyDataset2<-cast(TidyDataset2,Subject+Activity~...,mean)

################################################
# Save the Data Sets
write.table(TidyDataset, "TidyDataset1.txt", row.names = FALSE, quote = FALSE)
write.table(TidyDataset2, "TidyDataset2.txt", row.names = FALSE, quote = FALSE)