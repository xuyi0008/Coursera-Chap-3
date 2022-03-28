setwd("~/Coursera/Coursera-Chap-3")

#reading data
featureNAMES <- read.table("UCI HAR Dataset/features.txt")
activityLABELS <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
subjectTRAIN <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTRAIN <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTRAIN <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjectTEST <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTEST <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTEST <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Binding Data into Single Source
subject <- rbind(subjectTRAIN, subjectTEST)
activity <- rbind(activityTRAIN, activityTEST)
features <- rbind(featuresTRAIN, featuresTEST)

#Naming Columns & Merging Data
colnames(features) <- t(featureNAMES[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
Fulldata <- cbind(features,activity,subject)

#Extracts mean and standard deviation
columnswithmeanstddev <- grep(".*Mean.*|.*Std.*", names(Fulldata), ignore.case=TRUE)
requiredcolumns <- c(columnswithmeanstddev, 562, 563)
dim(Fulldata)

extracteddata <- Fulldata[,requiredcolumns]
dim(extracteddata)

#Adding descriptive names to activities
extracteddata$Activity <- as.character(extracteddata$Activity)
for (i in 1:6){
extracteddata$Activity[extracteddata$Activity == i] <- as.character(activityLABELS[i,2])
}

extracteddata$Activity <- as.factor(extracteddata$Activity)

#Labelling datasets
names(extracteddata)<-gsub("Acc", "Accelerometer", names(extracteddata))
names(extracteddata)<-gsub("Gyro", "Gyroscope", names(extracteddata))
names(extracteddata)<-gsub("BodyBody", "Body", names(extracteddata))
names(extracteddata)<-gsub("Mag", "Magnitude", names(extracteddata))
names(extracteddata)<-gsub("^t", "Time", names(extracteddata))
names(extracteddata)<-gsub("^f", "Frequency", names(extracteddata))
names(extracteddata)<-gsub("tBody", "TimeBody", names(extracteddata))
names(extracteddata)<-gsub("-mean()", "Mean", names(extracteddata), ignore.case = TRUE)
names(extracteddata)<-gsub("-std()", "STD", names(extracteddata), ignore.case = TRUE)
names(extracteddata)<-gsub("-freq()", "Frequency", names(extracteddata), ignore.case = TRUE)
names(extracteddata)<-gsub("angle", "Angle", names(extracteddata))
names(extracteddata)<-gsub("gravity", "Gravity", names(extracteddata))

#Create tidy dataset with average
extracteddata$Subject <- as.factor(extracteddata$Subject)
extracteddata <- data.table(extracteddata)

tidydata <- aggregate(. ~Subject + Activity, extracteddata, mean)
tidydata <- tidydata[order(tidydata$Subject,tidydata$Activity),]
write.table(tidydata, file = "Tidy.txt", row.names = FALSE)
