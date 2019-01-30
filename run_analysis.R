# 1. Merging the training & the Test sets to create one data set:

# Read trainings tables:
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# Read testing tables:
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# Read feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')

# Read activity labels:
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

# Assigning column Names
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId', 'activityType')

# Merging all data in one set
mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
setAllInOne <- rbind(mrg_train, mrg_test)

# 2. Extract only the measurements on the mean and std deviation for each measurement

# Read column names
colNames <- colnames(setAllInOne)

# create vector for defining ID, mean and std deviation
mean_and_std <- (grepl("activityId", colNames) |
                 grepl("subjectId", colNames) |
                 grepl("mean..", colNames) |
                 grepl("std..", colNames)
                         )

# make necessary subset
setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

#3. Uses descriptive activity names to name the activities in the data set
setWithActivityNames <- merge(setForMeanAndStd, activityLabels, by='activityId', all.x=TRUE)

# 4 & 5. create a second, independent tidy data set with the average of each variable for each activity & subject
secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId), ]

# write second tidy data set in txt file
write.table(secTidySet, "secTidySet.txt", row.name=FALSE)