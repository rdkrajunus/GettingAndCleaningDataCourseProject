library(dplyr)

# returns a dataframe of the activities, with the activities tidied into
# camelCase
getActivityDf <- function() {
    activity.df <- tbl_df(read.table(
        file = "UCI HAR Dataset/activity_labels.txt",
        col.names = c("activityId", "activityName")))

    levels(activity.df$activityName) <- list(
        walking = "WALKING",
        walkingUpstairs = "WALKING_UPSTAIRS",
        walkingDownstairs = "WALKING_DOWNSTAIRS",
        sitting = "SITTING",
        standing = "STANDING",
        laying = "LAYING")

    return(activity.df)
}

# Returns a character vector of column names from the features.txt file that
# correspond to the mean and standard deviation for each measurement
getColumnNames <- function(columnIndexes) {
    features <- tbl_df(read.table("UCI HAR Dataset/features.txt"))

    columns.df <-
        features %>%
        slice(columnIndexes) %>%
        select(V2)

    columns <- as.character(columns.df$V2)
    columns <- gsub("^t+", "time", columns)
    columns <- gsub("^f+", "frequency", columns)
    columns <- gsub("Acc", "Accelerometer", columns)
    columns <- gsub("Gyro", "Gyroscope", columns)
    columns <- gsub("Mag", "Magnitude", columns)
    columns <- gsub("BodyBody", "Body", columns)
    columns <- gsub("-", "", columns)
    columns <- gsub("mean\\(\\)", "Mean", columns)
    columns <- gsub("std\\(\\)", "StdDev", columns)
    columns
}

# Returns the subject and activity files, joined with the specified columns from
# the data file.  The data columns that are read are those whose mean and
# standard deviation is known.
mergeDataWithSubject <- function(
    dataFile, dataFileClasses, dataColNames, subjectFile, activityFile) {

    data.df <- tbl_df(read.table(
        file = dataFile,
        colClasses = dataFileClasses))
    colnames(data.df) <- dataColNames

    subject.df <- tbl_df(read.table(
        file = subjectFile,
        col.names = c("subjectId")))

    activity.df <- tbl_df(read.table(
        file = activityFile,
        col.names = c("activityId")))

    cbind(subject.df, activity.df, data.df)
}

# This section declares a few variables
training.file <- "UCI HAR Dataset/train/X_train.txt"
traininglabels.file <- "UCI HAR Dataset/train/y_train.txt"
trainingsubject.file <- "UCI HAR Dataset/train/subject_train.txt"

test.file <- "UCI HAR Dataset/test/X_test.txt"
testlabels.file <- "UCI HAR Dataset/test/y_test.txt"
testsubject.file <- "UCI HAR Dataset/test/subject_test.txt"

# colClasses will prevent mergeDataWithSubject() from reading in 561 features.
# Instead we'll only read the features that are needed.
colClasses <- rep("NULL", 561)
columnIndexes <- c(
    1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241,
    253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543)
colClasses[columnIndexes] <- "numeric"
colNames <- getColumnNames(columnIndexes)

# Assemble a data frame based on the training data
training.df <- mergeDataWithSubject(
    training.file, colClasses, colNames, trainingsubject.file,
    traininglabels.file)

# Assemble a data frame based on the test data
test.df <- mergeDataWithSubject(
    test.file, colClasses, colNames, testsubject.file,
    testlabels.file)

activity.Df <- getActivityDf()

# Join activity descriptions to the combined data frames
df <- inner_join(rbind(training.df, test.df),
                 activity.Df,
                 by=c("activityId"))

mean.df <- df %>%
    group_by(activityName, subjectId) %>%
    summarise_each(funs(mean)) %>%
    select(subjectId,
           activityName,
           timeBodyAccelerometerMeanX:frequencyBodyGyroscopeJerkMagnitudeStdDev)

mean.df.colNames <- colnames(mean.df)
mean.df.colNames <- gsub("^t+", "meanOfT", mean.df.colNames)
mean.df.colNames <- gsub("^f+", "meanOfF", mean.df.colNames)
colnames(mean.df) <- mean.df.colNames

write.table(mean.df, "tidyMeanOfFeatures.txt", row.names=FALSE)