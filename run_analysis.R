
# tested in Kubuntu 15.04, R version 3.1.2, Rstudio version 0.99.484  

# create data directory 
sourcedir <- dirname(sys.frame(1)$ofile)
setwd(sourcedir)
if (!file.exists("Data")) {
  dir.create("Data")
}

# download dataset file
dataset_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataset_localfile <- "Data/galaxyS_Data.zip"
dataset_extractfolder <- "Data/UCI HAR Dataset"
if (!file.exists(dataset_localfile)) {
  download.file( url=dataset_url, destfile=dataset_localfile)
  utils::unzip(dataset_localfile, exdir = "Data")
}
  
# STEP 1 : Merges the training and the test sets to create one data set.

# read test tables
test_set_path <- paste( dataset_extractfolder, "/test/X_test.txt", sep = "" )
test_set <- read.table(file=test_set_path, header = FALSE)
test_label_path <- paste( dataset_extractfolder, "/test/y_test.txt", sep = "" )
test_label <- read.table(file=test_label_path, header = FALSE)
test_subject_path  <- paste( dataset_extractfolder, "/test/subject_test.txt", sep = "" )
test_subject <- read.table(file=test_subject_path, header = FALSE)

# read test tables
train_set_path <- paste( dataset_extractfolder, "/train/X_train.txt", sep = "" )
train_set <- read.table(file=train_set_path, header = FALSE)
train_label_path <- paste( dataset_extractfolder, "/train/y_train.txt", sep = "" )
train_label <- read.table(file=train_label_path, header = FALSE)
train_subject_path  <- paste( dataset_extractfolder, "/train/subject_train.txt", sep = "" )
train_subject <- read.table(file=train_subject_path, header = FALSE)

# merge test and train tables
data_set <- rbind( test_set, train_set )
data_label <- rbind( test_label, train_label )
names(data_label) <- list( "activity_id" )
data_subject <- rbind( test_subject, train_subject )
names(data_subject) <- list( "subject_id" )

complete_data_set <- cbind( data_set, data_label, data_subject )

# STEP 2 : Extracts only the measurements on the mean and standard deviation for each measurement. 

# Read the features table
features_path <- paste( dataset_extractfolder, "/features.txt", sep = "" )
features <- read.table(file=features_path, header = FALSE, stringsAsFactors = FALSE)
names(features) <- list("feature_id", "feature_name")

# Build a lookup table for mean-based and std-based features
data_mean_std_lkp <- sapply( "mean()", grepl, features$feature_name ) | sapply( "std()", grepl, features$feature_name )
features_of_interest <- features[ data_mean_std_lkp, ]

# filter features and set names accordingly
data_set_interest <- complete_data_set[, features_of_interest$feature_id]
names(data_set_interest) <- features_of_interest$feature_name
data_set_interest <- cbind( data_set_interest, "activity_id" = complete_data_set$activity_id, "subject_id" = complete_data_set$subject_id )


# STEP 3 : Uses descriptive activity names to name the activities in the data set

# Read the activity labels table
activity_labels_path <- paste( dataset_extractfolder, "/activity_labels.txt", sep = "" )
activity_labels <- read.table(file=activity_labels_path, header = FALSE)
names(activity_labels) <- list( "activity_id", "activity_label")

library(plyr)
data_set_interest <- mutate( data_set_interest, activity_label = activity_labels$activity_label[ activity_id ] )

# STEP 4 : Appropriately labels the data set with descriptive variable names. 

names(data_set_interest) <- c ( features_of_interest$feature_name, "activity_id", "subject_id", "activity_label" )

# STEP 5 : From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

library(reshape2)
tidy_set <- melt(data_set_interest, id.vars = c("activity_id", "activity_label", "subject_id") )
tidy_set_ <- dcast(tidy_set, activity_label + subject_id ~ variable, mean)

# write the output table
write.table(tidy_set_, row.name = FALSE, file = "tidy_set.txt")



