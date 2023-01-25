install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%



# Declare the filename of the zip file
filename <- "packed.zip"

# Check if the file exists, if not download it from the specified URL
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        download.file(fileURL, filename, method="curl")
}  

# Check if the folder "data" exists, if not unzip the file
if (!file.exists("data")) { 
        unzip(filename) 
}

# Read the activity labels from the text file
activities<-read.table("data/UCI HAR Dataset/activity_labels.txt")

# Convert the second column to character type
activities[,2] <- as.character(activities[,2])

# Assign column names to the dataframe
names(activities)<-c('code','activity')

# Read the features from the text file
features<-read.table("data/UCI HAR Dataset/features.txt")

# Convert the second column to character type
features[,2] <- as.character(features[,2])

# Assign column names to the dataframe
names(features)<-c('code','feature')

# Get the indices of the features that contain "mean" or "std"
features.needed<- grep(".*mean.*|.*std.*", features[,2])

# Get the names of the needed features
features.needed.names <- features[features.needed,2]

# Replace "-mean" with "Mean" and "-std" with "Standard Deviation" in the feature names
features.needed.names = gsub('-mean', 'Mean', features.needed.names)
features.needed.names = gsub('-std', 'Standard Deviation', features.needed.names)

# Remove any remaining "-" or "()" characters from the feature names
features.needed.names <- gsub('[-()]', '', features.needed.names)

# expand abbreviations and clean up names
features.needed.names <- gsub("^f", "frequencyDomain", features.needed.names)
features.needed.names <- gsub("^t", "timeDomain", features.needed.names)
features.needed.names <- gsub("Acc", "Accelerometer", features.needed.names)
features.needed.names <- gsub("Gyro", "Gyroscope", features.needed.names)
features.needed.names <- gsub("Mag", "Magnitude", features.needed.names)
features.needed.names <- gsub("Freq", "Frequency", features.needed.names)
features.needed.names <- gsub("mean", "Mean", features.needed.names)
features.needed.names <- gsub("std", "StandardDeviation", features.needed.names)


# Read the X_train data and select only the needed features
X_train<-read.table("data/UCI HAR Dataset/train/X_train.txt")[features.needed]
# Read the y_train data
y_train<-read.table("data/UCI HAR Dataset/train/y_train.txt")
# Read the subjects_train data
subjects_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
# Combine the X_train, y_train, and subjects_train data into one dataframe
train_set<-cbind(subjects_train,X_train,y_train)


# Read the X_test data and select only the needed features
X_test<-read.table("data/UCI HAR Dataset/test/X_test.txt")[features.needed]
# Read the y_test data
y_test<-read.table("data/UCI HAR Dataset/test/y_test.txt")
# Read the subjects_test data
subjects_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
# Combine the X_test, y_test, and subjects_test data into one dataframe
test_set<-cbind(subjects_test,X_test,y_test)

# Combine the train and test data into one dataframe
final_df<-rbind(train_set,test_set)

# Assign column names to the final dataframe
names(final_df)<-c('Subject',features.needed.names,'Activity')

# Convert the Subject column to a factor variable
final_df$Subject <- as.factor(final_df$Subject)
# Convert the Activity column to a factor variable using the activity labels
final_df$Activity <- factor(final_df$Activity, levels = activities[,1], labels = activities[,2])


# Create a new dataframe by grouping by Subject and Activity, and summarizing each feature with the mean value
new_df <- final_df %>% 
        group_by(Subject, Activity) %>%
        summarise_each(funs(mean))

# Write the new dataframe to a text file, without row names and without quoting the values
write.table(new_df, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
