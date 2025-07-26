library(data.table)
library(dplyr)

cat("Step 1: Loading and merging training and test datasets...\n")

# Read training 
setwd("./UCI HAR Dataset/")
subject_train <- read.table("train/subject_train.txt", col.names = "subject")
X_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt", col.names = "activity")

# Read test data
subject_test <- read.table("test/subject_test.txt", col.names = "subject")
X_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt", col.names = "activity")

# Read feature names
features <- read.table("features.txt", col.names = c("index", "feature_name"))

# Read activity labels
activity_labels <- read.table("activity_labels.txt", col.names = c("activity", "activity_name"))

# Combine training and test sets
subjects <- rbind(subject_train, subject_test)
X_data <- rbind(X_train, X_test)
y_data <- rbind(y_train, y_test)

# Create complete dataset
complete_data <- cbind(subjects, y_data, X_data)

# Add column names to X_data part
names(complete_data)[3:ncol(complete_data)] <- features$feature_name

cat("Complete dataset created with dimensions:", dim(complete_data), "\n")

cat("Step 2: Extracting mean and standard deviation measurements...\n")

# Find columns that contain mean() or std() in their names
mean_std_cols <- grep("mean\\(\\)|std\\(\\)", features$feature_name)

# Select subject, activity, and mean/std columns
# Adding 2 to account for subject and activity columns at the beginning
selected_cols <- c(1, 2, mean_std_cols + 2)
mean_std_data <- complete_data[, selected_cols]

cat("Extracted", length(mean_std_cols), "mean and std deviation measurements\n")

cat("Step 3: Adding descriptive activity names...\n")

# Merge with activity labels
mean_std_data$activity <- factor(mean_std_data$activity, 
                                 levels = activity_labels$activity,
                                 labels = activity_labels$activity_name)

cat("Activity names added:\n")
print(levels(mean_std_data$activity))

cat("Step 4: Creating descriptive variable names...\n")

# Get current column names
col_names <- names(mean_std_data)

# Make descriptive names
col_names <- gsub("^t", "Time", col_names)
col_names <- gsub("^f", "Frequency", col_names)
col_names <- gsub("Acc", "Accelerometer", col_names)
col_names <- gsub("Gyro", "Gyroscope", col_names)
col_names <- gsub("Mag", "Magnitude", col_names)
col_names <- gsub("BodyBody", "Body", col_names)
col_names <- gsub("-mean\\(\\)", "Mean", col_names)
col_names <- gsub("-std\\(\\)", "StandardDeviation", col_names)
col_names <- gsub("-X", "XAxis", col_names)
col_names <- gsub("-Y", "YAxis", col_names)
col_names <- gsub("-Z", "ZAxis", col_names)

# Apply the new names
names(mean_std_data) <- col_names

cat("Variable names updated. Sample names:\n")
print(head(names(mean_std_data), 10))

cat("Step 5: Creating tidy dataset with averages...\n")

# Group by subject and activity, then calculate means
tidy_data <- mean_std_data %>%
  group_by(subject, activity) %>%
  summarise_all(mean) %>%
  ungroup()


cat("Saving tidy dataset...\n")

# Write the tidy dataset to a file
setwd("..")
write.table(tidy_data, "tidy_data.txt", row.names = FALSE)

cat("Tidy dataset saved as 'tidy_data.txt'\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Summary of final tidy dataset:\n")
str(tidy_data)

cat("\nFirst few rows of tidy dataset:\n")
print(head(tidy_data[, 1:6]))  # Show first 6 columns

cat("\nSample of subject-activity combinations:\n")
print(head(tidy_data[, 1:2], 10))

# Verification: Check that we have data for all subject-activity combinations
expected_rows <- length(unique(mean_std_data$subject)) * length(unique(mean_std_data$activity))
actual_rows <- nrow(tidy_data)
cat("Expected rows in tidy dataset:", expected_rows, "\n")
cat("Actual rows in tidy dataset:", actual_rows, "\n")
cat("Match:", expected_rows == actual_rows, "\n")

cat("\nAnalysis completed successfully!\n")
cat("The tidy dataset contains the average of each measurement\n")
cat("for each activity and each subject.\n")