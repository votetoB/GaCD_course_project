setwd(".\\UCI HAR Dataset")
train_data <- read.table("train\\X_train.txt")
test_data <- read.table("test\\X_test.txt")
merged_data <- rbind.data.frame(train_data, test_data)



train_subject <- read.table("train\\subject_train.txt")
test_subject <- read.table("test\\subject_test.txt")
merged_subject <- rbind.data.frame(train_subject, test_subject)

y_train <- read.table("train\\y_train.txt")
y_test <- read.table("test\\y_test.txt")
merged_y <- rbind.data.frame(y_train, y_test)
merged_data$activity <- merged_y$V1


labels <- read.table("activity_labels.txt")

for(i in 1:6){
    merged_data$activity[merged_data$activity == i] <- as.character(labels[i, 2])
}

merged_data$activity <- as.factor(merged_data$activity)

names(merged_data) <- c(as.character(features$V2), "activity")

merged_data$subject <- merged_subject$V1

features <- read.table("features.txt")
features <- features[,2]
for_mean_std <- c(grepl("mean", features) | grepl("std", features), T, T)
col_names <- grep("mean|std", features, value = T)
col_names <- gsub("[()-]", "", col_names)
col_names <- tolower(col_names)
col_names <- gsub("[a-b]{4,}\1", "\1", col_names)


only_mean_or_std<- merged_data[, for_mean_std]


final_array_activity <- tapply(merged_data[,1], list(merged_data$activity), mean)
for(i in 2:(ncol(only_mean_or_std)-2)){
    final_array_activity <- cbind.data.frame(final_array_activity, tapply(merged_data[,i], list(merged_data$activity), mean))
}

names(final_array_activity) <- names(only_mean_or_std)[1:(ncol(only_mean_or_std)-2)]

final_array_subject <- tapply(merged_data[,1], list(merged_data$subject), mean)
for(i in 2:(ncol(only_mean_or_std)-2)){
    final_array_subject <- cbind.data.frame(final_array_subject, tapply(merged_data[,i], list(merged_data$subject), mean))
}

names(final_array_subject) <- names(only_mean_or_std)[1:(ncol(only_mean_or_std)-2)]

final_array <- rbind.data.frame(final_array_subject, final_array_activity)
names(final_array) <- col_names

write.table(final_array, "..\\answer.txt")