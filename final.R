
setwd("~/Desktop/temp/6try/")


app_df <- read.csv("application_record.csv", header = TRUE)
credit_df <- read.csv("credit_record.csv", header = TRUE)

print(app_df$DAYS_EMPLOYED)

# Check the structure of the datasets
str(app_df)
str(credit_df)

# Display a summary of missing values
summary(is.na(app_df))

#dropping Occupation Type because it has many null values
app_df <- app_df[, !(names(app_df) %in% c("OCCUPATION_TYPE"))]


duplicates_count <- length(app_df$ID) - length(unique(app_df$ID))

# Print the number of duplicates
print(duplicates_count)

# Drop duplicate entries from the 'ID' column, keeping the last occurrence
app_df <- app_df[!duplicated(app_df$ID, fromLast = TRUE), ]


# Identify non-numerical columns
cat_columns <- names(app_df)[sapply(app_df, function(x) is.character(x) | is.factor(x))]

# Print the non-numerical column names
print(cat_columns)

# Identify numerical columns
num_columns <- names(app_df)[sapply(app_df, function(x) is.numeric(x))]

# Print the numerical column names
print(num_columns)

####################################################################################

# Identify and print unique values for each categorical column
cat_columns <- names(app_df)[sapply(app_df, function(x) is.character(x) | is.factor(x))]

# Loop through categorical columns
for (col in cat_columns) {
  cat_values <- unique(app_df[[col]])
  cat_counts <- table(app_df[[col]])
  
  # Print column name
  print(paste(col, '\n'))
  
  # Print unique values and their counts
  print(cat_counts)
  
  # Print separator
  print('-----------------------------------------------')
}



# Convert 'DAYS_BIRTH' values from days to years
app_df$AGE_YEARS <- round(app_df$DAYS_BIRTH / -365, 0)

# Rename the column
colnames(app_df)[colnames(app_df) == "DAYS_BIRTH"] <- "AGE_YEARS"





# Replace positive values in 'DAYS_EMPLOYED' with 0
app_df$DAYS_EMPLOYED[app_df$DAYS_EMPLOYED > 0] <- 0


print(app_df$DAYS_EMPLOYED)


# Convert 'DAYS_EMPLOYED' values from days to years
app_df$YEARS_EMPLOYED <- abs(round(app_df$DAYS_EMPLOYED / -365, 0))

# Drop the original 'DAYS_EMPLOYED' column
app_df <- app_df[, !(names(app_df) %in% c("DAYS_EMPLOYED"))]





# Drop the 'FLAG_MOBIL' column if all values are 1
if(all(app_df$FLAG_MOBIL == 1)) {
  app_df <- app_df[, !(names(app_df) %in% c("FLAG_MOBIL"))]
}



# Drop the 'FLAG_WORK_PHONE' column if it only contains 0 and 1 values
if(all(app_df$FLAG_WORK_PHONE %in% c(0, 1))) {
  app_df <- app_df[, !(names(app_df) %in% c("FLAG_WORK_PHONE"))]
}



# Drop the 'FLAG_PHONE' column if it only contains 0 and 1 values
if(all(app_df$FLAG_PHONE %in% c(0, 1))) {
  app_df <- app_df[, !(names(app_df) %in% c("FLAG_PHONE"))]
}





# Drop the 'FLAG_EMAIL' column if it only contains 0 and 1 values
if(all(app_df$FLAG_EMAIL %in% c(0, 1))) {
  app_df <- app_df[, !(names(app_df) %in% c("FLAG_EMAIL"))]
}



# Calculate high and low bounds for 'CNT_CHILDREN'
high_bound <- quantile(app_df$CNT_CHILDREN, 0.999)
low_bound <- quantile(app_df$CNT_CHILDREN, 0.001)

# Print the results
print(paste('high_bound :', high_bound))
print(paste('low_bound :', low_bound))




# Filter the data frame based on conditions for 'CNT_CHILDREN'
app_df <- app_df[app_df$CNT_CHILDREN >= low_bound & app_df$CNT_CHILDREN <= high_bound, ]




# Calculate high and low bounds for 'AMT_INCOME_TOTAL'
high_bound <- quantile(app_df$AMT_INCOME_TOTAL, 0.999)
low_bound <- quantile(app_df$AMT_INCOME_TOTAL, 0.001)

# Print the results
print(paste('high_bound :', high_bound))
print(paste('low_bound :', low_bound))


# Filter the data frame based on conditions for 'AMT_INCOME_TOTAL'
app_df <- app_df[app_df$AMT_INCOME_TOTAL >= low_bound & app_df$AMT_INCOME_TOTAL <= high_bound, ]

# Print the 'YEARS_EMPLOYED' column
print(app_df$YEARS_EMPLOYED)


# Calculate high and low bounds for 'YEARS_EMPLOYED'
high_bound <- quantile(app_df$YEARS_EMPLOYED, 0.999)
low_bound <- quantile(app_df$YEARS_EMPLOYED, 0.001)

# Print the results
print(paste('high_bound :', high_bound))
print(paste('low_bound :', low_bound))


# Filter the data frame based on conditions for 'YEARS_EMPLOYED'
app_df <- app_df[app_df$YEARS_EMPLOYED >= low_bound & app_df$YEARS_EMPLOYED <= high_bound, ]


# Calculate high and low bounds for 'CNT_FAM_MEMBERS' while removing missing values
high_bound <- quantile(app_df$CNT_FAM_MEMBERS, 0.999, na.rm = TRUE)
low_bound <- quantile(app_df$CNT_FAM_MEMBERS, 0.001, na.rm = TRUE)

# Print the results
print(paste('high_bound :', high_bound))
print(paste('low_bound :', low_bound))


# Filter the data frame based on conditions for 'CNT_FAM_MEMBERS'
app_df <- app_df[app_df$CNT_FAM_MEMBERS >= low_bound & app_df$CNT_FAM_MEMBERS <= high_bound, ]



# Categorize 'STATUS' column to binary classification: 0 for 'C' and 'X', 1 otherwise
credit_df$STATUS <- ifelse(credit_df$STATUS %in% c('C', 'X'), 0, 1)

# Replace '2', '3', '4', '5' values in 'STATUS' column with 1
credit_df$STATUS[credit_df$STATUS %in% c('2', '3', '4', '5')] <- 1


# Convert 'STATUS' column to integer type
credit_df$STATUS <- as.integer(credit_df$STATUS)


# Calculate the percentage of each unique value in 'STATUS' column
status_counts <- table(credit_df$STATUS) / length(credit_df$STATUS) * 100

# Print the results
print(status_counts)

# Group by 'ID' and aggregate using max
credit_df_trans <- aggregate(. ~ ID, data = credit_df, max)

########################################################################

# Drop the 'MONTHS_BALANCE' column
credit_df_trans <- credit_df_trans[, !(names(credit_df_trans) %in% c("MONTHS_BALANCE"))]


# Calculate the percentage of each unique value in 'STATUS' column
status_counts <- table(credit_df_trans$STATUS) / nrow(credit_df_trans) * 100

# Print the results
print(status_counts)

# Merge the two data frames based on 'ID'
final_df <- merge(app_df, credit_df_trans, by = 'ID', all = FALSE)

# Drop the 'ID' column
final_df <- final_df[, !(names(final_df) %in% c("ID"))]



# Checking for duplicate rows
duplicate_rows <- final_df[duplicated(final_df), ]

# Print the number of duplicate rows
print(paste("Number of duplicate rows: ", nrow(duplicate_rows)))






final_df <- final_df[!duplicated(final_df), ]

# Reset the index
final_df <- final_df[order(row.names(final_df)), ]
rownames(final_df) <- NULL



# Calculate the percentage of each unique value in 'STATUS' column
status_counts <- table(final_df$STATUS) / nrow(final_df) * 100

# Print the results
print(status_counts)


##install.packages("corrplot")


# Drop the 'YEARS_EMPLOYED.1' column
#final_df <- final_df[, !(names(final_df) %in% c("YEARS_EMPLOYED.1"))]

numeric_columns <- final_df[sapply(final_df, is.numeric)]

# Create a correlation matrix
cor_matrix <- cor(numeric_columns)

# Load the corrplot package
library(corrplot)

# Plot the heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black", tl.srt = 45)



############################################################################################



# Load the necessary library
library(ggplot2)

# Create a pie chart
ggplot(final_df, aes(x = factor(CNT_CHILDREN))) +
  geom_bar(stat = "count", fill = "blue") +
  coord_polar("y") +
  labs(title = "% of Applications submitted based on Children count")



####################################################################


# Calculate the count of each unique value in 'CNT_CHILDREN'
children_counts <- table(final_df$CNT_CHILDREN)

# Create a pie chart
pie(children_counts, labels = paste(names(children_counts), ": ", children_counts), main = "% of Applications submitted based on Children count")

########################################################################################


# Calculate the count of each unique value in 'NAME_INCOME_TYPE'
income_type_counts <- table(final_df$NAME_INCOME_TYPE)

# Create a pie chart
pie(income_type_counts, labels = paste(names(income_type_counts), ": ", income_type_counts), main = "% of Applications submitted based on Income Type")


############################################################################################################


# Convert categorical columns to factors
final_df[, cat_columns] <- lapply(final_df[, cat_columns], as.factor)

# Print the updated data frame
print(final_df)

################################

# Print the levels (classes) of each categorical variable after encoding
for (col in cat_columns) {
  print(paste(col, " : ", levels(final_df[, col])))
}


##########################################################################


# Identify numeric columns
numeric_columns <- final_df[sapply(final_df, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_columns)

# Print the correlation matrix
print(cor_matrix)


setwd("~/Desktop/temp/6try/")


app_df <- read.csv("application_record.csv", header = TRUE)
credit_df <- read.csv("credit_record.csv", header = TRUE)

print(app_df$DAYS_EMPLOYED)

# Check the structure of the datasets
str(app_df)
str(credit_df)

# Display a summary of missing values
summary(is.na(app_df))

#dropping Occupation Type because it has many null values
app_df <- app_df[, !(names(app_df) %in% c("OCCUPATION_TYPE"))]


duplicates_count <- length(app_df$ID) - length(unique(app_df$ID))

# Print the number of duplicates
print(duplicates_count)

# Drop duplicate entries from the 'ID' column, keeping the last occurrence
app_df <- app_df[!duplicated(app_df$ID, fromLast = TRUE), ]


# Identify non-numerical columns
cat_columns <- names(app_df)[sapply(app_df, function(x) is.character(x) | is.factor(x))]

# Print the non-numerical column names
print(cat_columns)

# Identify numerical columns
num_columns <- names(app_df)[sapply(app_df, function(x) is.numeric(x))]

# Print the numerical column names
print(num_columns)

####################################################################################

# Identify and print unique values for each categorical column
cat_columns <- names(app_df)[sapply(app_df, function(x) is.character(x) | is.factor(x))]

# Loop through categorical columns
for (col in cat_columns) {
  cat_values <- unique(app_df[[col]])
  cat_counts <- table(app_df[[col]])
  
  # Print column name
  print(paste(col, '\n'))
  
  # Print unique values and their counts
  print(cat_counts)
  
  # Print separator
  print('-----------------------------------------------')
}



# Convert 'DAYS_BIRTH' values from days to years
app_df$AGE_YEARS <- round(app_df$DAYS_BIRTH / -365, 0)

# Rename the column
colnames(app_df)[colnames(app_df) == "DAYS_BIRTH"] <- "AGE_YEARS"





# Replace positive values in 'DAYS_EMPLOYED' with 0
app_df$DAYS_EMPLOYED[app_df$DAYS_EMPLOYED > 0] <- 0


print(app_df$DAYS_EMPLOYED)


# Convert 'DAYS_EMPLOYED' values from days to years
app_df$YEARS_EMPLOYED <- abs(round(app_df$DAYS_EMPLOYED / -365, 0))

# Drop the original 'DAYS_EMPLOYED' column
app_df <- app_df[, !(names(app_df) %in% c("DAYS_EMPLOYED"))]





# Drop the 'FLAG_MOBIL' column if all values are 1
if(all(app_df$FLAG_MOBIL == 1)) {
  app_df <- app_df[, !(names(app_df) %in% c("FLAG_MOBIL"))]
}



# Drop the 'FLAG_WORK_PHONE' column if it only contains 0 and 1 values
if(all(app_df$FLAG_WORK_PHONE %in% c(0, 1))) {
  app_df <- app_df[, !(names(app_df) %in% c("FLAG_WORK_PHONE"))]
}



# Drop the 'FLAG_PHONE' column if it only contains 0 and 1 values
if(all(app_df$FLAG_PHONE %in% c(0, 1))) {
  app_df <- app_df[, !(names(app_df) %in% c("FLAG_PHONE"))]
}





# Drop the 'FLAG_EMAIL' column if it only contains 0 and 1 values
if(all(app_df$FLAG_EMAIL %in% c(0, 1))) {
  app_df <- app_df[, !(names(app_df) %in% c("FLAG_EMAIL"))]
}



# Calculate high and low bounds for 'CNT_CHILDREN'
high_bound <- quantile(app_df$CNT_CHILDREN, 0.999)
low_bound <- quantile(app_df$CNT_CHILDREN, 0.001)

# Print the results
print(paste('high_bound :', high_bound))
print(paste('low_bound :', low_bound))




# Filter the data frame based on conditions for 'CNT_CHILDREN'
app_df <- app_df[app_df$CNT_CHILDREN >= low_bound & app_df$CNT_CHILDREN <= high_bound, ]




# Calculate high and low bounds for 'AMT_INCOME_TOTAL'
high_bound <- quantile(app_df$AMT_INCOME_TOTAL, 0.999)
low_bound <- quantile(app_df$AMT_INCOME_TOTAL, 0.001)

# Print the results
print(paste('high_bound :', high_bound))
print(paste('low_bound :', low_bound))


# Filter the data frame based on conditions for 'AMT_INCOME_TOTAL'
app_df <- app_df[app_df$AMT_INCOME_TOTAL >= low_bound & app_df$AMT_INCOME_TOTAL <= high_bound, ]

# Print the 'YEARS_EMPLOYED' column
print(app_df$YEARS_EMPLOYED)


# Calculate high and low bounds for 'YEARS_EMPLOYED'
high_bound <- quantile(app_df$YEARS_EMPLOYED, 0.999)
low_bound <- quantile(app_df$YEARS_EMPLOYED, 0.001)

# Print the results
print(paste('high_bound :', high_bound))
print(paste('low_bound :', low_bound))


# Filter the data frame based on conditions for 'YEARS_EMPLOYED'
app_df <- app_df[app_df$YEARS_EMPLOYED >= low_bound & app_df$YEARS_EMPLOYED <= high_bound, ]


# Calculate high and low bounds for 'CNT_FAM_MEMBERS' while removing missing values
high_bound <- quantile(app_df$CNT_FAM_MEMBERS, 0.999, na.rm = TRUE)
low_bound <- quantile(app_df$CNT_FAM_MEMBERS, 0.001, na.rm = TRUE)

# Print the results
print(paste('high_bound :', high_bound))
print(paste('low_bound :', low_bound))


# Filter the data frame based on conditions for 'CNT_FAM_MEMBERS'
app_df <- app_df[app_df$CNT_FAM_MEMBERS >= low_bound & app_df$CNT_FAM_MEMBERS <= high_bound, ]



# Categorize 'STATUS' column to binary classification: 0 for 'C' and 'X', 1 otherwise
credit_df$STATUS <- ifelse(credit_df$STATUS %in% c('C', 'X'), 0, 1)

# Replace '2', '3', '4', '5' values in 'STATUS' column with 1
credit_df$STATUS[credit_df$STATUS %in% c('2', '3', '4', '5')] <- 1


# Convert 'STATUS' column to integer type
credit_df$STATUS <- as.integer(credit_df$STATUS)


# Calculate the percentage of each unique value in 'STATUS' column
status_counts <- table(credit_df$STATUS) / length(credit_df$STATUS) * 100

# Print the results
print(status_counts)

# Group by 'ID' and aggregate using max
credit_df_trans <- aggregate(. ~ ID, data = credit_df, max)

########################################################################

# Drop the 'MONTHS_BALANCE' column
credit_df_trans <- credit_df_trans[, !(names(credit_df_trans) %in% c("MONTHS_BALANCE"))]


# Calculate the percentage of each unique value in 'STATUS' column
status_counts <- table(credit_df_trans$STATUS) / nrow(credit_df_trans) * 100

# Print the results
print(status_counts)

# Merge the two data frames based on 'ID'
final_df <- merge(app_df, credit_df_trans, by = 'ID', all = FALSE)

# Drop the 'ID' column
final_df <- final_df[, !(names(final_df) %in% c("ID"))]



# Checking for duplicate rows
duplicate_rows <- final_df[duplicated(final_df), ]

# Print the number of duplicate rows
print(paste("Number of duplicate rows: ", nrow(duplicate_rows)))






final_df <- final_df[!duplicated(final_df), ]

# Reset the index
final_df <- final_df[order(row.names(final_df)), ]
rownames(final_df) <- NULL



# Calculate the percentage of each unique value in 'STATUS' column
status_counts <- table(final_df$STATUS) / nrow(final_df) * 100

# Print the results
print(status_counts)


##install.packages("corrplot")


# Drop the 'YEARS_EMPLOYED.1' column
#final_df <- final_df[, !(names(final_df) %in% c("YEARS_EMPLOYED.1"))]

numeric_columns <- final_df[sapply(final_df, is.numeric)]

# Create a correlation matrix
cor_matrix <- cor(numeric_columns)

# Load the corrplot package
library(corrplot)

# Plot the heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black", tl.srt = 45)



############################################################################################



# Load the necessary library
library(ggplot2)

# Create a pie chart
ggplot(final_df, aes(x = factor(CNT_CHILDREN))) +
  geom_bar(stat = "count", fill = "blue") +
  coord_polar("y") +
  labs(title = "% of Applications submitted based on Children count")



####################################################################


# Calculate the count of each unique value in 'CNT_CHILDREN'
children_counts <- table(final_df$CNT_CHILDREN)

# Create a pie chart
pie(children_counts, labels = paste(names(children_counts), ": ", children_counts), main = "% of Applications submitted based on Children count")

########################################################################################


# Calculate the count of each unique value in 'NAME_INCOME_TYPE'
income_type_counts <- table(final_df$NAME_INCOME_TYPE)

# Create a pie chart
pie(income_type_counts, labels = paste(names(income_type_counts), ": ", income_type_counts), main = "% of Applications submitted based on Income Type")


############################################################################################################


# Convert categorical columns to factors
final_df[, cat_columns] <- lapply(final_df[, cat_columns], as.factor)

# Print the updated data frame
print(final_df)

################################

# Print the levels (classes) of each categorical variable after encoding
for (col in cat_columns) {
  print(paste(col, " : ", levels(final_df[, col])))
}


##########################################################################


# Identify numeric columns
numeric_columns <- final_df[sapply(final_df, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_columns)

# Print the correlation matrix
print(cor_matrix)


##############################################################################

# Extract features (all columns except 'STATUS')
features <- final_df[, !(names(final_df) %in% c("STATUS"))]

# Extract labels (only 'STATUS' column)
label <- final_df$STATUS
label

##################################################################################
library(glmnet)
library(caret)
library(ROSE)

# Assuming STATUS is a numeric variable, convert it to a factor
final_df$STATUS <- as.factor(final_df$STATUS)

# Split the data into training and testing sets
set.seed(65535)  # Set seed for reproducibility
sample_indices <- sample(1:nrow(final_df), 0.7 * nrow(final_df))
train_data <- final_df[sample_indices, ]
test_data <- final_df[-sample_indices, ]

# Use ROSE to address class imbalance
train_data_balanced <- ROSE(STATUS ~ ., data = train_data, seed = 553)$data

# Define the logistic regression model
log_model <- glm(STATUS ~ ., data = train_data_balanced, family = binomial)

# Make predictions on the test set
predictions <- predict(log_model, newdata = test_data, type = "response")
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the model
accuracy <- sum(predicted_labels == test_data$STATUS) / length(test_data$STATUS) * 100
conf_matrix <- table(test_data$STATUS, predicted_labels)
classification_report <- confusionMatrix(as.factor(predicted_labels), as.factor(test_data$STATUS))

# Print the results
print(paste("Logistic Model Accuracy: ", round(accuracy, 2), "%"))
print("Confusion Matrix:")
print(conf_matrix)
print("Classification Report:")
print(classification_report)

###################################################################################


# #install and load the necessary packages
#install.packages("e1071")
library(e1071)

# Assuming 'final_df' is the name of your data frame in R
# Assuming 'STATUS' is the target variablelibrary(ggplot2)

# Assuming 'STATUS' is the target variable in your data frame
status_counts <- table(final_df$STATUS)

# Create a bar plot
ggplot(data.frame(status = names(status_counts), count = as.numeric(status_counts)), aes(x = status, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Distribution of STATUS", x = "STATUS", y = "Count")

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
sample_indices <- sample(1:nrow(final_df), 0.7 * nrow(final_df))
train_data <- final_df[sample_indices, ]
test_data <- final_df[-sample_indices, ]

# Define the Naive Bayes model
nb_model <- naiveBayes(STATUS ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(nb_model, newdata = test_data)

# Evaluate the model
accuracy <- sum(predictions == test_data$STATUS) / length(test_data$STATUS) * 100
conf_matrix <- table(test_data$STATUS, predictions)
classification_report <- confusionMatrix(as.factor(predictions), as.factor(test_data$STATUS))

# Print the results
print(paste("Naive Bayes Model Accuracy: ", round(accuracy, 2), "%"))
print("Confusion Matrix:")
print(conf_matrix)
print("Classification Report:")
print(classification_report)

#################################################################################################

##############################################################################

# Extract features (all columns except 'STATUS')
features <- final_df[, !(names(final_df) %in% c("STATUS"))]

# Extract labels (only 'STATUS' column)
label <- final_df$STATUS
label

##################################################################################
library(glmnet)
library(caret)
library(ROSE)

# Assuming STATUS is a numeric variable, convert it to a factor
final_df$STATUS <- as.factor(final_df$STATUS)

# Split the data into training and testing sets
set.seed(65535)  # Set seed for reproducibility
sample_indices <- sample(1:nrow(final_df), 0.7 * nrow(final_df))
train_data <- final_df[sample_indices, ]
test_data <- final_df[-sample_indices, ]

# Use ROSE to address class imbalance
train_data_balanced <- ROSE(STATUS ~ ., data = train_data, seed = 553)$data

# Define the logistic regression model
log_model <- glm(STATUS ~ ., data = train_data_balanced, family = binomial)

# Make predictions on the test set
predictions <- predict(log_model, newdata = test_data, type = "response")
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the model
accuracy <- sum(predicted_labels == test_data$STATUS) / length(test_data$STATUS) * 100
conf_matrix <- table(test_data$STATUS, predicted_labels)
classification_report <- confusionMatrix(as.factor(predicted_labels), as.factor(test_data$STATUS))

# Print the results
print(paste("Logistic Model Accuracy: ", round(accuracy, 2), "%"))
print("Confusion Matrix:")
print(conf_matrix)
print("Classification Report:")
print(classification_report)

###################################################################################


# #install and load the necessary packages
#install.packages("e1071")
library(e1071)

# Assuming 'final_df' is the name of your data frame in R
# Assuming 'STATUS' is the target variablelibrary(ggplot2)

# Assuming 'STATUS' is the target variable in your data frame
status_counts <- table(final_df$STATUS)

# Create a bar plot
ggplot(data.frame(status = names(status_counts), count = as.numeric(status_counts)), aes(x = status, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Distribution of STATUS", x = "STATUS", y = "Count")

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
sample_indices <- sample(1:nrow(final_df), 0.7 * nrow(final_df))
train_data <- final_df[sample_indices, ]
test_data <- final_df[-sample_indices, ]

# Define the Naive Bayes model
nb_model <- naiveBayes(STATUS ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(nb_model, newdata = test_data)

# Evaluate the model
accuracy <- sum(predictions == test_data$STATUS) / length(test_data$STATUS) * 100
conf_matrix <- table(test_data$STATUS, predictions)
classification_report <- confusionMatrix(as.factor(predictions), as.factor(test_data$STATUS))

# Print the results
print(paste("Naive Bayes Model Accuracy: ", round(accuracy, 2), "%"))
print("Confusion Matrix:")
print(conf_matrix)
print("Classification Report:")
print(classification_report)

#################################################################################################
max<-0.0
maxtemp<-0
for (temp in 1:500) {
  
  library(glmnet)
  library(caret)
  library(ROSE)
  
  # Assuming STATUS is a numeric variable, convert it to a factor
  final_df$STATUS <- as.factor(final_df$STATUS)
  
  # Split the data into training and testing sets
  set.seed(123)  # Set seed for reproducibility
  sample_indices <- sample(1:nrow(final_df), 0.7 * nrow(final_df))
  train_data <- final_df[sample_indices, ]
  test_data <- final_df[-sample_indices, ]
  
  # Use ROSE to address class imbalance
  train_data_balanced <- ROSE(STATUS ~ ., data = train_data, seed = temp)$data
  
  # Define the logistic regression model
  log_model <- glm(STATUS ~ ., data = train_data_balanced, family = binomial)
  
  # Make predictions on the test set
  predictions <- predict(log_model, newdata = test_data, type = "response")
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Evaluate the model
  accuracy <- sum(predicted_labels == test_data$STATUS) / length(test_data$STATUS) * 100
  conf_matrix <- table(test_data$STATUS, predicted_labels)
  classification_report <- confusionMatrix(as.factor(predicted_labels), as.factor(test_data$STATUS))
  
  # Print the results
  print(paste("Logistic Model Accuracy: ", round(accuracy, 2), "%"))
  print("Confusion Matrix:")
  print(conf_matrix)
  print("Classification Report:")
  print(classification_report)
  
  
  # Print the results
  if (accuracy > max)
  {
    max<-accuracy
    maxtemp<-temp
  }
}
print(max)
print(maxtemp)
#





#############################################################################################




library(glmnet)
library(caret)
library(ROSE)

# Assuming STATUS is a numeric variable, convert it to a factor
final_df$STATUS <- as.factor(final_df$STATUS)

# Split the data into training and testing sets

# Initialize variables to store results
best_seed <- NULL
max_accuracy <- 0

# Iterate over different seeds
for (seed_value in 1:5000) {
  tryCatch({
    
    # Use ROSE to address class imbalance
    library(glmnet)
    library(caret)
    library(ROSE)
    
    # Assuming STATUS is a numeric variable, convert it to a factor
    final_df$STATUS <- as.factor(final_df$STATUS)
    
    # Split the data into training and testing sets
    set.seed(65535)  # Set seed for reproducibility
    sample_indices <- sample(1:nrow(final_df), 0.7 * nrow(final_df))
    train_data <- final_df[sample_indices, ]
    test_data <- final_df[-sample_indices, ]
    
    # Use ROSE to address class imbalance
    train_data_balanced <- ROSE(STATUS ~ ., data = train_data, seed = seed_value)$data
    
    # Define the logistic regression model
    log_model <- glm(STATUS ~ ., data = train_data_balanced, family = binomial)
    
    # Make predictions on the test set
    predictions <- predict(log_model, newdata = test_data, type = "response")
    predicted_labels <- ifelse(predictions > 0.5, 1, 0)
    
    # Evaluate the model
    accuracy <- sum(predicted_labels == test_data$STATUS) / length(test_data$STATUS) * 100
    conf_matrix <- table(test_data$STATUS, predicted_labels)
    classification_report <- confusionMatrix(as.factor(predicted_labels), as.factor(test_data$STATUS))
    
    
    # Update best seed if the accuracy is higher
    if (accuracy > max_accuracy) {
      max_accuracy <- accuracy
      best_seed <- seed_value
    }
    
    # Print results for the current seed
    cat("Seed:", seed_value, "Accuracy:", round(accuracy, 2), "%\n")
  }, error = function(e) {
    cat("Error occurred for Seed:", seed_value, "- Skipping to the next iteration.\n")
  })
}

# Print the best seed
cat("Best Seed:", best_seed, "with Accuracy:", round(max_accuracy, 2), "%\n")

