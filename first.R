library(readr)
library(dplyr)

# Set the working directory to the location of your datasets
# setwd("/path/to/your/directory")

# Read the application_record.csv file
app_data <- read_csv("application_record.csv")

# Display the structure of the application dataset
str(app_data)

# Read the credit_record.csv file
credit_data <- read_csv("credit_record.csv")

# Display the structure of the credit dataset
str(credit_data)

# Merge the two datasets based on the ID column
merged_data <- inner_join(app_data, credit_data, by = "ID")

# Display the structure of the merged dataset
str(merged_data)

# Display the first few rows of the merged dataset
head(merged_data)

write_csv(merged_data, "merged_data.csv")

print(merged_data)


# Install and load necessary libraries
# install.packages("readr")  # Uncomment and run if 'readr' is not installed
# install.packages("dplyr")  # Uncomment and run if 'dplyr' is not installed
# Install and load necessary libraries
# install.packages("readr")  # Uncomment and run if 'readr' is not installed
# install.packages("dplyr")  # Uncomment and run if 'dplyr' is not installed
library(readr)
library(dplyr)

# Set the working directory to the location of your datasets
# setwd("/path/to/your/directory")

# Read the merged_data.csv file
merged_data <- read_csv("merged_data.csv")

# Display the column names of the merged dataset
colnames(merged_data)

# Display the first few rows of the merged dataset
head(merged_data)

# Perform additional data transformations or calculations
# Check the column names and adjust accordingly
merged_data <- merged_data %>%
  mutate(
    AGE = -(AGE / 365),  # Assuming AGE is already in years
    EMPLOYMENT_LENGTH = ifelse(EMPLOYMENT_LENGTH == 365243, NA, -(EMPLOYMENT_LENGTH / 365))
  )

# Display the updated structure and first few rows of the dataset
str(merged_data)
head(merged_data)

# Save the modified dataset to a new CSV file
write_csv(merged_data, "modified_merged_data.csv")


##############################################################################


library(readr)
library(dplyr)
install.packages("glmnet")  # Uncomment and run to install 'glmnet'
library(glmnet)

# Set the working directory to the location of your datasets
# setwd("/path/to/your/directory")

# Read the modified_merged_data.csv file
merged_data <- read_csv("modified_merged_data.csv")

# Display the structure and first few rows of the dataset
str(merged_data)
head(merged_data)

# Perform logistic regression
# Assuming you want to predict the 'HAS_PHONE' variable
# Adjust the formula and response variable as needed
model <- glm(HAS_PHONE ~ AGE + EMPLOYMENT_LENGTH + CNT_CHILDREN + AMT_INCOME_TOTAL + CNT_FAM_MEMBERS, 
             data = merged_data, 
             family = binomial)

# Display the summary of the logistic regression model
summary(model)

