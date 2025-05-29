# Install necessary libraries
install.packages("tidyverse")  # For data manipulation
install.packages("skimr")      # For detailed data inspection
install.packages("VIM")        # For missing data visualization

# Load libraries
library(tidyverse)
library(skimr)
library(VIM)

# Step 1: Load the Dataset
data <- read.csv("W:/Important Documents/Manan/2 UK/MSc/PBA/thyroidDF.csv")

# Step 2: Basic Data Inspection
# Check the structure of the dataset
str(data)

# View summary statistics
summary(data)

# View first few rows
head(data)

# Step 3: Check for Missing Values
# Count missing values per column
colSums(is.na(data))

# Visualize missing data
aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

# Step 4: Handle Missing Values
# Impute missing numerical values with mean
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Impute missing categorical values with the mode
mode_impute <- function(x) {
  x[is.na(x)] <- names(sort(table(x), decreasing = TRUE))[1]
  return(x)
}
data <- data %>%
  mutate(across(where(is.character), mode_impute))

# Step 5: Check for Outliers (Boxplot Method)
boxplot(data$age, main="Boxplot for Outliers", col="lightblue")  # Replace 'SomeNumericColumn' with actual column names

# Step 6: Normalize Numerical Features (Optional)
# Scaling numerical columns between 0 and 1
data <- data %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.numeric()))

# Step 7: Encode Categorical Variables (One-Hot Encoding)
# Convert categorical variables to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Create dummy variables
data <- model.matrix(~.-1, data = data) %>%
  as.data.frame()

# Step 8: Save Preprocessed Dataset
write.csv(data, "preprocessed_thyroidDF.csv", row.names = FALSE)

# Print success message
print("Data loading and preprocessing completed. Preprocessed data saved as 'preprocessed_thyroidDF.csv'.")
