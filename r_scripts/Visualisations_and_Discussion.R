# Load required libraries
library(dplyr)
library(ggplot2)
library(corrplot)

# Load the dataset
thyroid_data <- read.csv("W:/Important Documents/Manan/2 UK/MSc/PBA/preprocessed_thyroid_data.csv")


# Data Exploration
# 1. Summary statistics for the dataset
summary_stats <- summary(thyroid_data)
print(summary_stats)

target_df <- thyroid_data %>%
  group_by(target) %>%
  summarise(count = n())

# Create a bar plot for the target distribution
plt <- ggplot(target_df, aes(x = target, y = count, fill = target)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(
    title = "Target Distribution",
    x = "Thyroid Conditions",
    y = "Number of People"
  ) +
  theme_minimal()

# Save the plot to a file
ggsave("target_distribution_plot.png", plt)

# Display the plot
print(plt)


# Visualizations

# 1. Map gender codes to proper gender categories
gender_map <- c("0" = "Female", "1" = "Male")
thyroid_data_copy$sex <- as.character(thyroid_data_copy$sex)
thyroid_data_copy$sex <- ifelse(thyroid_data_copy$sex %in% names(gender_map), gender_map[thyroid_data_copy$sex], "Unknown")

# Print updated dataset with gender mapping
print("Updated dataset with gender mapping:")
print(head(thyroid_data_copy))

# Visualize the distribution of the target column

# Create a histogram for the age column
age_plot <- ggplot(thyroid_data_copy, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = "Frequency"
  ) +
  theme_minimal()

# Save the plot to a file
ggsave("age_distribution_plot.png", age_plot)

# Display the plot
print(age_plot)

# Gender distribution visualization
gender_df <- thyroid_data %>%
  group_by(sex) %>%
  summarise(count = n())

# Map gender codes to labels
gender_df$sex <- factor(gender_df$sex, labels = c("Female", "Male"))

# Create a bar plot for gender distribution
gender_plot <- ggplot(gender_df, aes(x = sex, y = count, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Number of People"
  ) +
  theme_minimal()

# Save the plot to a file
ggsave("gender_distribution_plot.png", gender_plot)

# Display the plot
print(gender_plot)


# Boxplot for Age Distribution by Target
age_boxplot <- ggplot(thyroid_data_copy, aes(x = target, y = age, fill = target)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Age Group by Thyroid Conditions",
    x = "Thyroid Condition",
    y = "Age"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Save the boxplot to a file
ggsave("age_boxplot_by_target.png", age_boxplot)

# Display the boxplot
print(age_boxplot)


# Filter dataframes for self-reported conditions for Hypothyroid and Hyperthyroid
hypo_data <- thyroid_data %>% filter(query_hypothyroid == 1)
hyper_data <- thyroid_data %>% filter(query_hyperthyroid == 1)

# Count target variable occurrences
hypo_counts <- hypo_data %>% count(target)
hyper_counts <- hyper_data %>% count(target)

# Create pie charts
par(mfrow = c(1, 2))

# Pie chart for Hypothyroid
pie(
  hypo_counts$n, 
  labels = paste(hypo_counts$target, "(", round(hypo_counts$n / sum(hypo_counts$n) * 100, 2), "%)"), 
  main = "Self-Reported Hypothyroidism vs Actual Diagnosis", 
  col = rainbow(length(hypo_counts$n))
)

# Pie chart for Hyperthyroid
pie(
  hyper_counts$n, 
  labels = paste(hyper_counts$target, "(", round(hyper_counts$n / sum(hyper_counts$n) * 100, 2), "%)"), 
  main = "Self-Reported Hyperthyroidism vs Actual Diagnosis", 
  col = rainbow(length(hyper_counts$n))
)

# Reset plot layout
par(mfrow = c(1, 1))


###########################################################################################
# Discussion and Insights on the Preprocessed Dataset
###########################################################################################
# The preprocessed thyroid dataset is vital for ensuring accurate and reliable model performance.
# It contains three classes:
# - Class 1 (majority class)
# - Classes 2 and 3 (minority classes).
# The imbalance in class distribution makes it challenging to correctly identify Classes 2 and 3, 
# which are critical for medical diagnostics.

# Key Insights:
# 1. Class Imbalance:
#    - Most instances belong to Class 1, while Classes 2 and 3 are underrepresented.
#    - Accurate detection of minority classes is essential to avoid missed diagnoses.

# 2. Preprocessing:
#    - Numerical features were scaled to ensure fairness for algorithms sensitive to feature ranges.
#    - Categorical features were one-hot encoded for better model interpretation.
#    - These steps ensured the dataset was prepared for robust modeling.

# Metrics for Evaluation:

# In this medical dataset, focusing on Classes 2 and 3 is more important than overall accuracy.
# Metrics like precision and recall are crucial to evaluate how well the models identify these critical conditions.

# 1. Precision and Recall:
#    - Recall is key for Classes 2 and 3 to ensure true cases are not missed.
#    - Precision reduces false positives, ensuring predictions are trustworthy.

# 2. F1-Score:
#    - Balances precision and recall, making it a reliable metric for evaluating performance on minority classes.

# 3. Confusion Matrix:
#    - Helps identify misclassifications, especially false negatives in Classes 2 and 3.

# Conclusion:
# The preprocessing ensured the dataset was ready for accurate modeling. 
# For evaluation, metrics like recall, precision, and F1-score are crucial, as they focus on the critical minority classes.
# These metrics ensure models prioritize identifying thyroid conditions accurately, reducing risks in a medical context.