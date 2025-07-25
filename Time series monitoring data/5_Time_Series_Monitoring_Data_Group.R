# Load required libraries
library(tidyverse)
library(janitor)
library(ggplot2)
library(modeest)

# 1. Load the dataset
data <- read.csv("C:/Users/naomi/OneDrive/Desktop/R3/4_Time_series_Mointoring_data_Group_005.csv")

# 2. Clean column names
data <- clean_names(data)

# 3. Check structure
str(data)

# 4. Handle missing values
data <- na.omit(data)

# 5. Convert all necessary columns to numeric just in case
data <- data %>% mutate(across(everything(), as.numeric))

# --- 6. DESCRIPTIVE STATISTICS FUNCTION ---
describe_variable <- function(x) {
  list(
    Mean = mean(x),
    Median = median(x),
    SD = sd(x),
  )
}

# Apply the function to each numeric column
desc_stats <- lapply(data[,-1], describe_variable)
desc_stats

# --- 7. DATA VISUALIZATIONS ---

# a. Histogram of Patient 1 Average Steps
ggplot(data, aes(x = patient_1_avg_steps)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 15) +
  ggtitle("Histogram: Patient 1 Average Steps")

# b. Boxplot of BMI across 3 Patients
data_long_bmi <- data %>%
  pivot_longer(cols = starts_with("patient_") & ends_with("bmi"),
               names_to = "Patient", values_to = "BMI")
ggplot(data_long_bmi, aes(x = Patient, y = BMI, fill = Patient)) +
  geom_boxplot() +
  ggtitle("Boxplot: BMI of Patients")

# c. Scatter plot: Steps vs Stress Level for Patient 1
ggplot(data, aes(x = patient_1_avg_steps, y = patient_1_stress_level)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter: Patient 1 Steps vs Stress Level")

# d. Bar Graph: Average Stress Levels per Patient
avg_stress <- data %>%
  summarise(across(contains("stress_level"), mean)) %>%
  pivot_longer(everything(), names_to = "Patient", values_to = "Avg_Stress")
ggplot(avg_stress, aes(x = Patient, y = Avg_Stress, fill = Patient)) +
  geom_col() +
  ggtitle("Bar Graph: Average Stress Level per Patient")

# --- 8. ADVANCED STATISTICAL TEST ---

# Pearson Correlation: Steps vs Stress for each patient
cor_p1 <- cor.test(data$patient_1_avg_steps, data$patient_1_stress_level)
cor_p2 <- cor.test(data$patient_2_avg_steps, data$patient_2_stress_level)
cor_p3 <- cor.test(data$patient_3_avg_steps, data$patient_3_stress_level)

# Print results
cat("Patient 1 correlation:", cor_p1$estimate, "p-value:", cor_p1$p.value, "\n")
cat("Patient 2 correlation:", cor_p2$estimate, "p-value:", cor_p2$p.value, "\n")
cat("Patient 3 correlation:", cor_p3$estimate, "p-value:", cor_p3$p.value, "\n")

