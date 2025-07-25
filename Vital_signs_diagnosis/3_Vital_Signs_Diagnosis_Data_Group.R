# Load libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(psych)
library(ggpubr)

# Load Dataset
data <- read.csv("C:/Users/naomi/OneDrive/Desktop/R3/1_Vital_signs_diagnosis_data_Group_005.csv", stringsAsFactors = FALSE)

# View structure
glimpse(data)

# Convert columns to numeric
data <- data %>%
  mutate(
    Age = as.numeric(Age),
    Weight_kg = as.numeric(Weight_kg),
    Height_cm = as.numeric(Height_cm),
    BMI = as.numeric(BMI),
    Systolic_BP = as.numeric(Systolic_BP),
    Diastolic_BP = as.numeric(Diastolic_BP),
    Heart_rate = as.numeric(Heart_rate),
    Glucose_mg.dL = as.numeric(Glucose_mg.dL),
    Cholesterol_mg.dL = as.numeric(Cholesterol_mg.dL),
    Daily_Sleeping_hours = as.numeric(Daily_Sleeping_hours),
    Sex = factor(Sex, levels = c(0, 1), labels = c("Female", "Male"))
  )

# Descriptive Statistics
summary_stats <- data %>%
  select(Age, Weight_kg, Height_cm, BMI, Systolic_BP, Diastolic_BP,
         Heart_rate, Glucose_mg.dL, Cholesterol_mg.dL, Daily_Sleeping_hours) %>%
  psych::describe()

print(summary_stats)

# Boxplot: BMI by Sex
ggplot(data, aes(x = Sex, y = BMI, fill = Sex)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "BMI Distribution by Sex",
    subtitle = "Comparison of BMI between Female and Male patients",
    x = "Sex",
    y = "Body Mass Index (BMI)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Histogram: Cholesterol Levels
ggplot(data, aes(x = Cholesterol_mg.dL)) +
  geom_histogram(fill = "darkorange", color = "black", bins = 15) +
  labs(
    title = "Distribution of Cholesterol Levels",
    subtitle = "Cholesterol (mg/dL) across all patients",
    x = "Cholesterol (mg/dL)",
    y = "Number of Patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Rounded Sleep Hours for Bar Plot
data$Sleep_Hours_Rounded <- round(data$Daily_Sleeping_hours)

# Bar Plot: Rounded Sleep Hours
ggplot(data, aes(x = as.factor(Sleep_Hours_Rounded))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Frequency of Rounded Daily Sleeping Hours",
    subtitle = "Sleep duration distribution among patients",
    x = "Sleep Hours (Rounded)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Scatter Plot: Age vs Heart Rate
ggplot(data, aes(x = Age, y = Heart_rate)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE, linewidth = 1.2) +
  labs(
    title = "Relationship Between Age and Heart Rate",
    subtitle = "Red line shows linear regression with 95% confidence interval",
    x = "Age",
    y = "Heart Rate (bpm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Correlation Test: Systolic vs Diastolic BP
cor_test <- cor.test(data$Systolic_BP, data$Diastolic_BP)
print(cor_test)


