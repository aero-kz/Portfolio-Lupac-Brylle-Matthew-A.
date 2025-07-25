# --- 1. LOAD NECESSARY LIBRARIES ---
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(psych)

# --- 2. LOAD THE DATASET ---
data <- read.csv("C:/Users/naomi/OneDrive/Desktop/R3/3_Nutritional_Dietary_data_Group_005.csv")

# --- 3. CHECK STRUCTURE AND CLEAN HEADERS ---
str(data)

colnames(data) <- c(
  "Patient_ID", "Body_Fat", "Muscle_Mass", "BMI", "Physical_Activity_Hours",
  "Daily_Calories", "Protein_g", "Fat_g", "Carbs_g", "Vitamin_C_mg", "Iron_mg", "Water_ml"
)

glimpse(data)
colnames(data)

# --- 4. CHECK FOR MISSING VALUES ---
sum(is.na(data))  # Count of missing values

# --- 5. DESCRIPTIVE STATISTICS ---
summary_stats <- describe(data)
print(summary_stats)

# ===============================
# --- 6. DATA VISUALIZATIONS ---
# ===============================

# a. Histogram: BMI
ggplot(data, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  ggtitle("Histogram of BMI") +
  xlab("BMI") +
  ylab("Frequency")

# b. Boxplot: Body Fat by Physical Activity
ggplot(data, aes(x = as.factor(Physical_Activity_Hours), y = Body_Fat)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Boxplot: Body Fat vs Physical Activity Hours") +
  xlab("Hours of Physical Activity per Week") +
  ylab("Body Fat (%)")

# c. Scatter Plot: BMI vs. Body Fat
ggplot(data, aes(x = BMI, y = Body_Fat)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Scatter Plot: BMI vs Body Fat") +
  xlab("BMI") +
  ylab("Body Fat (%)")

# d. Bar Graph: Distribution of Water Intake
data$Water_Bin <- cut(
  data$Water_ml,
  breaks = c(0, 1000, 2000, 3000, 4000, Inf),
  labels = c("0–1000", "1001–2000", "2001–3000", "3001–4000", "4001+"),
  right = TRUE
)

ggplot(data, aes(x = Water_Bin)) +
  geom_bar(fill = "mediumpurple", color = "black", width = 0.7) +
  labs(
    title = "Distribution of Daily Water Intake",
    x = "Water Intake Range (ml)",
    y = "Number of Individuals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# ===============================
# --- 7. ADVANCED STATISTICAL TEST: ONE-WAY ANOVA ---
# ===============================

# a. Fat Intake Grouping
data$Fat_Group <- cut(
  data$Fat_g,
  breaks = quantile(data$Fat_g, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
  labels = c("Low Fat", "Medium Fat", "High Fat"),
  include.lowest = TRUE
)

# b. Carb Intake Grouping
data$Carb_Group <- cut(
  data$Carbs_g,
  breaks = quantile(data$Carbs_g, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
  labels = c("Low Carb", "Medium Carb", "High Carb"),
  include.lowest = TRUE
)

# c. One-Way ANOVA: Daily Calories ~ Fat Group
anova_fat <- aov(Daily_Calories ~ Fat_Group, data = data)
summary(anova_fat)

# d. One-Way ANOVA: Daily Calories ~ Carb Group
anova_carb <- aov(Daily_Calories ~ Carb_Group, data = data)
summary(anova_carb)

