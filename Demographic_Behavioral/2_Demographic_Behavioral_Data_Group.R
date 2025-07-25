# Load Required Libraries
library(tidyverse)

# 1. Load Dataset
data <- read.csv("C:/Users/naomi/OneDrive/Desktop/R3/2_Demographic_Behavioral_data_Group_005.csv", stringsAsFactors = FALSE)

# 2. View Column Names
colnames(data)

# 3. Select Relevant Columns
data <- data %>%
  select(Patient.ID, Age, Sex, Weight_kg, Height_cm, BMI, Region, Socioeconomic,
         Education, Physical_Activity_Hours_Week, Smoking_Status, Drinking_Status,
         Patient_Satisfaction_Score, Health_Literacy_Score)

# 4. Convert Categorical Columns to Factor with Labels
data <- data %>%
  mutate(
    Sex = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Region = as.factor(Region),
    Socioeconomic = as.factor(Socioeconomic),
    Education = factor(Education, levels = c(0, 1, 2, 3), labels = c("Uneducated", "Primary", "Secondary", "Tertiary")),
    Smoking_Status = factor(Smoking_Status, levels = c(0, 1, 2), labels = c("Non-Smoker", "Occasional Smoker", "Chainsmoker")),
    Drinking_Status = factor(Drinking_Status, levels = c(0, 1, 2), labels = c("Non-drinker", "Casual drinker", "Heavy drinker"))
  )

# 5. Remove Missing Values
data <- data %>% drop_na()

# 6. Summary Statistics
summary(data)

# 7. Descriptive Statistics (Mean, Median, SD)
data %>%
  select(Age, Weight_kg, Height_cm, BMI, Physical_Activity_Hours_Week) %>%
  summarise_all(list(mean = mean, median = median, sd = sd), na.rm = TRUE)

# 8. Histogram: Weekly Physical Activity Hours
ggplot(data, aes(x = Physical_Activity_Hours_Week)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 12) +
  theme_minimal() +
  ggtitle("Histogram: Weekly Physical Activity Hours") +
  xlab("Physical Activity (Hours per Week)") +
  ylab("Number of Patients")

# 9. Boxplot: BMI by Drinking Status
ggplot(data, aes(x = Drinking_Status, y = BMI, fill = Drinking_Status)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Boxplot: BMI Distribution by Drinking Status") +
  xlab("Drinking Status") +
  ylab("Body Mass Index (BMI)") +
  scale_fill_brewer(palette = "Pastel2")

# 10. Scatter Plot: Health Literacy vs Satisfaction
ggplot(data, aes(x = Health_Literacy_Score, y = Patient_Satisfaction_Score)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  ggtitle("Scatter Plot: Health Literacy vs Patient Satisfaction") +
  xlab("Health Literacy Score (1 = Uneducated, 5 = Extremely Educated)") +
  ylab("Patient Satisfaction Score (1 = Unsatisfied, 5 = Extremely Satisfied)")

# 11. Barplot: Distribution of Education Levels
ggplot(data, aes(x = Education, fill = Education)) +
  geom_bar() +
  theme_minimal() +
  ggtitle("Bar Plot: Distribution of Education Levels") +
  xlab("Education Level") +
  ylab("Number of Patients") +
  scale_fill_brewer(palette = "Set2")

# 12. Advanced Test: Correlation between Satisfaction and Health Literacy
cor.test(data$Patient_Satisfaction_Score, data$Health_Literacy_Score)


