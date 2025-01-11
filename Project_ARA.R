# Load necessary packages
library(haven)
library(tidyverse)
library(survey)
library(ggplot2)

# Read the datasets (replace with your actual file paths)
## Step 1: Load the datasets
sleep_data <- read_xpt("C:\\Users\\lasya\\OneDrive\\Desktop\\SLQ_J.XPT")
bmi_data <- read_xpt("C:\\Users\\lasya\\OneDrive\\Desktop\\BMX_J.XPT")
demo_data <- read_xpt("C:\\Users\\lasya\\OneDrive\\Desktop\\DEMO_J.XPT")

# Prepare data by selecting and renaming relevant columns
sleep_data_clean <- sleep_data %>%
  select(SEQN, SLD012, SLQ050) %>%
  rename(SleepHours = SLD012, SleepQuality = SLQ050)

bmi_data_clean <- bmi_data %>%
  select(SEQN, BMXBMI) %>%
  rename(BMI = BMXBMI)

demo_data_clean <- demo_data %>%
  select(SEQN, RIDAGEYR, RIAGENDR, SDMVPSU, SDMVSTRA, WTINT2YR) %>%
  rename(Age = RIDAGEYR, Gender = RIAGENDR, PSU = SDMVPSU, Strata = SDMVSTRA, Weight = WTINT2YR)

# Merge the cleaned datasets
merged_data <- reduce(list(sleep_data_clean, bmi_data_clean, demo_data_clean), full_join, by = "SEQN")

# Convert Gender and SleepQuality to factors
merged_data$Gender <- as.factor(merged_data$Gender)
merged_data$SleepQuality <- as.factor(merged_data$SleepQuality)

# Remove rows with missing SleepHours or BMI for cleaner analysis
merged_data <- merged_data %>% drop_na(SleepHours, BMI)

# ===================================================================
# DATA EXPLORATION - Before Model Building
# ===================================================================

# Histograms
ggplot(merged_data, aes(x = SleepHours)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Sleep Hours", x = "Sleep Hours", y = "Frequency")

ggplot(merged_data, aes(x = BMI)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  labs(title = "Histogram of BMI", x = "BMI", y = "Frequency")

# Scatter plot of BMI vs Sleep Hours
ggplot(merged_data, aes(x = SleepHours, y = BMI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Scatter Plot of BMI vs Sleep Hours", x = "Sleep Hours", y = "BMI")

# Scatter plot of BMI vs Age by Gender
ggplot(merged_data, aes(x = Age, y = BMI, color = Gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Scatter Plot of BMI vs Age by Gender", x = "Age", y = "BMI")

# ===================================================================
# MODEL BUILDING - Linear Regression
# ===================================================================

# Define survey design with nesting
survey_design <- svydesign(ids = ~PSU, strata = ~Strata, weights = ~Weight, data = merged_data, nest = TRUE)

# Linear Model
model_linear <- svyglm(BMI ~ SleepHours + SleepQuality + Age + Gender, design = survey_design)
summary(model_linear)

# ===================================================================
# MODEL BUILDING - Interaction Model
# ===================================================================

# Interaction Model
model_interaction <- svyglm(BMI ~ SleepHours * Gender + SleepHours * Age + SleepQuality * Gender + SleepQuality * Age, 
                            design = survey_design)
summary(model_interaction)

# ===================================================================
# MODEL COMPARISON
# ===================================================================

# AIC Comparison
AIC(model_linear, model_interaction)

# Likelihood Ratio Test (LRT)
#anova(model_linear, model_interaction, test = "LRT")
# Extract residual deviance
linear_deviance <- deviance(model_linear)
interaction_deviance <- deviance(model_interaction)

# Calculate test statistic
LRT_stat <- linear_deviance - interaction_deviance

# Degrees of freedom: difference in the number of parameters
df_diff <- length(coef(model_interaction)) - length(coef(model_linear))

# P-value
p_value <- pchisq(LRT_stat, df = df_diff, lower.tail = FALSE)

# Print results
cat("Likelihood Ratio Test Statistic:", LRT_stat, "\n")
cat("Degrees of Freedom:", df_diff, "\n")
cat("P-value:", p_value, "\n")


# ===================================================================
# MODEL DIAGNOSTICS - For Both Models
# ===================================================================

# Diagnostic plots
# Plotting fitted vs residuals to check homoscedasticity
ggplot(data.frame(fitted = fitted(model_linear), resid = residuals(model_linear, type = "pearson")), aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

# Normal Q-Q plot
qqnorm(residuals(model_linear, type = "pearson"))
qqline(residuals(model_linear, type = "pearson"), col = "red")

# Scale-Location Plot (Spread vs Level)
resid_sqrt <- sqrt(abs(residuals(model_linear, type = "pearson")))
ggplot(data.frame(fitted = fitted(model_linear), resid_sqrt = resid_sqrt), aes(x = fitted, y = resid_sqrt)) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  labs(title = "Scale-Location Plot", x = "Fitted Values", y = "Sqrt(|Residuals|)")

# Cook's distance plot to identify influential observations
cooks_distances <- cooks.distance(model_linear)
ggplot(data.frame(index = seq_along(cooks_distances), cooks = cooks_distances), aes(x = index, y = cooks)) +
  geom_bar(stat = "identity") +
  labs(title = "Cook's Distance Plot", x = "Observation", y = "Cook's Distance")

# Diagnostics for Interaction Model
ggplot(data.frame(fitted = fitted(model_interaction), resid = residuals(model_interaction, type = "pearson")), 
       aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values (Interaction Model)", x = "Fitted Values", y = "Residuals")

qqnorm(residuals(model_interaction, type = "pearson"))
qqline(residuals(model_interaction, type = "pearson"), col = "red")

# Calculate the square root of absolute residuals for the interaction model
resid_sqrt_interaction <- sqrt(abs(residuals(model_interaction, type = "pearson")))

# Create the Scale-Location Plot
ggplot(data.frame(fitted = fitted(model_interaction), resid_sqrt = resid_sqrt_interaction), 
       aes(x = fitted, y = resid_sqrt)) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  labs(title = "Scale-Location Plot (Interaction Model)", 
       x = "Fitted Values", 
       y = "Sqrt(|Residuals|)") +
  theme_minimal()


# Calculate Cook's distance for the interaction model
cooks_distances_interaction <- cooks.distance(model_interaction)

# Create a Cook's Distance Plot
ggplot(data.frame(index = seq_along(cooks_distances_interaction), cooks = cooks_distances_interaction), 
       aes(x = index, y = cooks)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Cook's Distance Plot (Interaction Model)", 
       x = "Observation", 
       y = "Cook's Distance") +
  geom_hline(yintercept = 4 / nrow(merged_data), color = "red", linetype = "dashed", 
             lwd = 1, label = "Threshold") +
  theme_minimal()


# ===================================================================
# VISUALIZING INTERACTIONS
# ===================================================================

# BMI vs SleepHours by Gender
ggplot(merged_data, aes(x = SleepHours, y = BMI, color = Gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Interaction: BMI vs Sleep Hours by Gender")

# BMI vs SleepHours by Age Group
merged_data <- merged_data %>% mutate(AgeGroup = cut(Age, breaks = c(16, 30, 50, 70, 150), 
                                                     labels = c("16-30", "31-50", "51-70", "71+")))
ggplot(merged_data, aes(x = SleepHours, y = BMI, color = AgeGroup)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Interaction: BMI vs Sleep Hours by Age Group")