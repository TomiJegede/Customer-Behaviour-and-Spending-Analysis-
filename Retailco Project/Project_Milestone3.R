# Project Milestone 3, ALY 6010, 14/12/2024

# Upload the data set

data <- read.csv("customer_satisfaction_survey_clean.csv")

# Question 1: Is there a significant relationship between Annual_Income and Spending_Score?

# Dependent Variable: Spending_Score (Customer spending score).
# Independent Variable: Annual_Income (Customer annual income). 

# Null Hypothesis (𝐻0): There is no significant relationship between Annual_Income and Spending_Score.
# Alternative Hypothesis (𝐻𝑎): There is a significant relationship between Annual_Income and Spending_Score.

# Calculate means
mean_x <- mean(data$Annual_Income)
mean_y <- mean(data$Spending_Score)

# Calculate slope (beta1)
numerator <- sum((data$Annual_Income - mean_x) * (data$Spending_Score - mean_y))
denominator <- sum((data$Annual_Income - mean_x)^2)
beta1 <- numerator / denominator

# Calculate intercept (beta0)
beta0 <- mean_y - beta1 * mean_x

# Calculate predicted Spending_Score
data$predicted_y <- beta0 + beta1 * data$Annual_Income

# Residuals
data$residuals <- data$Spending_Score - data$predicted_y

# Standard Error of Regression
SE <- sqrt(sum(data$residuals^2) / (nrow(data) - 2))

# Hypothesis Testing
# t = beta1 / SE_beta1
SE_beta1 <- SE / sqrt(denominator)
t_value <- beta1 / SE_beta1

# Degrees of freedom
df <- nrow(data) - 2

# p-value
p_value <- 2 * (1 - pt(abs(t_value), df))

# Results
cat("Beta1 (Slope):", beta1, "\n")
cat("T-Value:", t_value, "\n")
cat("P-Value:", p_value, "\n")

# Decision
if (p_value < 0.05) {
  cat("Reject null hypothesis: There is a significant relationship between Annual Income and Spending Score.\n")
} else {
  cat("Fail to reject null hypothesis: No significant relationship between Annual Income and Spending Score.\n")
} 

# Scatterplot of Annual_Income vs. Spending_Score
library(ggplot2)

ggplot(data, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Annual Income and Spending Score",
       x = "Annual Income",
       y = "Spending Score") +
  theme_minimal()

# Question 2: Does Purchase_Frequency significantly differ across Loyalty_Tier?
# Dependent Variable: Purchase_Frequency (Number of purchases per year).
# Independent Variable: Loyalty_Tier (Categorical variable with different loyalty levels)

# Null Hypothesis (𝐻0): There is no difference in Purchase_Frequency across loyalty tiers.
# Alternative Hypothesis (𝐻𝑎): There is a difference in Purchase_Frequency across loyalty tiers.

# Overall mean
grand_mean <- mean(data$Purchase_Frequency)

# Means for each group
group_means <- tapply(data$Purchase_Frequency, data$Loyalty_Tier, mean)

# Total Sum of Squares (SST)
sst <- sum((data$Purchase_Frequency - grand_mean)^2)

# Ensure Loyalty_Tier is a factor
data$Loyalty_Tier <- as.factor(data$Loyalty_Tier)

# Compute Between-Group Sum of Squares (SSB)
ssb <- sum(sapply(unique(data$Loyalty_Tier), function(tier) {
  n_tier <- sum(data$Loyalty_Tier == tier)  # Count of observations in each tier
  mean_tier <- group_means[tier]           # Group mean for the tier
  n_tier * (mean_tier - grand_mean)^2      # SSB contribution for the tier
}))

# Within-Group Sum of Squares (SSW)
ssw <- sst - ssb

# Degrees of freedom
df_between <- length(group_means) - 1
df_within <- nrow(data) - length(group_means)

# Mean squares
ms_between <- ssb / df_between
ms_within <- ssw / df_within

# F-statistic
f_stat <- ms_between / ms_within

# p-value
p_value <- 1 - pf(f_stat, df_between, df_within)

# Results
cat("F-Statistic:", f_stat, "\n")
cat("P-Value:", p_value, "\n")

# Decision
if (p_value < 0.05) {
  cat("Reject null hypothesis: At least one Loyalty Tier has a significantly different Purchase Frequency.\n")
} else {
  cat("Fail to reject null hypothesis: No significant difference in Purchase Frequency across Loyalty Tiers.\n")
}

# Boxplot of Purchase_Frequency by Loyalty_Tier
ggplot(data, aes(x = Loyalty_Tier, y = Purchase_Frequency, fill = Loyalty_Tier)) +
  geom_boxplot() +
  labs(title = "Purchase Frequency Across Loyalty Tiers",
       x = "Loyalty Tier",
       y = "Purchase Frequency") +
  theme_minimal()


# Question 3: Is there a significant relationship between Age and Spending_Score?
# Dependent Variable: Purchase_Frequency (Number of purchases per year).
# Independent Variable: Annual_Income (Customer annual income).

# Null Hypothesis (𝐻0): There is no significant relationship between Purchase_Frequency and Annual_Income.
# Alternative Hypothesis (𝐻𝑎): There is a significant relationship between Purchase_Frequency and Annual_Income.

# Calculate means for Age and Spending_Score
age_mean <- mean(data$Age)
spending_mean <- mean(data$Spending_Score)

# Calculate intermediate values for the slope (beta_1)
data$diff_age <- data$Age - age_mean
data$diff_spending <- data$Spending_Score - spending_mean
data$xy_product <- data$diff_age * data$diff_spending
data$x_squared <- data$diff_age^2

# Compute beta_1 (slope)
beta_1 <- sum(data$xy_product) / sum(data$x_squared)

# Calculate beta_0 (intercept)
beta_0 <- spending_mean - beta_1 * age_mean

# Display the final linear regression equation
cat("Linear Regression Equation: Spending_Score =", round(beta_0, 2), "+", round(beta_1, 2), "* Age\n")

# Predict Spending_Score for specific ages
# Example: Predict spending score for age 30
new_age <- 30
predicted_spending <- beta_0 + beta_1 * new_age
cat("Predicted Spending Score for Age", new_age, ":", round(predicted_spending, 2), "\n")

# Example: Predict spending score for age 50
new_age <- 50
predicted_spending <- beta_0 + beta_1 * new_age
cat("Predicted Spending Score for Age", new_age, ":", round(predicted_spending, 2), "\n")

# Visualization
ggplot(data, aes(x = Age, y = Spending_Score)) +
  geom_point(colour = 'blue', size = 2) +
  geom_abline(intercept = beta_0, slope = beta_1, colour = 'red') +
  labs(
    title = "Spending Score vs. Age",
    x = "Age",
    y = "Spending Score"
  ) +
  theme_minimal()

# Question 4: Is there a significant relationship between Purchase_Frequency and Annual_Income?
# Dependent Variable: Purchase_Frequency (Number of purchases per year).
# Independent Variable: Spending_Score (Customer spending score). 

# Hypothesis:

# Null Hypothesis (𝐻0): There is no significant relationship between Purchase_Frequency and Spending_Score.
# Alternative Hypothesis (𝐻𝑎): There is a significant relationship between Purchase_Frequency and Spending_Score.

# Calculate means for Purchase_Frequency and Annual_Income
freq_mean <- mean(data$Purchase_Frequency)
income_mean <- mean(data$Annual_Income)

# Calculate intermediate values for the slope (beta_1)
data$diff_freq <- data$Purchase_Frequency - freq_mean
data$diff_income <- data$Annual_Income - income_mean
data$xy_product <- data$diff_freq * data$diff_income
data$x_squared <- data$diff_freq^2

# Compute beta_1 (slope)
beta_1 <- sum(data$xy_product) / sum(data$x_squared)

# Calculate beta_0 (intercept)
beta_0 <- income_mean - beta_1 * freq_mean

# Display the final linear regression equation
cat("Linear Regression Equation: Annual_Income =", round(beta_0, 2), "+", round(beta_1, 2), "* Purchase_Frequency\n")

# Predict Annual_Income for specific Purchase_Frequency values
# Example: Predict income for purchase frequency of 5
new_freq <- 5
predicted_income <- beta_0 + beta_1 * new_freq
cat("Predicted Annual Income for Purchase Frequency", new_freq, ":", round(predicted_income, 2), "\n")

# Visualization
ggplot(data, aes(x = Purchase_Frequency, y = Annual_Income)) +
  geom_point(colour = 'green', size = 2) +
  geom_abline(intercept = beta_0, slope = beta_1, colour = 'purple') +
  labs(
    title = "Annual Income vs. Purchase Frequency",
    x = "Purchase Frequency",
    y = "Annual Income"
  ) +
  theme_minimal()
