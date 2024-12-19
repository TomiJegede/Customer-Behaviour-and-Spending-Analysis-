# Project Milestone 2, ALY 6010, 2/12/2024

# The clean data set was exported from the previous script and 
# read into this script for further analysis.

data <- read.csv("customer_satisfaction_survey_clean.csv")

# Hypothesis Testing

# Part 1: one sample t-test
#Question: Does the average Spending_Score of RetailCo's customers differ 
#significantly from the industry standard of 50?
# This test assumes that the mean 'spending_score' is 50

# Hypothesis
# H0: Mean Spending_Score <= 50
# H1: Mean Spending_Score > 50

# Define population mean
pop_mean <- 50

# Calculate sample statistics
sample_mean <- mean(data$Spending_Score)
sample_sd <- sd(data$Spending_Score)
n <- length(data$Spending_Score)
df <- n-1

# Calculate the standard error
standard_error <- sample_sd / sqrt(n)

# Calculate the t-value
t_value <- (sample_mean - pop_mean) / standard_error

# Calculate the p-value
p_value <- 1 - pt(t_value, df)

# Output the results
cat("T Test Value:", t_value, "\n")
cat("P Test Value:", p_value, "\n")

# Decision to accept or reject null hypothesis
decision <- ifelse(p_value < 0.05, 
                   "Reject null hypothesis: The mean Spending_Score is significantly less than 50.",
                   "Fail to reject null hypothesis: The mean Spending_Score is not significantly less than 50.")
decision

# Part 2: Two-tailed test
#Question: Is there a significant difference in Annual_Income between customers 
#in the "Gold" and "Platinum" loyalty tiers?

# Hypothesis
# H0: Mean Annual_Income (Gold) = Mean Annual_Income (Platinum)
# H1: Mean Annual_Income (Gold) ≠ Mean Annual_Income (Platinum)

# Subset data for gold and platinum loyalty tiers
gold_data <- subset(data, Loyalty_Tier == 'Gold')$Annual_Income
platinum_data <- subset(data, Loyalty_Tier == 'Platinum')$Annual_Income

# calculate the sample statistics
n1 <- length(gold_data) # sample size for gold
n2 <- length(platinum_data) # sample size for platinum
mean1 <- mean(gold_data) # mean for gold
mean2 <- mean(platinum_data) # mean for platinum
sd1 <- sd(gold_data) #sd for gold
sd2 <- sd(platinum_data) #sd for platinum

# Calculate pooled standard deviation
pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

# Calculate standard error of the difference in means
standard_error <- pooled_sd * sqrt(1 / n1 + 1 / n2)

# Calculate the t-value
t_value <- (mean1 - mean2) / standard_error

# Calculate the degrees of freedom
df <- n1 + n2 - 2

# Calculate the p-value for a two-tailed test
p_value <- 2 * (1 - pt(abs(t_value), df))

# output the results
cat("T-Value: ", t_value, "\n")
cat("P-Value: ", p_value, "\n")

# decision
if (p_value < 0.05) {
  cat("Reject null hypothesis: The mean Annual_Income is significantly different between Gold and Platinum loyalty tiers.\n")
} else {
  cat("Fail to reject null hypothesis: The mean Annual_Income is not significantly different between Gold and Platinum loyalty tiers.\n")
}

# Part 3: Two-tailed test
# Question: Is there a significant difference in the proportion of customers in 
#the "High" income bracket between two specific regions (e.g., North and South)?

# Hypothesis  
# H0:The proportion of "High" income customers is the same in the two regions.
# H1:The proportion of "High" income customers is different in the two regions.

# Subset the data for North and South regions
north_data <- subset(data, Region == "North")
south_data <- subset(data, Region == "South")

# Calculate the proportions of "High" income customers in each region
p1 <- mean(north_data$Income_Bracket == "High")
p2 <- mean(south_data$Income_Bracket == "High")

# Calculate the sample sizes
n1 <- nrow(north_data)
n2 <- nrow(south_data)

# Calculate the pooled proportion
p_pool <- (sum(north_data$Income_Bracket == "High") + sum(south_data$Income_Bracket == "High")) / (n1 + n2)

# Calculate the z-score
z_score <- (p1 - p2) / sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))

# Calculate the p-value for a two-tailed test
p_value <- 2 * (1 - pnorm(abs(z_score)))

# Output the results
cat("Z-Score:", z_score, "\nP-Value:", p_value)

# decision
if (p_value < 0.05) {
  cat("Reject null hypothesis: The proportion of 'High' income customers is significantly different between North and South.\n")
} else {
  cat("Fail to reject null hypothesis: The proportion of 'High' income customers is not significantly different between North and South.\n")
}

# Part 4: Two-tailed test
# Question: Is there a significant difference in the proportion of male and 
# female customers in the "High" spending category?

# Hypotheses:
# H0: The proportion of male and female customers in the "High" spending category is the same.
# H1: The proportion of male and female customers in the "High" spending category is different.

# Subset data by Gender
male_data <- subset(data, Gender == "Male")
female_data <- subset(data, Gender == "Female")

# Proportions for "High" spending category
p1 <- mean(male_data$Spending_Category == "High")
p2 <- mean(female_data$Spending_Category == "High")

# Sample sizes
n1 <- nrow(male_data)
n2 <- nrow(female_data)

# Pooled proportion
p_pool <- (sum(male_data$Spending_Category == "High") + sum(female_data$Spending_Category == "High")) / (n1 + n2)

# Z-score calculation
z_score <- (p1 - p2) / sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))

# P-value for two-tailed test
p_value <- 2 * (1 - pnorm(abs(z_score)))

# Output results
cat("Z-Score:", z_score, "\nP-Value:", p_value)

# Decision Rule
if (p_value < 0.05) {
  cat("Reject null hypothesis: The proportion of male and female customers in the 'High' spending category is significantly different.\n")
} else {
  cat("Fail to reject null hypothesis: The proportion of male and female customers in the 'High' spending category is not significantly different.\n")
}

# Part 5: one-tailed test
# Question: Does the mean age of male customers differ significantly and is it 
#less than the mean age of female customers?

#Hypothesis
# H0:The mean age of male customers is greater than or equal to the mean age of female customers.
# H1:The mean age of male customers is less than the mean age of female customers.

# Filter data for male and female customers
male_age <- subset(data, Gender == "Male")$Age
female_age <- subset(data, Gender == "Female")$Age

# Calculate sample statistics
n1 <- length(male_age)  # Sample size for males
n2 <- length(female_age)  # Sample size for females

mean1 <- mean(male_age, na.rm = TRUE)  # Mean for males
mean2 <- mean(female_age, na.rm = TRUE)  # Mean for females

sd1 <- sd(male_age, na.rm = TRUE)  # Standard deviation for males
sd2 <- sd(female_age, na.rm = TRUE)  # Standard deviation for females

# Pooled standard deviation (assuming equal variances)
pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

# Calculate standard error of the difference in means
standard_error <- pooled_sd * sqrt(1 / n1 + 1 / n2)

# Calculate the t-value
t_value <- (mean1 - mean2) / standard_error

# Degrees of freedom
df <- n1 + n2 - 2

# Calculate the p-value for a left-tailed test
p_value <- pt(t_value, df)  # Left-tailed test

# Output the results
cat("T Test Value:", t_value, "\n")
cat("P Test Value:", p_value, "\n")

# Decision
if (p_value < 0.05) {
  cat("Reject null hypothesis: The mean age of male customers is significantly less than the mean age of female customers.\n")
} else {
  cat("Fail to reject null hypothesis: The mean age of male customers is not significantly less than the mean age of female customers.\n")
}