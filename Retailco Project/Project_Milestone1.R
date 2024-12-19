# Project Milestone 1, ALY 6010, 12/11/2024

# load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv("customer_satisfaction_survey.csv")

# Initial Data Inspection
head(df)
summary(df)
str(df)

# Check for missing values
colSums(is.na(df))

#Handle missing values(basic imputation)
df_clean <- df %>%
  mutate(
    Annual_Income = ifelse(is.na(Annual_Income), median(Annual_Income, na.rm =TRUE), Annual_Income),
    Spending_Score = ifelse(is.na(Spending_Score), median(Spending_Score, na.rm=TRUE), Spending_Score),
    Purchase_Frequency = ifelse(is.na(Purchase_Frequency), median(Purchase_Frequency, na.rm=TRUE), Purchase_Frequency)
  )

# Check that there are no missing values
colSums(is.na(df_clean))

#Check for outliers in the numerical columns
iqr_age <- IQR(df_clean$Age, na.rm=TRUE)
iqr_income <- IQR(df_clean$Annual_Income, na.rm=TRUE)
iqr_score <- IQR(df_clean$Spending_Score, na.rm=TRUE)

#Filter out outliers
df_clean <- df_clean %>%
  filter(
    Age >= (quantile(Age, 0.25, na.rm=TRUE) - 1.5 * iqr_age) &
    Age <= (quantile(Age, 0.75, na.rm=TRUE) + 1.5 * iqr_age),
    Annual_Income >= (quantile(Annual_Income, 0.25, na.rm=TRUE) - 1.5 * iqr_income) &
    Annual_Income <= (quantile(Annual_Income, 0.75, na.rm=TRUE) + 1.5 * iqr_income),
    Spending_Score >= (quantile(Spending_Score, 0.25, na.rm=TRUE) - 1.5 * iqr_score) &
    Spending_Score <= (quantile(Spending_Score, 0.75, na.rm=TRUE) + 1.5 * iqr_score)
  )

#Descriptive Statistics
mean_age <- mean(df_clean$Age, na.rm=TRUE)
median_income <- median(df_clean$Annual_Income, na.rm=TRUE)
sd_score <- sd(df_clean$Spending_Score, na.rm=TRUE)

mean_age
median_income
sd_score

#Basic Visualizations for EDA
#Age Distribution
ggplot(df_clean, aes(x=Age)) +
  geom_histogram(binwidth=5) +
  ggtitle("Age Distribution")

#Income box plot
ggplot(df_clean, aes(y = Annual_Income)) +
  geom_boxplot() +
  ggtitle("Annual Income Boxplot")

# Purchase frequency distribution
ggplot(df_clean, aes(x = Purchase_Frequency)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Purchase Frequency", 
       x = "Number of Purchases per Year", 
       y = "Count") +
  theme_minimal()

# Scatter plot for Spending Score vs. Annual Income
ggplot(df_clean, aes(x=Annual_Income, y=Spending_Score)) +
  geom_point(alpha = 0.5) +
  ggtitle("Spending Score vs. Annual Income")

# Distribution of customers by region
ggplot(df_clean, aes(x= Region)) +
  geom_bar(fill= "skyblue") +
  labs(
    title = "Customer Distribution by Region", 
    x = "Region", 
    y = "Number of Customers") +
  theme_minimal()

#Distribution of customers by loyalty tier
ggplot(df_clean, aes(x = Loyalty_Tier)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Customer Distribution by Loyalty Tier", x = "Loyalty Tier", y = "Number of Customers") +
  theme_minimal()

# Scatter plot of Annual Income vs. Spending Score, colored by Loyalty Tier
ggplot(df_clean, aes(x = Annual_Income, y = Spending_Score, color = Loyalty_Tier)) +
  geom_point(alpha = 0.7) +
  labs(title = "Annual Income vs. Spending Score by Loyalty Tier", x = "Annual Income", y = "Spending Score") +
  theme_minimal()

# Gender distribution within each Loyalty Tier
df_clean %>%
  group_by(Gender, Loyalty_Tier) %>%
  summarise(Customer_Count = n()) %>%
  ggplot(aes(x = Gender, y = Customer_Count, fill = Loyalty_Tier)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Customer Count by Gender and Loyalty Tier", x = "Gender", y = "Number of Customers") +
  theme_minimal()

# Box plot for Spending Score by Region
ggplot(df_clean, aes(x = Region, y = Spending_Score, fill = Region)) +
  geom_boxplot() +
  labs(title = "Spending Score by Region", x = "Region", y = "Spending Score") +
  theme_minimal()

# Creating new columns for additional insights
# Income Brackets
df_clean <- df_clean %>%
  mutate(
    Income_Bracket = case_when(
      Annual_Income < 30000 ~ "Low",
      Annual_Income >= 30000 & Annual_Income < 70000 ~ "Medium",
      Annual_Income >= 70000 ~ "High"
    )
  )

# High vs. low spending score
df_clean <- df_clean %>%
  mutate(
    Spending_Category = ifelse(Spending_Score > median(Spending_Score, na.rm=TRUE), "High", "Low")
  )

#Age grouping
df_clean <- df_clean %>%
  mutate(
    Age_Group = case_when(
      Age < 20 ~ "Teen",
      Age >= 20 & Age < 40 ~ "Young Adult",
      Age >= 40 & Age < 60 ~ "Adult",
      Age >= 60 ~ "Senior"
    )
  )

# Distribution of customers across Income Brackets
ggplot(df_clean, aes(x = Income_Bracket)) +
  geom_bar(fill = "skyblue") +
  labs(
    title = "Distribution of Customers by Income Bracket", 
    x = "Income Bracket", 
    y = "Count")

#Age group vs. Income brackets
ggplot(df_clean, aes(x = Age_Group, fill = Income_Bracket)) +
  geom_bar(position = "fill") +
  labs(
    title = "Income Bracket Distribution Across Age Groups", 
    x = "Age Group", 
    y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format())

# High vs. low spending score
df_clean %>%
  count(Spending_Category) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = n, fill = Spending_Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) + # Position labels inside the pie
  labs(title = "High vs. Low Spending Score") +
  theme_void()

# export the clean dataset
write.csv(df_clean, file="customer_satisfaction_survey_clean.csv")

