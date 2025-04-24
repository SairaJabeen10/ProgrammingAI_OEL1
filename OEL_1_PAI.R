library(tidyverse)

# Load data
data <- read_csv("customer_churn.csv")

# Number of customers who have churned
data %>%
  filter(Churn == "Yes") %>%
  nrow()

# Create a new column ChargeGap as TotalCharges
data <- data %>%
  mutate(TotalCharges = as.numeric(TotalCharges),
         ChargeGap = TotalCharges - (MonthlyCharges * Tenure))
view(data)

#Filter customers
filtered_customers <- data %>%
  filter(Tenure > 24, Churn == "No")
view(filtered_customers)

#Find the average MonthlyCharges for each ContractType 
data %>%
  group_by(ContractType) %>%
  summarise(avg_MonthlyCharges = mean(MonthlyCharges, na.rm = TRUE))

#Categorize customers based on Age:
data <- data %>%
  mutate(AgeGroup = case_when(
    Age < 25 ~ "Youth",
    Age >= 25 & Age <= 55 ~ "Adult",
    Age > 55 ~ "Senior"
  ))

#Find the top 5 cities with the highest number of churned customers.
data %>%
  filter(Churn == "Yes") %>%
  count(City, sort = TRUE) %>%
  top_n(5)

# Extract only the names and cities of customers 
data %>%
  filter(TotalCharges > 3000, ContractType == "Month-to-Month", Churn == "Yes") %>%
  select(Name, City)

#Create a table that shows the average tenure and total revenue
data %>%
  group_by(ContractType) %>%
  summarise(
    avg_Tenure = mean(Tenure, na.rm = TRUE),
    total_Revenue = sum(TotalCharges, na.rm = TRUE)
  )




