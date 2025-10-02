# Workplace Wellness Case Analysis
# Author: Sabrina Lin

# Load the package
library(dplyr)
library(tidyverse)

# Load data
claims <- read.csv("../claims.csv")

#Q4
# Set variables for Q4
vars_q4 <- c(
  "covg_0715_0716",
  "nonzero_spend_0715_0716",
  "spendRx_0715_0716",
  "spendHosp_0715_0716",
  "spendOff_0715_0716",
  "spend_0715_0716")

# Create table for Q4
results_q4 <- data.frame(
  variable = character(),
  control_mean = numeric(),
  treatment_mean = numeric(),
  p_value = numeric())

# Run regression for each variable
for (v in vars_q4) {
  
  formula_q4 <- as.formula(paste(v, "~ treat"))
  model_q4 <- lm(formula_q4, data = claims)
  
  means_q4 <- claims %>%
    group_by(treat) %>%
    summarize(mean = mean(.data[[v]], na.rm = TRUE), .groups = "drop")
  
# Get p-value
p_val <- summary(model_q4)$coefficients[2, 4]
  
# Add results
results_q4 <- rbind(results_q4, data.frame(
    variable = v,
    control_mean = means_q4$mean[means_q4$treat == 0],
    treatment_mean = means_q4$mean[means_q4$treat == 1],
    p_value = p_val))}

# Print Q4 result
print(results_q4)

#Q5
# Set variables for Q5
vars_q5 <- c(
  "covg_0816_0717",
  "nonzero_spend_0816_0717",
  "spendRx_0816_0717",
  "spendHosp_0816_0717",
  "spendOff_0816_0717",
  "spend_0816_0717")

# Create table for Q5
results_q5 <- data.frame(
  variable = character(),
  no_controls = character(),    
  with_controls = character())

# Loop through each variable
for (v in vars_q5) {
  
# Model 1: No controls
formula_q5a <- as.formula(paste(v, "~ treat"))
model_q5a <- lm(formula_q5a, data = claims)
beta_q5a <- coef(summary(model_q5a))["treat", "Estimate"]
se_q5a <- coef(summary(model_q5a))["treat", "Std. Error"]
  
# Model 2: With demographic controls
formula_q5b <- as.formula(paste(v, "~ treat + male + white + age37_49 + age50"))
model_q5b <- lm(formula_q5b, data = claims)
beta_q5b <- coef(summary(model_q5b))["treat", "Estimate"]
se_q5b <- coef(summary(model_q5b))["treat", "Std. Error"]
  
# Add results to the table
results_q5 <- rbind(results_q5, data.frame(
    variable = v,
    no_controls = sprintf("%.2f (%.2f)", beta_q5a, se_q5a),
    with_controls = sprintf("%.2f (%.2f)", beta_q5b, se_q5b)))}

# Print Q5 results
print(results_q5)

#Q6
# Set variables for Q6
vars_q6 <- c(
  "covg_0816_0717",
  "nonzero_spend_0816_0717",
  "spendRx_0816_0717",
  "spendHosp_0816_0717",
  "spendOff_0816_0717",
  "spend_0816_0717")

# Create table for Q6
results_q6 <- data.frame(
  variable = character(),
  no_controls = character(),
  with_controls = character())

# Loop through each variable
for (v in vars_q6) {
  
  # Model 1: No demographic controls
  formula_q6a <- as.formula(paste(v, "~ completed_screening_nomiss_2016"))
  model_q6a <- lm(formula_q6a, data = claims)
  beta_q6a <- coef(summary(model_q6a))["completed_screening_nomiss_2016", "Estimate"]
  se_q6a <- coef(summary(model_q6a))["completed_screening_nomiss_2016", "Std. Error"]
  
  # Model 2: With demographic controls
  formula_q6b <- as.formula(paste(v, "~ completed_screening_nomiss_2016 + male + white + age37_49 + age50"))
  model_q6b <- lm(formula_q6b, data = claims)
  beta_q6b <- coef(summary(model_q6b))["completed_screening_nomiss_2016", "Estimate"]
  se_q6b <- coef(summary(model_q6b))["completed_screening_nomiss_2016", "Std. Error"]
  
  # Add results to table
  results_q6 <- rbind(results_q6, data.frame(
    variable = v,
    no_controls = sprintf("%.2f (%.2f)", beta_q6a, se_q6a),
    with_controls = sprintf("%.2f (%.2f)", beta_q6b, se_q6b)))}

# Print Q6 result
print(results_q6)