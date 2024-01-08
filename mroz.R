# R code equivalent to the given Stata code
graphics.off()
rm(list=ls())

# Load the required package for data manipulation
library(haven)
library(tidyverse)
# Load the dataset
mroz <- read_dta("http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta")

# Inspect data
mroz |> dim()
# Keep only required variables
#mroz <- mroz[, c("inlf", "nwifeinc", "educ", "exper", "age", "kidslt6", "kidsge6")]

#select, filter, mutate, arrange, summarise

mroz_small <-  mroz %>% select(inlf, nwifeinc, educ, exper, age, kidslt6, kidsge6)


mroz %>% select(inlf, nwifeinc, educ, exper, age, kidslt6, kidsge6) %>%
  filter(inlf==1) %>%   ##
  mutate(exper2=exper^2) %>%
  arrange(desc(exper2)) %>%
  summarise(mean_exper2=mean(exper2))

names(mroz)

mroz |> names()
mroz |> colnames()
# Inspect data with glimpse
mroz |> glimpse()
mroz |> head(10)
mroz |> tail(10)
# Generate experience square
#mroz$exper2 <- mroz$exper^2


#select, filter, mutate
mroz <- mroz |> mutate(exper2=exper^2)

mroz |> dim()

# summarize inlf
mroz %>% group_by(inlf) %>% summarise(count=n()) #pipe operator which serves as a connect

# summarize kidslt6
mroz |> group_by(kidslt6) |> summarise(count=n())

# summarize kidsge6
mroz |> group_by(kidsge6) |> summarise(count=n())

# summarize educ
mroz |> group_by(educ) |> summarise(count=n())

# summarize age
mroz |> group_by(age) |> summarise(count=n())

# Summarize all variables using gt and gtsummary
library(gt)
library(kableExtra)
library(gtsummary)

mroz |>
  select(inlf, nwifeinc, educ, exper, exper2, age, kidslt6, kidsge6) |>
  filter(inlf==1) |>
  tbl_summary()

# %>% and |> are the same

mroz |>
  select(inlf, nwifeinc, educ, exper, exper2, age, kidslt6, kidsge6) |>
  tbl_summary(by = inlf) |>
  add_p()


library(gtExtras)
mroz |>
  select(inlf, nwifeinc, educ, exper, exper2, age, kidslt6, kidsge6) |>
  gt_plt_summary()

## Library skimr
library(skimr)
mroz |> skimr::skim()

??length

# Run Linear Probability Model

OLS <- lm(inlf ~ nwifeinc + educ + exper + exper2 + age + kidslt6 + kidsge6, data = mroz)

# Run linear probability model with robust standard errors

library(lmtest)
library(sandwich)
library(estimatr)
# Use Robust Standard Errors
model2 <- lm(inlf ~ nwifeinc + educ + exper + exper2 + age + kidslt6 + kidsge6, data = mroz)
# Compute robust standard errors
robust_se <- sqrt(diag(vcovHC(model2, type = "HC1")))

# Display the regression coefficients and robust standard errors
summary(model2)
coeftest(model2, vcov. = vcovHC)

OLS_robust <- lm_robust(inlf ~ nwifeinc + educ + exper + exper2 + age
                     + kidslt6 + kidsge6, data = mroz)


summary(model2)  # This will display the coefficients along with standard errors

# Run a logistic regression
logit <- glm(inlf ~ nwifeinc + educ + exper + exper2 + age +
                kidslt6 + kidsge6, family = binomial(link = "logit"), data = mroz)

# Run a probit model
probit <- glm(inlf ~ nwifeinc + educ + exper + exper2 + age +
                kidslt6 + kidsge6, family = binomial(link = "probit"), data = mroz)

## Use gtsummary to display the results

 tbl_regression(probit, exponentiate = TRUE)


 # Save results
 tbl <- tbl_regression(probit, exponentiate = TRUE)
 tbl %>%
   as_gt() %>%
   gt::gtsave(filename = "mroz_reg.png") # use extensions .png, .html, .docx, .rtf, .tex, .ltx

# Display standard errors along with coefficients
library(estimatr)

library(modelsummary)
model_list <- list(OLS, OLS_robust, logit, probit)


msummary(model_list, model.labels = c("OLS", "OLS_robust", "Logit", "probit"),
       keep.var.labels = TRUE, keep.constant = TRUE, stars = TRUE,
       style = "default", se = "std.error", p = 0.05, digits = 3)


reg_table <- msummary(model_list, model.labels = c("OLS", "OLS_robust", "Logit", "probit"),
                      keep.var.labels = TRUE, keep.constant = TRUE, stars = TRUE,
                      style = "default", se = "std.error", p = 0.05, digits = 3)
reg_table
library(gt)
library(kableExtra)
# Save the regression table
modelsummary(reg_table,output="reg_table.png")






library(modelsummary)
library(officer)
#use a named model list (the names will be used for temporary file names later on):
  the_models <- list(
    m1 = lm(Sepal.Length ~ Species, iris),
    m2 = lm(Sepal.Length ~ Sepal.Width + Species, iris)
  )
#create an empty doc representation:
  all_tables_doc <- read_docx()
#save one (temporary) docx file per model summary, and add these files to the main document's representation (here: all_tables_doc):
names(the_models) |>
  Map(f = \(name){
    file_name <- file.path(tempdir(), paste0(name,'.docx'))
    modelsummary(the_models[[name]], file_name)
    all_tables_doc <- all_tables_doc |>
      body_add_docx(file_name)
    name
  })
#save the main document:
  print(all_tables_doc, target = "all_tables.docx")

df <- mroz |> select(inlf,wage)

wage |> head(10)


# Install and load the 'ineq' package if not already installed
if (!requireNamespace("ineq", quietly = TRUE)) {
  install.packages("ineq")
}
library(ineq)

# Assuming your data frame is named 'df'
# where 'inlf' is the binary indicator and 'wage' is the wage variable
# Replace 'df' with your actual data frame name

# Calculate Gini coefficient
gini_coefficient <- ineq(df$wage, weight = df$inlf, type = "Gini")

# Print the Gini coefficient
print(paste("Gini Coefficient:", gini_coefficient$Gini))

# Calculate Gini coefficient
gini_coefficient <- ineq(df$wage, type = "Gini")

# Print the Gini coefficient
print(paste("Gini Coefficient:", gini_coefficient$Gini))

# Print the Gini coefficient
print(paste("Gini Coefficient:", gini_coefficient))





# Example usage:
wages <- c(1000, 2000, 1500, 2500, 1200, 800, 1500, 1200, 1800, 1000)
categories <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
df |> names()
# Split the data into two groups based on categories
wages_group1 <- mroz |> select(wage, kidslt6) |> filter(kidslt6<= 1)
wages_group2 <- mroz |> select(wage, kidslt6) |> filter(kidslt6>1)

# convert wage group to vector
wages_group1 <- wages_group1 |> pull(wage)
wages_group2 <- wages_group2 |> pull(wage)
# Get the number of observations in each group
n1 <- length(wages_group1)
n2 <- length(wages_group2)
n1
n
# Calculate the means of the two groups
mu_hat1 <- mean(wages_group1)
mu_hat2 <- mean(wages_group2)

# Create a vector of indices from 1 to n for each group
I1 <- 1:n1
I2 <- 1:n2

# Sort the wages in each group in ascending order
sorted_wage1 <- sort(wages_group1)
sorted_wage2 <- sort(wages_group2)

# Calculate the sum of sorted wages for each group
swi1 <- sum(sorted_wage1)
swi2 <- sum(sorted_wage2)

# Calculate the Gini coefficients for each group
gini_coeff1 <- (2 * swi1) / (mu_hat1 * n1 * (n1 - 1))
gini_coeff2 <- (2 * swi2) / (mu_hat2 * n2 * (n2 - 1))

# Calculate the z values for each group
z1 <- -(gini_coeff1 + 1) * sorted_wage1 + (2 * I1 - 1) / n1 * sorted_wage1
z2 <- -(gini_coeff2 + 1) * sorted_wage2 + (2 * I2 - 1) / n2 * sorted_wage2

# Calculate the mean of z values for each group
z_bar1 <- mean(z1)
z_bar2 <- mean(z2)

# Calculate the variances of the Gini coefficients for each group
var_gini1 <- (1 / (n1 - mu_hat1)^2) * sum((z1 - z_bar1)^2)
var_gini2 <- (1 / (n2 - mu_hat2)^2) * sum((z2 - z_bar2)^2)

# Display the results
result <- data.frame(
  gini_coeff1 = gini_coeff1,
  var_gini1 = var_gini1,
  gini_coeff2 = gini_coeff2,
  var_gini2 = var_gini2
)
print(result)



# Assuming you have your wage data in a vector called 'wages'
library(ineq)
# Assuming you have your wage data in a vector called 'wages'
library(ineq)

# Calculate the Gini coefficient
gini <- ineqGini(mroz$wage)

# Perform a two-tailed t-test
t_test <- t.test(mroz$wage, mu = mean(wages))

# View the test results
print(t_test)


# Assuming you have GINI indices for each time period in a data frame called 'gini_data'
data <- mroz |> select(wage) |> na.omit()
# Generate synthetic GINI index data
set.seed(123)  # Set seed for reproducibility
gini_data <- data.frame(
  time_period = c("T1", "T2", "T3"),
  gini_index = c(0.25, 0.30, 0.28)
)

# Print the synthetic GINI index data
print(gini_data)
# Permutation Test
permutation_test <- function(data, num_permutations) {
  observed_statistic <- mean(data$gini_index)  # Observed test statistic (mean GINI index)
  permuted_statistics <- numeric(num_permutations)
  for (i in 1:num_permutations) {
    permuted_data <- data[sample(nrow(data)), ]  # Permute the data
    permuted_statistic <- mean(permuted_data$gini_index)  # Test statistic for permuted data
    permuted_statistics[i] <- permuted_statistic
  }
  p_value <- sum(permuted_statistics >= observed_statistic) / num_permutations  # Calculate the p-value
  return(p_value)
}

# Bootstrap Test
bootstrap_test <- function(data, num_bootstraps) {
  observed_statistic <- mean(data$gini_index)  # Observed test statistic (mean GINI index)
  bootstrapped_statistics <- numeric(num_bootstraps)
  for (i in 1:num_bootstraps) {
    bootstrapped_data <- data[sample(nrow(data), replace = TRUE), ]  # Resample the data with replacement
    bootstrapped_statistic <- mean(bootstrapped_data$gini_index)  # Test statistic for bootstrapped data
    bootstrapped_statistics[i] <- bootstrapped_statistic
  }
  p_value <- sum(bootstrapped_statistics >= observed_statistic) / num_bootstraps  # Calculate the p-value
  return(p_value)
}

# Call the permutation test function
permutation_p_value <- permutation_test(gini_data, num_permutations = 1000)

# Call the bootstrap test function
bootstrap_p_value <- bootstrap_test(gini_data, num_bootstraps = 1000)

# Print the p-values
print(paste("Permutation Test p-value:", permutation_p_value))
print(paste("Bootstrap Test p-value:", bootstrap_p_value))
x1=5
x2=10
var_x1=2
var_x2=3

asym_test <- function(x1,x2,var_x1,var_x2) {
  #t-test
  s0=(x1-x2)/sqrt(var_x1+var_x2)
  p_value=2*pnorm(-abs(s0))
  return(p_value)
}


asym_test <- function(x1, x2, var_x1, var_x2) {
  # Standard error of the difference between means
  se = sqrt(var_x1 + var_x2)

  # Test statistic
  z = (x1 - x2) / se

  # Two-tailed p-value
  p_value = 2 * pnorm(-abs(z))

  return(p_value)
}

# Example usage
x1 = 5
x2 = 10
var_x1 = 2
var_x2 = 3

p_value = asym_test(x1, x2, var_x1, var_x2)
print(paste("p-value:", p_value))





# Generate synthetic wage data for 500 people across three time periods
set.seed(123)  # Set seed for reproducibility
wage_data <- data.frame(
  person_id = 1:500,
  time_period = rep(c("T1", "T2", "T3"), each = 500),
  wage = rnorm(1500, mean = 30000, sd = 5000)  # Generating random wages with normal distribution
)

# Print the first few rows of the synthetic wage data
head(wage_data)
# Permutation Test
permutation_test <- function(wage_data, num_permutations) {
  observed_statistic <- mean(wage_data$wage)  # Replace with your actual function to calculate the observed test statistic
  permuted_statistics <- numeric(num_permutations)
  for (i in 1:num_permutations) {
    permuted_data <- wage_data[sample(nrow(wage_data)), ]  # Permute the data
    permuted_statistic <- mean(permuted_data$wage)  # Test statistic for permuted data
    permuted_statistics[i] <- permuted_statistic
  }
  p_value <- sum(permuted_statistics >= observed_statistic) / num_permutations  # Calculate the p-value
  return(p_value)
}

# Bootstrap Test
bootstrap_test <- function(wage_data, num_bootstraps) {
  observed_statistic <- mean(wage_data$wage)  # Replace with your actual function to calculate the observed test statistic
  bootstrapped_statistics <- numeric(num_bootstraps)
  for (i in 1:num_bootstraps) {
    bootstrapped_data <- wage_data[sample(nrow(wage_data), replace = TRUE), ]  # Resample the data with replacement
    bootstrapped_statistic <- mean(bootstrapped_data$wage)  # Test statistic for bootstrapped data
    bootstrapped_statistics[i] <- bootstrapped_statistic
  }
  p_value <- sum(bootstrapped_statistics >= observed_statistic) / num_bootstraps  # Calculate the p-value
  return(p_value)
}

# Call the permutation test function
permutation_p_value <- permutation_test(wage_data, num_permutations = 1000)

# Call the bootstrap test function
bootstrap_p_value <- bootstrap_test(wage_data, num_bootstraps = 1000)

# Print the p-values
print(paste("Permutation Test p-value:", permutation_p_value))
print(paste("Bootstrap Test p-value:", bootstrap_p_value))
