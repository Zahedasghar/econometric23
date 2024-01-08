# Load the required libraries
library(ISLR)
library(tidyverse)
library(ggeffects)
library(broom)
library(wooldridge)
# Load the mroz dataset
data("mroz")

# Explore the structure of the dataset
glimpse(mroz)
#disable scientific notation for model summary
options(scipen=999)
# Fit a logistic regression model
logit_model <- glm(inlf ~ nwifeinc + educ + exper + age, data = mroz, family = binomial)

# Display the summary of the logistic regression model
summary(logit_model)
tidy(logit_model) |> mutate_if(is.numeric, round, 3)
# Extract odds ratios
odds_ratios <- exp(coef(logit_model))

# Display odds ratios
cat("\nOdds Ratios:\n")
print(odds_ratios)


# Display tidy model results
cat("\nTidy Model Results:\n")
print(tidy_model)

# Create marginal effects
marginal_effects <- ggpredict(logit_model, c("nwifeinc", "educ", "exper", "age"))
marginal_effects
# Plot marginal effects
cat("\nMarginal Plots:\n")
plot(marginal_effects)

# Alternatively, you can customize the plots using ggplot2
ggplot(marginal_effects, aes(x = nwifeinc, y = predicted)) +
  geom_line() +
  labs(title = "Marginal Effect of nwifeinc on inlf",
       x = "nwifeinc",
       y = "Predicted Probability")



# Assuming you have a logistic regression model called logit_model
# Fit your logistic regression model
logit_model <- glm(inlf ~ nwifeinc + educ + exper + age, family = "binomial", data = mroz)

# Extract coefficient estimates
coef_estimates <- coef(logit_model)

# Calculate odds ratios
odds_ratios <- exp(coef_estimates)

# Print or view the odds ratios
print(odds_ratios)


