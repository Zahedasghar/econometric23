
library(pacman)
library(parallel)
p_load(
  broom,
  latex2exp, ggplot2, ggthemes, viridis, extrafont,
  kableExtra,
  dplyr, magrittr, knitr, parallel
)

# Load packages
library(pacman)
library(magrittr)
p_load(tidyverse, Ecdat)
# Select and rename desired variables; assign to new dataset; format as tibble
test_df <- Caschool %>% select(
  test_score = testscr, ratio = str, income = avginc, enrollment = enrltot
) %>% as_tibble()
# View first 2 rows of the dataset
head(test_df, 2)

# Model 1: test ~ ratio + income
test_df %<>% mutate(e1 = lm(test_score ~ ratio + income, data = test_df) %>% residuals())

# Plot
ggplot(data = test_df, aes(x = income, y = e1)) +
  geom_point(size = 3, alpha = 0.5, color = red_pink) +
  theme_axes_serif

# Model 1: test ~ ratio + income
test_df %<>% mutate(e2 = lm(log(test_score) ~ ratio + income, data = test_df) %>% residuals())
# Plot
ggplot(data = test_df, aes(x = income)) +
  geom_point(aes(y = e2), size = 3, alpha = 0.5, color = red_pink) +
  labs(x = "Income", y = "e2") +
  theme_axes_serif

# Model 1: test ~ ratio + income
test_df %<>% mutate(e3 = lm(log(test_score) ~ ratio + log(income), data = test_df) %>% residuals())
# Plot
ggplot(data = test_df, aes(x = log(income))) +
  geom_point(aes(y = e3), size = 3, alpha = 0.5, color = red_pink) +
  labs(x = "Log(Income)", y = "e2") +
  theme_axes_serif

white_r2_spec <- lm(e3^2 ~
                      ratio * log(income) + I(ratio^2) + I(log(income)^2),
                    data = test_df
) %>% summary() %$% r.squared
white_stat_spec <- white_r2_spec %>% multiply_by(420)

# Load 'fixest' package
p_load(fixest)
# Regress log score on ratio and log income
test_reg <- feols(test_score ~ ratio + income, data = test_df)

## # Het-robust standard errors with 'vcov = 'hetero''
## summary(test_reg, vcov = 'hetero')

test_het_out <- summary(test_reg, vcov = 'hetero') %>% capture.output()
test_het_out[4:7] %>% paste0("\n") %>% cat()

## summary(test_reg, vcov = 'hetero')

test_het_out <- summary(test_reg, vcov = 'hetero') %>% capture.output()
test_het_out[4:7] %>% paste0("\n") %>% cat()

## summary(test_reg, vcov = 'iid')

test_hom_out <- summary(test_reg, vcov = 'iid') %>% capture.output()
test_hom_out[4:7] %>% paste0("\n") %>% cat()

# Create WLS transformed variables, multiplying by sqrt of 'pop'
test_df <- mutate(test_df,
                  test_score_wls = test_score * sqrt(enrollment),
                  ratio_wls      = ratio * sqrt(enrollment),
                  income_wls     = income * sqrt(enrollment),
                  intercept_wls  = 1 * sqrt(enrollment)
)

# WLS regression
wls_reg <- lm(
  test_score_wls ~ -1 + intercept_wls + ratio_wls + income_wls,
  data = test_df
)

# Grab the summary
test_wls_out <- summary(wls_reg) %>% capture.output()
# Print the coefficients
test_wls_out[11:14] %>% paste0("\n") %>% cat()

# Print the coefficients
test_het_out[4:7] %>% paste0("\n") %>% cat()

## lm(test_score ~ ratio + income, data = test_df, weights = enrollment)

b0 <- 1L
b1 <- 10L

set.seed(12345)
ggplot(data = tibble(
  x = runif(1e3, 0.5, 1.5),
  y = b0 + b1 * x + rnorm(1e3, 0, sd = x^2)
), aes(x = x, y = y)) +
  geom_point(color = "darkslategrey", size = 2.75, alpha = 0.5) +
  geom_abline(intercept = b0, slope = b1, color = "orange", size = 1.5, alpha = 0.85) +
  labs(x = "x", y = "y") +
  theme_axes_math

set.seed(12345)
ggplot(data = tibble(
  x = runif(1e3, 0.5, 1.5),
  u = rnorm(1e3, 0, sd = x^2)
), aes(x = x, y = u)) +
  geom_point(color = "darkslategrey", size = 2.75, alpha = 0.5) +
  labs(x = "x", y = "u") +
  theme_axes_math

## # Parameters
## b0 <- 1
## b1 <- 10
## s2 <- 1
## # Sample size
## n <- 30
## # Generate data
## sample_df <- tibble(
##   x = runif(n, 0.5, 1.5),
##   y = b0 + b1 * x + rnorm(n, 0, sd = s2 * x^2)
## )

## # OLS
## ols <- feols(y ~ x, data = sample_df)
## # WLS: Correct weights
## wls_t <- lm(y ~ x, data = sample_df, weights = 1/x^2)
## # WLS: Correct weights
## wls_f <- lm(y ~ x, data = sample_df, weights = 1/x)
## # Coefficients and standard errors
## summary(ols, vcov = 'iid')
## summary(ols, vcov = 'hetero')
## summary(wls_t)
## summary(wls_f)

# Parameters
b0 <- 1
b1 <- 10
s2 <- 1
# Sample size
n <- 30
# Number of iterations
n_iter <- 1e3
# Set seed
set.seed(1234)
# The simulation
# The simulation

library(purrr)

sim_df <- map_df(1:n_iter, ~{
  # Generate data
  sample_df <- tibble(
    x = runif(n, 0.5, 1.5),
    y = b0 + b1 * x + rnorm(n, 0, sd = s2 * x^2)
  )
  # OLS
  ols <- feols(y ~ x, data = sample_df)
  # WLS: Correct weights
  wls_t <- lm(y ~ x, data = sample_df, weights = 1/x^2)
  # WLS: Correct weights
  wls_f <- lm(y ~ x, data = sample_df, weights = 1/x)
  # Save results
  iter_df <- rbind(
    summary(ols, vcov = 'iid') %>% coeftable() %>% magrittr::extract(2, 1:2),
    summary(ols, vcov = 'hetero') %>% coeftable() %>% magrittr::extract(2, 1:2),
    summary(wls_t) %>% coef() %>% magrittr::extract(2,1:2),
    summary(wls_f) %>% coef() %>% magrittr::extract(2,1:2)
  ) %>%
    as_tibble() %>%
    mutate(
      model = c("OLS Hom.", "OLS Het.", "WLS T", "WLS F"),
      iter = .x
    )
  # Return the data
  return(iter_df)
})

#save(sim_df,file="data/sim_df.RData")
load("data/sim_df.RData")
# Change names
names(sim_df) <- c("coef", "se", "model", "iter")






ggplot(data = sim_df %>% filter(model != "OLS Hom."), aes(x = coef, color = model, fill = model)) +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_density(alpha = 0.1) +
  geom_hline(yintercept = 0) +
  labs(x = "Estimated coefficient", y = "Density") +
  scale_color_viridis_d("",
                        labels = c("OLS", "WLS Incorrect", "WLS Correct"),
                        end = 0.9, option = "C"
  ) +
  scale_fill_viridis_d("",
                       labels = c("OLS", "WLS Incorrect", "WLS Correct"),
                       end = 0.9, option = "C"
  ) +  theme(
    legend.position = c(.85,.9),
    # legend.background = element_blank(),
    legend.key.size = unit(1, "cm")
  )+theme_minimal()

ggplot(data = sim_df, aes(x = (coef-10)/se, color = model, fill = model)) +
  geom_vline(xintercept = qt(c(0.025, 0.975), df = 28), linetype = "dashed") +
  geom_density(alpha = 0.1) +
  geom_hline(yintercept = 0) +
  labs(x = "t statistic testing the true value", y = "Density") +
  scale_color_viridis_d("",
                        labels = c("OLS + Het.-robust", "Plain OLS", "WLS Incorrect", "WLS Correct"),
                        end = 0.9, option = "C"
  ) +
  scale_fill_viridis_d("",
                       labels = c("OLS + Het.-robust", "Plain OLS", "WLS Incorrect", "WLS Correct"),
                       end = 0.9, option = "C"
  ) +
  theme(
    legend.position = c(.85,.9),
    # legend.background = element_blank(),
    legend.key.size = unit(1, "cm")
  )

library(gt)

sim_df %>%
  filter(model != "OLS Hom.") %>%
  mutate(Estimator = recode(model,
                            "OLS Het." = "OLS",
                            "WLS F" = "WLS Incorrect",
                            "WLS T" = "WLS Correct"
  )) %>%
  group_by(Estimator) %>%
  summarize(Mean = mean(coef) %>% round(3), "S.D." = sd(coef) %>% round(3)) %>% gt()


sim_df %>%
  mutate(Estimators = recode(model,
                             "OLS Hom." = "OLS + Homosk.",
                             "OLS Het." = "OLS + Het.-robust",
                             "WLS F" = "WLS Incorrect",
                             "WLS T" = "WLS Correct"
  )) %>%
  group_by(Estimators) %>%
  summarize(`% Reject` = mean(abs(coef-10)/se > qt(0.975, 28)) %>% multiply_by(100) %>% round(1)) %>%
  gt()

# And an id
test_df %<>% mutate(id = 1:nrow(test_df))
# Stack the dataset a bunch of times
dup_df = lapply(X = 1:20, FUN = function(x) mutate(test_df, dup = x)) %>% bind_rows()
# Now run a regression
dup_est = lapply(
  X = 1:20,
  FUN = function(x) feols(test_score ~ ratio + income, data = dup_df %>% filter(dup <= x))
)
# Grab standard errors
dup_se = lapply(
  X = seq_along(dup_est),
  FUN = function(x) {
    data.frame(
      dups = x - 1,
      se_iid = dup_est[[x]] %>% summary(vcov = 'iid') %>% coeftable() %>% magrittr::extract(2,2),
      se_cl = dup_est[[x]] %>% summary(cluster = 'id') %>% coeftable() %>% magrittr::extract(2,2)
    )
  }
) %>% bind_rows()

ggplot(
  data = dup_se,
  aes(x = dups, y = se_iid)
) +
  geom_hline(yintercept = 0) +
  geom_line(color = red_pink, size = 0.5) +
  geom_point(color = red_pink, size = 3.5) +
  labs(x = "Number of times dataset is duplicated", y = "Standard error") +
  theme_minimal()

ggplot(
  data = dup_se,
  aes(x = dups)
) +
  geom_hline(yintercept = 0) +
  geom_line(aes(y = se_iid), color = red_pink, size = 0.5, alpha = 0.25) +
  geom_point(aes(y = se_iid), color = red_pink, size = 3.5, alpha = 0.25) +
  geom_line(aes(y = se_cl), color = orange, size = 0.5) +
  geom_point(aes(y = se_cl), color = orange, size = 3.5) +
  labs(x = "Number of times dataset is duplicated", y = "Standard error") +
  theme_minimal()

