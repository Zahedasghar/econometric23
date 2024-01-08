#https://tilburgsciencehub.com/building-blocks/analyze-data/regressions/model-summary/

library(stargazer)
library(modelsummary)
library(tidyverse)
libr
# Fit a linear regression model
model <- lm(mpg ~ wt + hp + qsec, data = mtcars)

# Create LaTeX table
stargazer(model, title = "Linear Regression Results", align = TRUE)

stargazer(model, title = "Linear Regression Results", align = TRUE, out = "regression_table.tex")

library(modelsummary)

msummary(model,     
         vcov = "HC1",
         fmt = 3,
         estimate = "{estimate}",
         statistic = "[{std.error}]",
         coef_map = cm2, 
         gof_omit = 'AIC|BIC|RMSE|Within|FE',
         gof_map = gm2,
         output = "latex",
         escape = FALSE
)


# Load packages
library(modelsummary)
library(dplyr)
library(fixest)
library(stringr)


# Load data
data_url <- "https://github.com/tilburgsciencehub/website/blob/master/content/building-blocks/analyze-data/regressions/data_rent.Rda?raw=true"
load(url(data_url)) #data_rent is the cleaned data set


reg1 <- feols(logrent ~ 
                green_rating + size_new + oocc_new + class_a + class_b + 
                net + empl_new | 
                id, 
              data = data_rent
)

# Split "green rating" into two classifications: energystar and leed
reg2 <- feols(logrent ~ 
                energystar + leed + size_new + oocc_new + class_a + class_b + 
                net + empl_new | 
                id, 
              data = data_rent
)


reg3 <- feols(logrent ~ 
                green_rating + size_new + oocc_new + class_a + class_b + 
                net + empl_new + 
                age_0_10 + age_10_20 + age_20_30 + age_30_40 + renovated | 
                id, 
              data = data_rent
)

reg4 <- feols(logrent ~ 
                green_rating + size_new + oocc_new + class_a + class_b + 
                net + empl_new + 
                age_0_10 + age_10_20 + age_20_30 + age_30_40 + 
                renovated + story_medium + story_high + amenities  | 
                id, data = data_rent
)

# add fixed effects for green rating
reg5 <- feols(logrent ~ 
                size_new + oocc_new + class_a + class_b + 
                net + empl_new  + renovated + 
                age_0_10 + age_10_20 + age_20_30 + age_30_40 + 
                story_medium + story_high + amenities | 
                id + green_rating, 
              data = data_rent
)


models <- list(
  "(1)" = reg1, 
  "(2)" = reg2, 
  "(3)" = reg3, 
  "(4)" = reg4, 
  "(5)" = reg5)

msummary(models)



cm = c('green_rating'    = 'Green rating (1 = yes)',
       'energystar'    = 'Energystar (1 = yes)',
       'leed'    = 'LEED (1 = yes)',
       'size_new'    = 'Building size (millions of sq.ft)',
       'oocc_new' = 'Fraction occupied',
       'class_a' = 'Building class A (1 = yes)',
       'class_b' = 'Building class B (1 = yes)',
       'net' = 'Net contract (1 = yes)',
       'age_0_10' = 'Age: <10 years',
       'age_10_20' = 'Age: 10-20 years',
       'age_20_30' = 'Age: 20-30 years',
       'age_30_40' = 'Age: 30-40 years',
       'renovated' = 'Renovated (1 = yes)',
       'story_medium' = 'Stories: Intermediate (1 = yes)', 
       'story_high' = 'Stories: High (1 = yes)', 
       'amenities' = 'Amenities (1 = yes)')

msummary(models, vcov="HC1",
         coef_map = cm)



gm <- list(
  list("raw" = "nobs", "clean" = "Sample size", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R<sup>2</sup>", "fmt" = 2),
  list("raw" = "adj.r.squared", "clean" = "Adjusted R<sup>2</sup>", "fmt" = 2))
#get_gof(reg1) to see "raw" names of these statistics.

msummary(models, vcov="HC1",
         coef_map = cm, 
         gof_omit = 'AIC|BIC|RMSE|Within|Std.Errors|FE',
         gof_map = gm)


notetable1 <- c(
  "Notes: Each regression also includes 694 dummy variables, one for each locational cluster. 
  Regression (5) also includes an additional 694 dummy variables, one for each green building in the sample. 
  Standard errors are in parentheses")

titletable1 <- 'Table 1—Regression Results, Commercial Office Rents and Green Ratings 
(dependent variable: logarithm of effective rent in dollars per square foot)'

msummary(models, vcov="HC1",
         coef_map = cm, 
         gof_omit = 'AIC|BIC|RMSE|Within|Std.Errors|FE',
         gof_map = gm, 
         notes = notetable1, 
         title = titletable1)


msummary(models,
         vcov = "HC1",
         stars=TRUE,
         coef_map = cm, 
         gof_omit = 'AIC|BIC|RMSE|Within|Std.Errors|FE',
         gof_map = gm, 
         notes = notetable1, 
         title = titletable1)

note2table1 <- c(
  "Notes: Each regression also includes 694 dummy variables, one for each locational cluster. 
  Regression (5) also includes an additional 694 dummy variables, one for each green building in the sample. 
  Standard errors are in brackets.",
  "***Significant at the 1 percent level.", 
  "**Significant at the 5 percent level.",
  "*Significant at the 10 percent level.")


msummary(models,
         stars  = c('*' = .1, '**' = 0.05, '***' = .01),
         estimate = "{estimate}",
         statistic = "[{std.error}]{stars}",
         coef_map = cm, 
         gof_omit = 'AIC|BIC|RMSE|Within|Std.Errors|FE',
         gof_map = gm,
         notes = note2table1,
         title = titletable1)


msummary(models,
         vcov = "HC1",
         fmt = fmt_statistic(estimate = 3, std.error = 3),
         #just adding "fmt = 3" gives same result, since estimate and standard error are both set to 3 decimals.
         stars  = c('*' = .1, '**' = 0.05, '***' = .01),
         estimate = "{estimate}",
         statistic = "[{std.error}]{stars}",
         coef_map = cm, 
         gof_omit = 'AIC|BIC|RMSE|Within|Std.Errors|FE',
         gof_map = gm,
         notes = note2table1,
         title = titletable1)


#Change the note to be correct
note3table1 <-  c("Notes: Each regression also includes 694 dummy variables, one for 
each locational cluster. Regression (5) also includes an additional 694 dummy variables, 
one for each green building in the sample. Confidence intervals are in brackets.")

msummary(models,
         vcov = "HC1",
         fmt = fmt_statistic(estimate = 3, conf.int = 3),
         statistic ='conf.int',
         coef_map = cm, 
         gof_omit = 'AIC|BIC|RMSE|Within|Std.Errors|FE',
         gof_map = gm,
         notes = note3table1,
         title = titletable1)



msummary(models, output = "table1.md",
         vcov = "HC1",
         fmt = fmt_statistic(estimate = 3, std.error = 3),
         stars  = c('*' = .1, '**' = 0.05, '***' = .01),
         estimate = "{estimate}",
         statistic = "[{std.error}]{stars}",
         coef_map = cm, 
         gof_omit = 'AIC|BIC|RMSE|Within|FE',
         gof_map = gm,
         notes = note2table1,
         title = titletable1)




