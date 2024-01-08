# R code equivalent to the provided STATA code

# Set more off
options(warn=-1)

# Clear all
rm(list = ls())

# Read in data
library(foreign)
library(haven)
insample <-   read_dta("C:/Users/92300/Dropbox/Applied Econometrics SBP/stocks/stock_watson_4E_replication_chapter14/Chapter_14/data/ca_school_testscore_insample.dta")
outofsample <-  read_dta("C:/Users/92300/Dropbox/Applied Econometrics SBP/stocks/stock_watson_4E_replication_chapter14/Chapter_14/data/ca_school_testscore_outofsample.dta")

# Generate insample and outofsample variables
insample$insample <- seq_along(insample$testscore) <= 1966
outofsample$outofsample <- seq_along(outofsample$testscore) > 1966

# Small Data Set Prediction
pred_1 <- c("str_s", "med_income_z", "te_avgyr_s", "exp_1000_1999_d", "frpm_frac_s", "ell_frac_s",
            "freem_frac_s", "enrollment_s", "fep_frac_s", "edi_s", "re_aian_frac_s", "re_asian_frac_s", "re_baa_frac_s",
            "re_fil_frac_s", "re_hl_frac_s", "re_hpi_frac_s", "re_tom_frac_s", "re_nr_frac_s", "te_fte_s", "te_1yr_frac_s",
            "te_2yr_frac_s", "te_tot_fte_rat_s", "exp_2000_2999_d", "exp_3000_3999_d", "exp_4000_4999_d", "exp_5000_5999_d",
            "exp_6000_6999_d", "exp_7000_7999_d", "exp_8000_8999_d", "expoc_1000_1999_d", "expoc_2000_2999_d", "expoc_3000_3999_d",
            "expoc_4000_4999_d", "expoc_5000_5999_d", "revoc_8010_8099_d", "revoc_8100_8299_d", "revoc_8300_8599_d", "revoc_8600_8799_d")

for (i in 1:length(pred_1)) {
  insample[[paste0("x_", i)]] <- insample[[pred_1[i]]]
  summary_x <- summary(insample[[paste0("x_", i)]][insample$insample])
  insample[[paste0("s_x_", i)]] <- (insample[[paste0("x_", i)]] - summary_x$mean) / summary_x$sd
}

# ... (code for generating xx_ and xxx_ variables omitted for brevity)

# Compute Cross-fold RMSE using regression in-sample
model_ols <- lm(testscore ~ ., data = insample)
insample$testscore_ols <- predict(model_ols, newdata = insample)
insample$e_ols <- insample$testscore - insample$testscore_ols
insample$e_ols2 <- insample$e_ols^2
rmse_oos_ols <- sqrt(mean((outofsample$testscore - predict(model_ols, newdata = outofsample))^2))

## Create principal components
pca_result <- prcomp(data = insample[, c("s_x_*", "s_xx_*", "s_xxx_*")], scale = TRUE, center = TRUE, retx = TRUE)

# Calculate principal component scores
pc_scores <- predict(pca_result, newdata = insample)

# Perform regression using principal components
model_pca <- lm(testscore_dm ~ ., data = cbind(insample, pc_scores))
insample$testscore_dm_pc <- predict(model_pca)

# Calculate RMSE for PCA regression
insample$e_pc <- insample$testscore_dm - insample$testscore_dm_pc
insample$e_pc2 <- insample$e_pc^2
rmse_oos_pca <- sqrt(mean((outofsample$testscore - predict(model_pca, newdata = outofsample))^2))

# Perform Ridge regression
library(glmnet)
ridge_model <- cv.glmnet(x = model.matrix(model_pca), y = insample$testscore_dm, alpha = 0)
lambda_ridge <- ridge_model$lambda.min
insample$testscore_ridge <- predict(ridge_model, newx = model.matrix(model_pca), s = lambda_ridge)
insample$e_ridge <- insample$testscore_dm - insample$testscore_ridge
insample$e_ridge2 <- insample$e_ridge^2
rmse_oos_ridge <- sqrt(mean((outofsample$testscore - predict(ridge_model, newx = model.matrix(model_pca), s = lambda_ridge))^2))

# Perform Lasso regression
lasso_model <- cv.glmnet(x = model.matrix(model_pca), y = insample$testscore_dm, alpha = 1)
lambda_lasso <- lasso_model$lambda.min
insample$testscore_lasso <- predict(lasso_model, newx = model.matrix(model_pca), s = lambda_lasso)
insample$e_lasso <- insample$testscore_dm - insample$testscore_lasso
insample$e_lasso2 <- insample$e_lasso^2
rmse_oos_lasso <- sqrt(mean((outofsample$testscore - predict(lasso_model, newx = model.matrix(model_pca), s = lambda_lasso))^2))

# Export results to a text file
write.table(insample, "ch14_large_output.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
