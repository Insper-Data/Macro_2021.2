# LASSO -------------------------------------------------------------------

# AM + EM -----------------------------------------------------------------

setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")

rm(list=ls())

library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(stargazer)
library(plm)
library(lmtest)
library(glmnet)

# Database ----------------------------------------------------------------

data <- read.csv("Bases/merged2.csv")

panel_dataset <- pdata.frame(data, index = c("country", "year"))

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur  
  )

# Adding Lags + mean ------------------------------------------------------

# 1 lag

panel_dataset_lags <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_1 = dplyr::lag(GDP_growth,1),
    lag_fx_volatility_1 = dplyr::lag(fx_volatility,1),
    lag_nominal_rate_1 = dplyr::lag(nominal_rate,1),
    lag_taxes_1 = dplyr::lag(taxes,1),
    lag_account_balance_1 = dplyr::lag(account_balance,1),
    lag_lending_borroeing_rate_1 = dplyr::lag(lending_borroeing_rate,1),
    lag_unemployment_1 = dplyr::lag(unemployment,1),
    lag_inflation_mean_1 = dplyr::lag(inflation_mean,1),
    lag_debt_to_GDP_1 = dplyr::lag(debt_to_GDP,1),
    lag_real_interest_rate_1 = dplyr::lag(real_interest_rate,1),
    lag_Ep_1 = dplyr::lag(Ep,1),
    lag_Sp_1 = dplyr::lag(Sp,1),
    lag_Gp_1 = dplyr::lag(Gp,1),
    lag_foreign_debt_to_gdp_1 = dplyr::lag(foreign_debt_to_gdp, 1),
    lag_vix_EUA_1 = dplyr::lag(vix_EUA, 1))

# 2 lag 

panel_dataset_lags_2 <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_2 = dplyr::lag(GDP_growth,2),    
    GDP_growth_2 = 100*(GDP_cur - dplyr::lag(GDP_cur,2))/dplyr::lag(GDP_cur,2),
    lag_fx_volatility_2 = dplyr::lag(fx_volatility,2),
    lag_nominal_rate_2 = dplyr::lag(nominal_rate,2),
    lag_taxes_2 = dplyr::lag(taxes,2),
    lag_account_balance_2 = dplyr::lag(account_balance,2),
    lag_lending_borroeing_rate_2 = dplyr::lag(lending_borroeing_rate,2),
    lag_unemployment_2 = dplyr::lag(unemployment,2),
    lag_inflation_mean_2 = dplyr::lag(inflation_mean,2),
    lag_debt_to_GDP_2 = dplyr::lag(debt_to_GDP,2),
    lag_real_interest_rate_2 = dplyr::lag(real_interest_rate,2),
    lag_Ep_2 = dplyr::lag(Ep,2),
    lag_Sp_2 = dplyr::lag(Sp,2),
    lag_Gp_2 = dplyr::lag(Gp,2),
    lag_foreign_debt_to_gdp_2 = dplyr::lag(foreign_debt_to_gdp, 2),
    lag_vix_EUA_2 = dplyr::lag(vix_EUA, 2))

# 3 lag 

panel_dataset_lags_3 <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_3 = dplyr::lag(GDP_growth,3),    
    GDP_growth_3 = 100*(GDP_cur - dplyr::lag(GDP_cur,3))/dplyr::lag(GDP_cur,3),
    lag_fx_volatility_3 = dplyr::lag(fx_volatility,3),
    lag_nominal_rate_3 = dplyr::lag(nominal_rate,3),
    lag_taxes_3 = dplyr::lag(taxes,3),
    lag_account_balance_3 = dplyr::lag(account_balance,3),
    lag_lending_borroeing_rate_3 = dplyr::lag(lending_borroeing_rate,3),
    lag_unemployment_3 = dplyr::lag(unemployment,3),
    lag_inflation_mean_3 = dplyr::lag(inflation_mean,3),
    lag_debt_to_GDP_3 = dplyr::lag(debt_to_GDP,3),
    lag_real_interest_rate_3 = dplyr::lag(real_interest_rate,3),
    lag_Ep_3 = dplyr::lag(Ep,3),
    lag_Sp_3 = dplyr::lag(Sp,3),
    lag_Gp_3 = dplyr::lag(Gp,3),
    lag_foreign_debt_to_gdp_3 = dplyr::lag(foreign_debt_to_gdp, 3),
    lag_vix_EUA_3 = dplyr::lag(vix_EUA, 3))

# mean 

panel_dataset_mean_lags <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_mean = (dplyr::lag(GDP_growth,1) + dplyr::lag(GDP_growth,2) + dplyr::lag(GDP_growth,3))/3, 
    lag_fx_volatility_mean = (dplyr::lag(fx_volatility,1) +dplyr::lag(fx_volatility,2) +dplyr::lag(fx_volatility,3))/3,
    lag_nominal_rate_mean = (dplyr::lag(nominal_rate,1)+dplyr::lag(nominal_rate,2)+dplyr::lag(nominal_rate,3))/3,
    lag_taxes_mean = (dplyr::lag(taxes,1)+dplyr::lag(taxes,2)+dplyr::lag(taxes,3))/3,
    lag_account_balance_mean = (dplyr::lag(account_balance,1)+dplyr::lag(account_balance,2)+dplyr::lag(account_balance,3))/3,
    lag_lending_borroeing_rate_mean = (dplyr::lag(lending_borroeing_rate,1)+dplyr::lag(lending_borroeing_rate,2)+dplyr::lag(lending_borroeing_rate,3))/3,
    lag_unemployment_mean = (dplyr::lag(unemployment,1)+dplyr::lag(unemployment,2)+dplyr::lag(unemployment,3))/3,
    lag_inflation_mean_mean = (dplyr::lag(inflation_mean,1)+dplyr::lag(inflation_mean,2)+dplyr::lag(inflation_mean,3))/3,
    lag_debt_to_GDP_mean = (dplyr::lag(debt_to_GDP,1)+dplyr::lag(debt_to_GDP,2)+dplyr::lag(debt_to_GDP,3))/3,
    lag_real_interest_rate_mean = (dplyr::lag(real_interest_rate,1)+dplyr::lag(real_interest_rate,2)+dplyr::lag(real_interest_rate,3))/3,
    lag_Ep_mean = (dplyr::lag(Ep,1)+dplyr::lag(Ep,2)+dplyr::lag(Ep,3))/3,
    lag_Sp_mean = (dplyr::lag(Sp,1)+dplyr::lag(Sp,2)+dplyr::lag(Sp,3))/3,
    lag_Gp_mean = (dplyr::lag(Gp,1)+dplyr::lag(Gp,2)+dplyr::lag(Gp,3))/3,
    lag_foreign_debt_to_gdp_mean = (dplyr::lag(foreign_debt_to_gdp, 1) +dplyr::lag(foreign_debt_to_gdp, 2) + dplyr::lag(foreign_debt_to_gdp, 3))/3,
    lag_vix_EUA_mean = (dplyr::lag(vix_EUA, 1) + dplyr::lag(vix_EUA, 2) + dplyr::lag(vix_EUA, 3))/ 3)

# LASSO regression --------------------------------------------------------

## Excluding real_interest_rate and taxes due to NA values

lambda.array <- seq(from = 0, to = 1, by = 0.001)

# Lag 0 (no lags) ---------------------------------------------------------

# Identifying the variables

lag_0 <- subset(panel_dataset, select = c(GDP_growth, fx_volatility, nominal_rate, account_balance, lending_borroeing_rate, unemployment, inflation_mean, debt_to_GDP, Ep, Sp, Gp, foreign_debt_to_gdp, vix_EUA, spreads))

lag_0 <- na.omit(lag_0)

x <- subset(lag_0, select = -c(spreads))
x <- scale(x)
y <- lag_0[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1, lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Coefficients

model_plot <- glmnet(x, y, alpha = 1)
plot(model_plot, xvar = "lambda", label = T)

### Best lambda mean squared error (MSE)

MSE_lasso_0 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_0_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table0 <- as.data.frame(as.matrix(coef(best_model)))
table0_se = as.data.frame(as.matrix(coef(se_model)))
table0 <- cbind(table0, "new column" = table0_se$s0)
colnames(table0) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table0[nrow(table0) + 1,] = c(MSE_lasso_0, MSE_lasso_0_se)
rownames(table0)[15] <- c("MSE")
View(table0)

# Lag 1 -------------------------------------------------------------------

# Identifying the variables

lag_1 <- subset(panel_dataset_lags, select = c(lag_GDP_growth_1, lag_fx_volatility_1, lag_nominal_rate_1, lag_account_balance_1, lag_lending_borroeing_rate_1, lag_unemployment_1, lag_inflation_mean_1, lag_debt_to_GDP_1, lag_Ep_1, lag_Sp_1, lag_Gp_1, lag_foreign_debt_to_gdp_1, lag_vix_EUA_1, spreads))

lag_1 <- na.omit(lag_1)

x <- subset(lag_1, select = -c(spreads))
x <- scale(x)
y <- lag_1[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1, lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Coefficients

model_plot <- glmnet(x, y, alpha = 1)
plot(model_plot, xvar = "lambda", label = T)

### Best lambda mean squared error (MSE)

MSE_lasso_1 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_1_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table1 <- as.data.frame(as.matrix(coef(best_model)))
table1_se = as.data.frame(as.matrix(coef(se_model)))
table1 <- cbind(table1, "new column" = table1_se$s0)
colnames(table1) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table1[nrow(table1) + 1,] = c(MSE_lasso_1, MSE_lasso_1_se)
rownames(table1)[15] <- c("MSE")
View(table1)

# Lag 2 -------------------------------------------------------------------

# Identifying the variables

lag_2 <- subset(panel_dataset_lags_2, select = c(lag_GDP_growth_2, lag_fx_volatility_2, lag_nominal_rate_2, lag_account_balance_2, lag_lending_borroeing_rate_2, lag_unemployment_2, lag_inflation_mean_2, lag_debt_to_GDP_2, lag_Ep_2, lag_Sp_2, lag_Gp_2, lag_foreign_debt_to_gdp_2, lag_vix_EUA_2, spreads))

lag_2 <- na.omit(lag_2)

x <- subset(lag_2, select = -c(spreads))
x <- scale(x)
y <- lag_2[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Coefficients

model_plot <- glmnet(x, y, alpha = 1)
plot(model_plot, xvar = "lambda", label = T)

### Best lambda mean squared error (MSE)

MSE_lasso_2 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_2_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table2 <- as.data.frame(as.matrix(coef(best_model)))
table2_se = as.data.frame(as.matrix(coef(se_model)))
table2 <- cbind(table2, "new column" = table2_se$s0)
colnames(table2) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table2[nrow(table2) + 1,] = c(MSE_lasso_2, MSE_lasso_2_se)
rownames(table2)[15] <- c("MSE")
View(table2)

# Lag 3 -------------------------------------------------------------------

# Identifying the variables

lag_3 <- subset(panel_dataset_lags_3, select = c(lag_GDP_growth_3, lag_fx_volatility_3, lag_nominal_rate_3, lag_account_balance_3, lag_lending_borroeing_rate_3, lag_unemployment_3, lag_inflation_mean_3, lag_debt_to_GDP_3, lag_Ep_3, lag_Sp_3, lag_Gp_3, lag_foreign_debt_to_gdp_3, lag_vix_EUA_3, spreads))

lag_3 <- na.omit(lag_3)

x <- subset(lag_3, select = -c(spreads))
x <- scale(x)
y <- lag_3[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Coefficients

model_plot <- glmnet(x, y, alpha = 1)
plot(model_plot, xvar = "lambda", label = T)

### Best lambda mean squared error (MSE)

MSE_lasso_3 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_3_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table3 <- as.data.frame(as.matrix(coef(best_model)))
table3_se = as.data.frame(as.matrix(coef(se_model)))
table3 <- cbind(table3, "new column" = table0_se$s0)
colnames(table3) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table3[nrow(table3) + 1,] = c(MSE_lasso_3, MSE_lasso_3_se)
rownames(table3)[15] <- c("MSE")
View(table3)

# Mean --------------------------------------------------------------------

# Identifying the variables

mean <- subset(panel_dataset_mean_lags, select = c(lag_GDP_growth_mean, lag_fx_volatility_mean, lag_nominal_rate_mean, lag_account_balance_mean, lag_lending_borroeing_rate_mean, lag_unemployment_mean, lag_inflation_mean_mean, lag_debt_to_GDP_mean, lag_Ep_mean, lag_Sp_mean, lag_Gp_mean, lag_foreign_debt_to_gdp_mean, lag_vix_EUA_mean, spreads))

mean <- na.omit(mean)

x <- subset(mean, select = -c(spreads))
x <- scale(x)
y <- mean[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

### Coefficients

model_plot <- glmnet(x, y, alpha = 1)
plot(model_plot, xvar = "lambda", label = T)

### Best lambda mean squared error (MSE)

MSE_lasso_mean <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_mean_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table_mean <- as.data.frame(as.matrix(coef(best_model)))
table_mean_se = as.data.frame(as.matrix(coef(se_model)))
table_mean <- cbind(table_mean, "new column" = table_mean_se$s0)
colnames(table_mean) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table_mean[nrow(table_mean) + 1,] = c(MSE_lasso_mean, MSE_lasso_mean_se)
rownames(table_mean)[15] <- c("MSE")
View(table_mean)

# MSE and RMSE of each lag ------------------------------------------------

MSE_RMSE_AMEM <- data.frame(row.names = c("Lag_0", "Lag_1", "Lag_2", "Lag_3", "mean"),
                            MSE_best = c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean),
                            MSE_1se = c(MSE_lasso_0_se, MSE_lasso_1_se, MSE_lasso_2_se, MSE_lasso_3_se, MSE_lasso_mean_se),
                            RMSE_best = sqrt(c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean)),
                            RMSE_1se = sqrt(c(MSE_lasso_0_se, MSE_lasso_1_se, MSE_lasso_2_se, MSE_lasso_3_se, MSE_lasso_mean_se)))

### Interpretability x predictive power

View(MSE_RMSE_AMEM)


# AM ----------------------------------------------------------------------

## Only advenced

rm(list=ls())

# Database ----------------------------------------------------------------

data <- read.csv("Bases/merged2.csv")

panel_dataset <- pdata.frame(data, index = c("country", "year"))

# Panel dataset for AM 
panel_dataset <- panel_dataset %>% 
  filter(develop == "AM")

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur  
  )


# Adding Lags + mean ------------------------------------------------------

# 1 lag

panel_dataset_lags <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_1 = dplyr::lag(GDP_growth,1),
    lag_fx_volatility_1 = dplyr::lag(fx_volatility,1),
    lag_nominal_rate_1 = dplyr::lag(nominal_rate,1),
    lag_taxes_1 = dplyr::lag(taxes,1),
    lag_account_balance_1 = dplyr::lag(account_balance,1),
    lag_lending_borroeing_rate_1 = dplyr::lag(lending_borroeing_rate,1),
    lag_unemployment_1 = dplyr::lag(unemployment,1),
    lag_inflation_mean_1 = dplyr::lag(inflation_mean,1),
    lag_debt_to_GDP_1 = dplyr::lag(debt_to_GDP,1),
    lag_real_interest_rate_1 = dplyr::lag(real_interest_rate,1),
    lag_Ep_1 = dplyr::lag(Ep,1),
    lag_Sp_1 = dplyr::lag(Sp,1),
    lag_Gp_1 = dplyr::lag(Gp,1),
    lag_foreign_debt_to_gdp_1 = dplyr::lag(foreign_debt_to_gdp, 1),
    lag_vix_EUA_1 = dplyr::lag(vix_EUA, 1))

# 2 lag 

panel_dataset_lags_2 <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_2 = dplyr::lag(GDP_growth,2),    
    GDP_growth_2 = 100*(GDP_cur - dplyr::lag(GDP_cur,2))/dplyr::lag(GDP_cur,2),
    lag_fx_volatility_2 = dplyr::lag(fx_volatility,2),
    lag_nominal_rate_2 = dplyr::lag(nominal_rate,2),
    lag_taxes_2 = dplyr::lag(taxes,2),
    lag_account_balance_2 = dplyr::lag(account_balance,2),
    lag_lending_borroeing_rate_2 = dplyr::lag(lending_borroeing_rate,2),
    lag_unemployment_2 = dplyr::lag(unemployment,2),
    lag_inflation_mean_2 = dplyr::lag(inflation_mean,2),
    lag_debt_to_GDP_2 = dplyr::lag(debt_to_GDP,2),
    lag_real_interest_rate_2 = dplyr::lag(real_interest_rate,2),
    lag_Ep_2 = dplyr::lag(Ep,2),
    lag_Sp_2 = dplyr::lag(Sp,2),
    lag_Gp_2 = dplyr::lag(Gp,2),
    lag_foreign_debt_to_gdp_2 = dplyr::lag(foreign_debt_to_gdp, 2),
    lag_vix_EUA_2 = dplyr::lag(vix_EUA, 2))

# 3 lag

panel_dataset_lags_3 <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_3 = dplyr::lag(GDP_growth,3),    
    GDP_growth_3 = 100*(GDP_cur - dplyr::lag(GDP_cur,3))/dplyr::lag(GDP_cur,3),
    lag_fx_volatility_3 = dplyr::lag(fx_volatility,3),
    lag_nominal_rate_3 = dplyr::lag(nominal_rate,3),
    lag_taxes_3 = dplyr::lag(taxes,3),
    lag_account_balance_3 = dplyr::lag(account_balance,3),
    lag_lending_borroeing_rate_3 = dplyr::lag(lending_borroeing_rate,3),
    lag_unemployment_3 = dplyr::lag(unemployment,3),
    lag_inflation_mean_3 = dplyr::lag(inflation_mean,3),
    lag_debt_to_GDP_3 = dplyr::lag(debt_to_GDP,3),
    lag_real_interest_rate_3 = dplyr::lag(real_interest_rate,3),
    lag_Ep_3 = dplyr::lag(Ep,3),
    lag_Sp_3 = dplyr::lag(Sp,3),
    lag_Gp_3 = dplyr::lag(Gp,3),
    lag_foreign_debt_to_gdp_3 = dplyr::lag(foreign_debt_to_gdp, 3),
    lag_vix_EUA_3 = dplyr::lag(vix_EUA, 3))

# Mean

panel_dataset_mean_lags <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_mean = (dplyr::lag(GDP_growth,1) + dplyr::lag(GDP_growth,2) + dplyr::lag(GDP_growth,3))/3, 
    lag_fx_volatility_mean = (dplyr::lag(fx_volatility,1) +dplyr::lag(fx_volatility,2) +dplyr::lag(fx_volatility,3))/3,
    lag_nominal_rate_mean = (dplyr::lag(nominal_rate,1)+dplyr::lag(nominal_rate,2)+dplyr::lag(nominal_rate,3))/3,
    lag_taxes_mean = (dplyr::lag(taxes,1)+dplyr::lag(taxes,2)+dplyr::lag(taxes,3))/3,
    lag_account_balance_mean = (dplyr::lag(account_balance,1)+dplyr::lag(account_balance,2)+dplyr::lag(account_balance,3))/3,
    lag_lending_borroeing_rate_mean = (dplyr::lag(lending_borroeing_rate,1)+dplyr::lag(lending_borroeing_rate,2)+dplyr::lag(lending_borroeing_rate,3))/3,
    lag_unemployment_mean = (dplyr::lag(unemployment,1)+dplyr::lag(unemployment,2)+dplyr::lag(unemployment,3))/3,
    lag_inflation_mean_mean = (dplyr::lag(inflation_mean,1)+dplyr::lag(inflation_mean,2)+dplyr::lag(inflation_mean,3))/3,
    lag_debt_to_GDP_mean = (dplyr::lag(debt_to_GDP,1)+dplyr::lag(debt_to_GDP,2)+dplyr::lag(debt_to_GDP,3))/3,
    lag_real_interest_rate_mean = (dplyr::lag(real_interest_rate,1)+dplyr::lag(real_interest_rate,2)+dplyr::lag(real_interest_rate,3))/3,
    lag_Ep_mean = (dplyr::lag(Ep,1)+dplyr::lag(Ep,2)+dplyr::lag(Ep,3))/3,
    lag_Sp_mean = (dplyr::lag(Sp,1)+dplyr::lag(Sp,2)+dplyr::lag(Sp,3))/3,
    lag_Gp_mean = (dplyr::lag(Gp,1)+dplyr::lag(Gp,2)+dplyr::lag(Gp,3))/3,
    lag_foreign_debt_to_gdp_mean = (dplyr::lag(foreign_debt_to_gdp, 1) +dplyr::lag(foreign_debt_to_gdp, 2) + dplyr::lag(foreign_debt_to_gdp, 3))/3,
    lag_vix_EUA_mean = (dplyr::lag(vix_EUA, 1) + dplyr::lag(vix_EUA, 2) + dplyr::lag(vix_EUA, 3))/ 3)

# LASSO regression --------------------------------------------------------

## Excluding real_interest_rate and taxes due to NA values

lambda.array <- seq(from = 0, to = 1, by = 0.001)

# Lag 0 (no lags) ---------------------------------------------------------

# Identifying the variables

lag_0 <- subset(panel_dataset, select = c(GDP_growth, fx_volatility, nominal_rate, account_balance, lending_borroeing_rate, unemployment, inflation_mean, debt_to_GDP, Ep, Sp, Gp, foreign_debt_to_gdp, vix_EUA, spreads))

lag_0 <- na.omit(lag_0)

x <- subset(lag_0, select = -c(spreads))
x <- scale(x)
y <- lag_0[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_0 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_0_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table0 <- as.data.frame(as.matrix(coef(best_model)))
table0_se = as.data.frame(as.matrix(coef(se_model)))
table0 <- cbind(table0, "new column" = table0_se$s0)
colnames(table0) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table0[nrow(table0) + 1,] = c(MSE_lasso_0, MSE_lasso_0_se)
rownames(table0)[15] <- c("MSE")
View(table0)

# Lag 1 -------------------------------------------------------------------

# Identifying the variables

lag_1 <- subset(panel_dataset_lags, select = c(lag_GDP_growth_1, lag_fx_volatility_1, lag_nominal_rate_1, lag_account_balance_1, lag_lending_borroeing_rate_1, lag_unemployment_1, lag_inflation_mean_1, lag_debt_to_GDP_1, lag_Ep_1, lag_Sp_1, lag_Gp_1, lag_foreign_debt_to_gdp_1, lag_vix_EUA_1, spreads))

lag_1 <- na.omit(lag_1)

x <- subset(lag_1, select = -c(spreads))
x <- scale(x)
y <- lag_1[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_1 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_1_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table1 <- as.data.frame(as.matrix(coef(best_model)))
table1_se = as.data.frame(as.matrix(coef(se_model)))
table1 <- cbind(table1, "new column" = table1_se$s0)
colnames(table1) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table1[nrow(table1) + 1,] = c(MSE_lasso_1, MSE_lasso_1_se)
rownames(table1)[15] <- c("MSE")
View(table1)

# Lag 2 -------------------------------------------------------------------

# Identifying the variables

lag_2 <- subset(panel_dataset_lags_2, select = c(lag_GDP_growth_2, lag_fx_volatility_2, lag_nominal_rate_2, lag_account_balance_2, lag_lending_borroeing_rate_2, lag_unemployment_2, lag_inflation_mean_2, lag_debt_to_GDP_2, lag_Ep_2, lag_Sp_2, lag_Gp_2, lag_foreign_debt_to_gdp_2, lag_vix_EUA_2, spreads))

lag_2 <- na.omit(lag_2)

x <- subset(lag_2, select = -c(spreads))
x <- scale(x)
y <- lag_2[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_2 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_2_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table2 <- as.data.frame(as.matrix(coef(best_model)))
table2_se = as.data.frame(as.matrix(coef(se_model)))
table2 <- cbind(table2, "new column" = table2_se$s0)
colnames(table2) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table2[nrow(table2) + 1,] = c(MSE_lasso_2, MSE_lasso_2_se)
rownames(table2)[15] <- c("MSE")
View(table2)

# Lag 3 -------------------------------------------------------------------

# Identifying the variables

lag_3 <- subset(panel_dataset_lags_3, select = c(lag_GDP_growth_3, lag_fx_volatility_3, lag_nominal_rate_3, lag_account_balance_3, lag_lending_borroeing_rate_3, lag_unemployment_3, lag_inflation_mean_3, lag_debt_to_GDP_3, lag_Ep_3, lag_Sp_3, lag_Gp_3, lag_foreign_debt_to_gdp_3, lag_vix_EUA_3, spreads))

lag_3 <- na.omit(lag_3)

x <- subset(lag_3, select = -c(spreads))
x <- scale(x)
y <- lag_3[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_3 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_3_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table3 <- as.data.frame(as.matrix(coef(best_model)))
table3_se = as.data.frame(as.matrix(coef(se_model)))
table3 <- cbind(table3, "new column" = table0_se$s0)
colnames(table3) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table3[nrow(table3) + 1,] = c(MSE_lasso_3, MSE_lasso_3_se)
rownames(table3)[15] <- c("MSE")
View(table3)

# Mean --------------------------------------------------------------------

# Identifying the variables

mean <- subset(panel_dataset_mean_lags, select = c(lag_GDP_growth_mean, lag_fx_volatility_mean, lag_nominal_rate_mean, lag_account_balance_mean, lag_lending_borroeing_rate_mean, lag_unemployment_mean, lag_inflation_mean_mean, lag_debt_to_GDP_mean, lag_Ep_mean, lag_Sp_mean, lag_Gp_mean, lag_foreign_debt_to_gdp_mean, lag_vix_EUA_mean, spreads))

mean <- na.omit(mean)

x <- subset(mean, select = -c(spreads))
x <- scale(x)
y <- mean[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_mean <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_mean_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table_mean <- as.data.frame(as.matrix(coef(best_model)))
table_mean_se = as.data.frame(as.matrix(coef(se_model)))
table_mean <- cbind(table_mean, "new column" = table_mean_se$s0)
colnames(table_mean) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table_mean[nrow(table_mean) + 1,] = c(MSE_lasso_mean, MSE_lasso_mean_se)
rownames(table_mean)[15] <- c("MSE")
View(table_mean)


# MSE and RMSE of each lag ------------------------------------------------

MSE_RMSE_AM <- data.frame(row.names = c("Lag_0", "Lag_1", "Lag_2", "Lag_3", "mean"),
                          MSE_best = c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean),
                          MSE_1se = c(MSE_lasso_0_se, MSE_lasso_1_se, MSE_lasso_2_se, MSE_lasso_3_se, MSE_lasso_mean_se),
                          RMSE_best = sqrt(c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean)),
                          RMSE_se = sqrt(c(MSE_lasso_0_se, MSE_lasso_1_se, MSE_lasso_2_se, MSE_lasso_3_se, MSE_lasso_mean_se)))

### Interpretability x predictive power

View(MSE_RMSE_AM)

# EM ----------------------------------------------------------------------

## Only emerging

rm(list=ls())

# Database ----------------------------------------------------------------

data <- read.csv("Bases/merged2.csv")

panel_dataset <- pdata.frame(data, index = c("country", "year"))

# Panel dataset for EM 
panel_dataset <- panel_dataset %>% 
  filter(develop == "EM")

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur  
  )


# Adding Lags + mean ------------------------------------------------------

# 1 lag

panel_dataset_lags <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_1 = dplyr::lag(GDP_growth,1),
    lag_fx_volatility_1 = dplyr::lag(fx_volatility,1),
    lag_nominal_rate_1 = dplyr::lag(nominal_rate,1),
    lag_taxes_1 = dplyr::lag(taxes,1),
    lag_account_balance_1 = dplyr::lag(account_balance,1),
    lag_lending_borroeing_rate_1 = dplyr::lag(lending_borroeing_rate,1),
    lag_unemployment_1 = dplyr::lag(unemployment,1),
    lag_inflation_mean_1 = dplyr::lag(inflation_mean,1),
    lag_debt_to_GDP_1 = dplyr::lag(debt_to_GDP,1),
    lag_real_interest_rate_1 = dplyr::lag(real_interest_rate,1),
    lag_Ep_1 = dplyr::lag(Ep,1),
    lag_Sp_1 = dplyr::lag(Sp,1),
    lag_Gp_1 = dplyr::lag(Gp,1),
    lag_foreign_debt_to_gdp_1 = dplyr::lag(foreign_debt_to_gdp, 1),
    lag_vix_EUA_1 = dplyr::lag(vix_EUA, 1))

# 2 lag 

panel_dataset_lags_2 <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_2 = dplyr::lag(GDP_growth,2),    
    GDP_growth_2 = 100*(GDP_cur - dplyr::lag(GDP_cur,2))/dplyr::lag(GDP_cur,2),
    lag_fx_volatility_2 = dplyr::lag(fx_volatility,2),
    lag_nominal_rate_2 = dplyr::lag(nominal_rate,2),
    lag_taxes_2 = dplyr::lag(taxes,2),
    lag_account_balance_2 = dplyr::lag(account_balance,2),
    lag_lending_borroeing_rate_2 = dplyr::lag(lending_borroeing_rate,2),
    lag_unemployment_2 = dplyr::lag(unemployment,2),
    lag_inflation_mean_2 = dplyr::lag(inflation_mean,2),
    lag_debt_to_GDP_2 = dplyr::lag(debt_to_GDP,2),
    lag_real_interest_rate_2 = dplyr::lag(real_interest_rate,2),
    lag_Ep_2 = dplyr::lag(Ep,2),
    lag_Sp_2 = dplyr::lag(Sp,2),
    lag_Gp_2 = dplyr::lag(Gp,2),
    lag_foreign_debt_to_gdp_2 = dplyr::lag(foreign_debt_to_gdp, 2),
    lag_vix_EUA_2 = dplyr::lag(vix_EUA, 2))

# 3 lag

panel_dataset_lags_3 <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_3 = dplyr::lag(GDP_growth,3),    
    GDP_growth_3 = 100*(GDP_cur - dplyr::lag(GDP_cur,3))/dplyr::lag(GDP_cur,3),
    lag_fx_volatility_3 = dplyr::lag(fx_volatility,3),
    lag_nominal_rate_3 = dplyr::lag(nominal_rate,3),
    lag_taxes_3 = dplyr::lag(taxes,3),
    lag_account_balance_3 = dplyr::lag(account_balance,3),
    lag_lending_borroeing_rate_3 = dplyr::lag(lending_borroeing_rate,3),
    lag_unemployment_3 = dplyr::lag(unemployment,3),
    lag_inflation_mean_3 = dplyr::lag(inflation_mean,3),
    lag_debt_to_GDP_3 = dplyr::lag(debt_to_GDP,3),
    lag_real_interest_rate_3 = dplyr::lag(real_interest_rate,3),
    lag_Ep_3 = dplyr::lag(Ep,3),
    lag_Sp_3 = dplyr::lag(Sp,3),
    lag_Gp_3 = dplyr::lag(Gp,3),
    lag_foreign_debt_to_gdp_3 = dplyr::lag(foreign_debt_to_gdp, 3),
    lag_vix_EUA_3 = dplyr::lag(vix_EUA, 3))

# Mean

panel_dataset_mean_lags <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_mean = (dplyr::lag(GDP_growth,1) + dplyr::lag(GDP_growth,2) + dplyr::lag(GDP_growth,3))/3, 
    lag_fx_volatility_mean = (dplyr::lag(fx_volatility,1) +dplyr::lag(fx_volatility,2) +dplyr::lag(fx_volatility,3))/3,
    lag_nominal_rate_mean = (dplyr::lag(nominal_rate,1)+dplyr::lag(nominal_rate,2)+dplyr::lag(nominal_rate,3))/3,
    lag_taxes_mean = (dplyr::lag(taxes,1)+dplyr::lag(taxes,2)+dplyr::lag(taxes,3))/3,
    lag_account_balance_mean = (dplyr::lag(account_balance,1)+dplyr::lag(account_balance,2)+dplyr::lag(account_balance,3))/3,
    lag_lending_borroeing_rate_mean = (dplyr::lag(lending_borroeing_rate,1)+dplyr::lag(lending_borroeing_rate,2)+dplyr::lag(lending_borroeing_rate,3))/3,
    lag_unemployment_mean = (dplyr::lag(unemployment,1)+dplyr::lag(unemployment,2)+dplyr::lag(unemployment,3))/3,
    lag_inflation_mean_mean = (dplyr::lag(inflation_mean,1)+dplyr::lag(inflation_mean,2)+dplyr::lag(inflation_mean,3))/3,
    lag_debt_to_GDP_mean = (dplyr::lag(debt_to_GDP,1)+dplyr::lag(debt_to_GDP,2)+dplyr::lag(debt_to_GDP,3))/3,
    lag_real_interest_rate_mean = (dplyr::lag(real_interest_rate,1)+dplyr::lag(real_interest_rate,2)+dplyr::lag(real_interest_rate,3))/3,
    lag_Ep_mean = (dplyr::lag(Ep,1)+dplyr::lag(Ep,2)+dplyr::lag(Ep,3))/3,
    lag_Sp_mean = (dplyr::lag(Sp,1)+dplyr::lag(Sp,2)+dplyr::lag(Sp,3))/3,
    lag_Gp_mean = (dplyr::lag(Gp,1)+dplyr::lag(Gp,2)+dplyr::lag(Gp,3))/3,
    lag_foreign_debt_to_gdp_mean = (dplyr::lag(foreign_debt_to_gdp, 1) +dplyr::lag(foreign_debt_to_gdp, 2) + dplyr::lag(foreign_debt_to_gdp, 3))/3,
    lag_vix_EUA_mean = (dplyr::lag(vix_EUA, 1) + dplyr::lag(vix_EUA, 2) + dplyr::lag(vix_EUA, 3))/ 3)

# LASSO regression --------------------------------------------------------

## Excluding real_interest_rate and taxes due to NA values

lambda.array <- seq(from = 0, to = 1, by = 0.001)

# Lag 0 (no lags) ---------------------------------------------------------

# Identifying the variables

lag_0 <- subset(panel_dataset, select = c(GDP_growth, fx_volatility, nominal_rate, account_balance, lending_borroeing_rate, unemployment, inflation_mean, debt_to_GDP, Ep, Sp, Gp, foreign_debt_to_gdp, vix_EUA, spreads))

lag_0 <- na.omit(lag_0)

x <- subset(lag_0, select = -c(spreads))
x <- scale(x)
y <- lag_0[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_0 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_0_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table0 <- as.data.frame(as.matrix(coef(best_model)))
table0_se = as.data.frame(as.matrix(coef(se_model)))
table0 <- cbind(table0, "new column" = table0_se$s0)
colnames(table0) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table0[nrow(table0) + 1,] = c(MSE_lasso_0, MSE_lasso_0_se)
rownames(table0)[15] <- c("MSE")
View(table0)

# Lag 1 -------------------------------------------------------------------

# Identifying the variables

lag_1 <- subset(panel_dataset_lags, select = c(lag_GDP_growth_1, lag_fx_volatility_1, lag_nominal_rate_1, lag_account_balance_1, lag_lending_borroeing_rate_1, lag_unemployment_1, lag_inflation_mean_1, lag_debt_to_GDP_1, lag_Ep_1, lag_Sp_1, lag_Gp_1, lag_foreign_debt_to_gdp_1, lag_vix_EUA_1, spreads))

lag_1 <- na.omit(lag_1)

x <- subset(lag_1, select = -c(spreads))
x <- scale(x)
y <- lag_1[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_1 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_1_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table1 <- as.data.frame(as.matrix(coef(best_model)))
table1_se = as.data.frame(as.matrix(coef(se_model)))
table1 <- cbind(table1, "new column" = table1_se$s0)
colnames(table1) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table1[nrow(table1) + 1,] = c(MSE_lasso_1, MSE_lasso_1_se)
rownames(table1)[15] <- c("MSE")
View(table1)

# Lag 2 -------------------------------------------------------------------

# Identifying the variables

lag_2 <- subset(panel_dataset_lags_2, select = c(lag_GDP_growth_2, lag_fx_volatility_2, lag_nominal_rate_2, lag_account_balance_2, lag_lending_borroeing_rate_2, lag_unemployment_2, lag_inflation_mean_2, lag_debt_to_GDP_2, lag_Ep_2, lag_Sp_2, lag_Gp_2, lag_foreign_debt_to_gdp_2, lag_vix_EUA_2, spreads))

lag_2 <- na.omit(lag_2)

x <- subset(lag_2, select = -c(spreads))
x <- scale(x)
y <- lag_2[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_2 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_2_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table2 <- as.data.frame(as.matrix(coef(best_model)))
table2_se = as.data.frame(as.matrix(coef(se_model)))
table2 <- cbind(table2, "new column" = table2_se$s0)
colnames(table2) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table2[nrow(table2) + 1,] = c(MSE_lasso_2, MSE_lasso_2_se)
rownames(table2)[15] <- c("MSE")
View(table2)

# Lag 3 -------------------------------------------------------------------

# Identifying the variables

lag_3 <- subset(panel_dataset_lags_3, select = c(lag_GDP_growth_3, lag_fx_volatility_3, lag_nominal_rate_3, lag_account_balance_3, lag_lending_borroeing_rate_3, lag_unemployment_3, lag_inflation_mean_3, lag_debt_to_GDP_3, lag_Ep_3, lag_Sp_3, lag_Gp_3, lag_foreign_debt_to_gdp_3, lag_vix_EUA_3, spreads))

lag_3 <- na.omit(lag_3)

x <- subset(lag_3, select = -c(spreads))
x <- scale(x)
y <- lag_3[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_3 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_3_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table3 <- as.data.frame(as.matrix(coef(best_model)))
table3_se = as.data.frame(as.matrix(coef(se_model)))
table3 <- cbind(table3, "new column" = table0_se$s0)
colnames(table3) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table3[nrow(table3) + 1,] = c(MSE_lasso_3, MSE_lasso_3_se)
rownames(table3)[15] <- c("MSE")
View(table3)

# Mean --------------------------------------------------------------------

# Identifying the variables

mean <- subset(panel_dataset_mean_lags, select = c(lag_GDP_growth_mean, lag_fx_volatility_mean, lag_nominal_rate_mean, lag_account_balance_mean, lag_lending_borroeing_rate_mean, lag_unemployment_mean, lag_inflation_mean_mean, lag_debt_to_GDP_mean, lag_Ep_mean, lag_Sp_mean, lag_Gp_mean, lag_foreign_debt_to_gdp_mean, lag_vix_EUA_mean, spreads))

mean <- na.omit(mean)

x <- subset(mean, select = -c(spreads))
x <- scale(x)
y <- mean[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso_mean <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_mean_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table_mean <- as.data.frame(as.matrix(coef(best_model)))
table_mean_se = as.data.frame(as.matrix(coef(se_model)))
table_mean <- cbind(table_mean, "new column" = table_mean_se$s0)
colnames(table_mean) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table_mean[nrow(table_mean) + 1,] = c(MSE_lasso_mean, MSE_lasso_mean_se)
rownames(table_mean)[15] <- c("MSE")
View(table_mean)


# MSE and RMSE of each lag ------------------------------------------------

MSE_RMSE_EM <- data.frame(row.names = c("Lag_0", "Lag_1", "Lag_2", "Lag_3", "mean"),
                          MSE_best = c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean),
                          MSE_1se = c(MSE_lasso_0_se, MSE_lasso_1_se, MSE_lasso_2_se, MSE_lasso_3_se, MSE_lasso_mean_se),
                          RMSE_best = sqrt(c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean)),
                          RMSE_1se = sqrt(c(MSE_lasso_0_se, MSE_lasso_1_se, MSE_lasso_2_se, MSE_lasso_3_se, MSE_lasso_mean_se)))

### Interpretability x predictive power

View(MSE_RMSE_EM)

# Dynamic Panel -----------------------------------------------------------

rm(list=ls())

# Database ----------------------------------------------------------------

data <- read.csv("Bases/merged2.csv")

panel_dataset <- pdata.frame(data, index = c("country", "year"))

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur  
  )

# Adding Lags + mean ------------------------------------------------------

# 1 lag

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_1 = dplyr::lag(GDP_growth,1),
    lag_fx_volatility_1 = dplyr::lag(fx_volatility,1),
    lag_nominal_rate_1 = dplyr::lag(nominal_rate,1),
    lag_taxes_1 = dplyr::lag(taxes,1),
    lag_account_balance_1 = dplyr::lag(account_balance,1),
    lag_lending_borroeing_rate_1 = dplyr::lag(lending_borroeing_rate,1),
    lag_unemployment_1 = dplyr::lag(unemployment,1),
    lag_inflation_mean_1 = dplyr::lag(inflation_mean,1),
    lag_debt_to_GDP_1 = dplyr::lag(debt_to_GDP,1),
    lag_real_interest_rate_1 = dplyr::lag(real_interest_rate,1),
    lag_Ep_1 = dplyr::lag(Ep,1),
    lag_Sp_1 = dplyr::lag(Sp,1),
    lag_Gp_1 = dplyr::lag(Gp,1),
    lag_foreign_debt_to_gdp_1 = dplyr::lag(foreign_debt_to_gdp, 1),
    lag_vix_EUA_1 = dplyr::lag(vix_EUA, 1))

# 2 lag 

panel_dataset <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_2 = dplyr::lag(GDP_growth,2),    
    GDP_growth_2 = 100*(GDP_cur - dplyr::lag(GDP_cur,2))/dplyr::lag(GDP_cur,2),
    lag_fx_volatility_2 = dplyr::lag(fx_volatility,2),
    lag_nominal_rate_2 = dplyr::lag(nominal_rate,2),
    lag_taxes_2 = dplyr::lag(taxes,2),
    lag_account_balance_2 = dplyr::lag(account_balance,2),
    lag_lending_borroeing_rate_2 = dplyr::lag(lending_borroeing_rate,2),
    lag_unemployment_2 = dplyr::lag(unemployment,2),
    lag_inflation_mean_2 = dplyr::lag(inflation_mean,2),
    lag_debt_to_GDP_2 = dplyr::lag(debt_to_GDP,2),
    lag_real_interest_rate_2 = dplyr::lag(real_interest_rate,2),
    lag_Ep_2 = dplyr::lag(Ep,2),
    lag_Sp_2 = dplyr::lag(Sp,2),
    lag_Gp_2 = dplyr::lag(Gp,2),
    lag_foreign_debt_to_gdp_2 = dplyr::lag(foreign_debt_to_gdp, 2),
    lag_vix_EUA_2 = dplyr::lag(vix_EUA, 2))

# 3 lag

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_3 = dplyr::lag(GDP_growth,3),    
    GDP_growth_3 = 100*(GDP_cur - dplyr::lag(GDP_cur,3))/dplyr::lag(GDP_cur,3),
    lag_fx_volatility_3 = dplyr::lag(fx_volatility,3),
    lag_nominal_rate_3 = dplyr::lag(nominal_rate,3),
    lag_taxes_3 = dplyr::lag(taxes,3),
    lag_account_balance_3 = dplyr::lag(account_balance,3),
    lag_lending_borroeing_rate_3 = dplyr::lag(lending_borroeing_rate,3),
    lag_unemployment_3 = dplyr::lag(unemployment,3),
    lag_inflation_mean_3 = dplyr::lag(inflation_mean,3),
    lag_debt_to_GDP_3 = dplyr::lag(debt_to_GDP,3),
    lag_real_interest_rate_3 = dplyr::lag(real_interest_rate,3),
    lag_Ep_3 = dplyr::lag(Ep,3),
    lag_Sp_3 = dplyr::lag(Sp,3),
    lag_Gp_3 = dplyr::lag(Gp,3),
    lag_foreign_debt_to_gdp_3 = dplyr::lag(foreign_debt_to_gdp, 3),
    lag_vix_EUA_3 = dplyr::lag(vix_EUA, 3))

# Mean

panel_dataset <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    lag_GDP_growth_mean = (dplyr::lag(GDP_growth,1) + dplyr::lag(GDP_growth,2) + dplyr::lag(GDP_growth,3))/3, 
    lag_fx_volatility_mean = (dplyr::lag(fx_volatility,1) +dplyr::lag(fx_volatility,2) +dplyr::lag(fx_volatility,3))/3,
    lag_nominal_rate_mean = (dplyr::lag(nominal_rate,1)+dplyr::lag(nominal_rate,2)+dplyr::lag(nominal_rate,3))/3,
    lag_taxes_mean = (dplyr::lag(taxes,1)+dplyr::lag(taxes,2)+dplyr::lag(taxes,3))/3,
    lag_account_balance_mean = (dplyr::lag(account_balance,1)+dplyr::lag(account_balance,2)+dplyr::lag(account_balance,3))/3,
    lag_lending_borroeing_rate_mean = (dplyr::lag(lending_borroeing_rate,1)+dplyr::lag(lending_borroeing_rate,2)+dplyr::lag(lending_borroeing_rate,3))/3,
    lag_unemployment_mean = (dplyr::lag(unemployment,1)+dplyr::lag(unemployment,2)+dplyr::lag(unemployment,3))/3,
    lag_inflation_mean_mean = (dplyr::lag(inflation_mean,1)+dplyr::lag(inflation_mean,2)+dplyr::lag(inflation_mean,3))/3,
    lag_debt_to_GDP_mean = (dplyr::lag(debt_to_GDP,1)+dplyr::lag(debt_to_GDP,2)+dplyr::lag(debt_to_GDP,3))/3,
    lag_real_interest_rate_mean = (dplyr::lag(real_interest_rate,1)+dplyr::lag(real_interest_rate,2)+dplyr::lag(real_interest_rate,3))/3,
    lag_Ep_mean = (dplyr::lag(Ep,1)+dplyr::lag(Ep,2)+dplyr::lag(Ep,3))/3,
    lag_Sp_mean = (dplyr::lag(Sp,1)+dplyr::lag(Sp,2)+dplyr::lag(Sp,3))/3,
    lag_Gp_mean = (dplyr::lag(Gp,1)+dplyr::lag(Gp,2)+dplyr::lag(Gp,3))/3,
    lag_foreign_debt_to_gdp_mean = (dplyr::lag(foreign_debt_to_gdp, 1) +dplyr::lag(foreign_debt_to_gdp, 2) + dplyr::lag(foreign_debt_to_gdp, 3))/3,
    lag_vix_EUA_mean = (dplyr::lag(vix_EUA, 1) + dplyr::lag(vix_EUA, 2) + dplyr::lag(vix_EUA, 3))/ 3)

# LASSO regression --------------------------------------------------------

## Excluding real_interest_rate and taxes due to NA values

lambda.array <- seq(from = 0, to = 1, by = 0.001)

# Identifying the variables

dynamic <- subset(panel_dataset, select = c(GDP_growth, fx_volatility, GDP_per_cap_cur_USD, nominal_rate, account_balance, lending_borroeing_rate, unemployment, inflation_mean, debt_to_GDP, Ep, Sp, Gp, foreign_debt_to_gdp, vix_EUA, spreads, lag_GDP_growth_1, lag_fx_volatility_1, lag_nominal_rate_1, lag_account_balance_1, lag_lending_borroeing_rate_1, lag_unemployment_1, lag_inflation_mean_1, lag_debt_to_GDP_1, lag_Ep_1, lag_Sp_1, lag_Gp_1, lag_foreign_debt_to_gdp_1, lag_vix_EUA_1, lag_GDP_growth_2, lag_fx_volatility_2, lag_nominal_rate_2, lag_account_balance_2, lag_lending_borroeing_rate_2, lag_unemployment_2, lag_inflation_mean_2, lag_debt_to_GDP_2, lag_Ep_2, lag_Sp_2, lag_Gp_2, lag_foreign_debt_to_gdp_2, lag_vix_EUA_2, lag_GDP_growth_3, lag_fx_volatility_3, lag_nominal_rate_3, lag_account_balance_3, lag_lending_borroeing_rate_3, lag_unemployment_3, lag_inflation_mean_3, lag_debt_to_GDP_3, lag_Ep_3, lag_Sp_3, lag_Gp_3, lag_foreign_debt_to_gdp_3, lag_vix_EUA_3, lag_GDP_growth_mean, lag_fx_volatility_mean, lag_nominal_rate_mean, lag_account_balance_mean, lag_lending_borroeing_rate_mean, lag_unemployment_mean, lag_inflation_mean_mean, lag_debt_to_GDP_mean, lag_Ep_mean, lag_Sp_mean, lag_Gp_mean, lag_foreign_debt_to_gdp_mean, lag_vix_EUA_mean))

dynamic <- na.omit(dynamic)

x <- subset(dynamic, select = -c(spreads))
x <- scale(x)
y <- dynamic[, c("spreads")]
y <- scale(y)

# LASSO regression

## Perform k-fold cross-validation to find optimal lambda value

set.seed(1234)

cv_model <- cv.glmnet(x, y, alpha = 1,  lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Best lambda mean squared error (MSE)

MSE_lasso <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

### Find optimal lambda plus 1se

se_lambda <- cv_model$lambda.1se
se_lambda

### 1se model coefficients

se_model <- glmnet(x, y, alpha = 1, lambda = se_lambda)

### 1se lambda mean squared error (MSE)

MSE_lasso_se <- cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]

# Comparing coefficients and MSE of lambda = best_lambda and lambda + 1se

table <- as.data.frame(as.matrix(coef(best_model)))
table_se = as.data.frame(as.matrix(coef(se_model)))
table <- cbind(table, "new column" = table_se$s0)
colnames(table) <- c(sprintf("best lambda = %s", best_lambda), sprintf("best lambda + 1se = %s", se_lambda))
table[nrow(table) + 1,] = c(MSE_lasso, MSE_lasso_se)
rownames(table)[68] <- c("MSE")
View(table)


# MSE and RMSE of each lag ------------------------------------------------

MSE_RMSE_DP <- data.frame(row.names = c("Dynamic Panel"),
                          MSE_best = c(MSE_lasso),
                          MSE_1se = c(MSE_lasso_se),
                          RMSE_best = sqrt(c(MSE_lasso)),
                          RMSE_se = sqrt(c(MSE_lasso_se)))

### Interpretability x predictive power

View(MSE_RMSE_DP)
