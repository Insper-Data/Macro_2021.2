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
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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

cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Mean squared error (MSE)

MSE_lasso_0 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

# Comparing coefficients and MSE of lambda = best_lambda and 0.1

table0 <- as.data.frame(as.matrix(coef(best_model)))
colnames(table0) <- c("Best lambda")

## lambda = 0.1

set.seed(1234)
cv_model_ <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = seq(from = 0.1, to = 1, by = 0.0001))
best_lambda_ <- cv_model_$lambda.min
best_model_ <- glmnet(x, y, alpha = 1, lambda = best_lambda_)
MSE_lasso_0_ <- cv_model_$cvm[cv_model_$lambda == cv_model_$lambda.min]

## Adjusting data

table0_ = as.data.frame(as.matrix(coef(best_model_)))
table0 <- cbind(table0, "lambda = 0.1" = table0_$s0)

table0[nrow(table0) + 1,] = c(MSE_lasso_0, MSE_lasso_0_)
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

cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Mean squared error (MSE)

MSE_lasso_1 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

# Comparing coefficients and MSE of lambda = best_lambda and 0.1

table1 <- as.data.frame(as.matrix(coef(best_model)))
colnames(table1) <- c("Best lambda")

## lambda = 0.1

set.seed(1234)
cv_model_ <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = seq(from = 0.1, to = 1, by = 0.0001))
best_lambda_ <- cv_model_$lambda.min
best_model_ <- glmnet(x, y, alpha = 1, lambda = best_lambda_)
MSE_lasso_1_ <- cv_model_$cvm[cv_model_$lambda == cv_model_$lambda.min]

## Adjusting data

table1_ = as.data.frame(as.matrix(coef(best_model_)))
table1 <- cbind(table1, "lambda = 0.1" = table1_$s0)

table1[nrow(table1) + 1,] = c(MSE_lasso_1, MSE_lasso_1_)
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

cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Mean squared error (MSE)

MSE_lasso_2 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

# Comparing coefficients and MSE of lambda = best_lambda and 0.1

table2 <- as.data.frame(as.matrix(coef(best_model)))
colnames(table2) <- c("Best lambda")

## lambda = 0.1

set.seed(1234)
cv_model_ <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = seq(from = 0.1, to = 1, by = 0.0001))
best_lambda_ <- cv_model_$lambda.min
best_model_ <- glmnet(x, y, alpha = 1, lambda = best_lambda_)
MSE_lasso_2_ <- cv_model_$cvm[cv_model_$lambda == cv_model_$lambda.min]

## Adjusting data

table2_ = as.data.frame(as.matrix(coef(best_model_)))
table2 <- cbind(table2, "lambda = 0.1" = table2_$s0)

table2[nrow(table2) + 1,] = c(MSE_lasso_2, MSE_lasso_2_)
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

cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Mean squared error (MSE)

MSE_lasso_3 <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

# Comparing coefficients and MSE of lambda = best_lambda and 0.1

table3 <- as.data.frame(as.matrix(coef(best_model)))
colnames(table3) <- c("Best lambda")

## lambda = 0.1

set.seed(1234)
cv_model_ <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = seq(from = 0.1, to = 1, by = 0.0001))
best_lambda_ <- cv_model_$lambda.min
best_model_ <- glmnet(x, y, alpha = 1, lambda = best_lambda_)
MSE_lasso_3_ <- cv_model_$cvm[cv_model_$lambda == cv_model_$lambda.min]

## Adjusting data

table3_ = as.data.frame(as.matrix(coef(best_model_)))
table3 <- cbind(table3, "lambda = 0.1" = table3_$s0)

table3[nrow(table3) + 1,] = c(MSE_lasso_3, MSE_lasso_3_)
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

cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = lambda.array)

### Find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

### Produce plot of test MSE by lambda value

plot(cv_model)

### Best model (best lambda) coefficients

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

### Mean squared error (MSE)

MSE_lasso_mean <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

# Comparing coefficients and MSE of lambda = best_lambda and 0.1

table_mean <- as.data.frame(as.matrix(coef(best_model)))
colnames(table_mean) <- c("Best lambda")

## lambda = 0.1

set.seed(1234)
cv_model_ <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = seq(from = 0.1, to = 1, by = 0.0001))
best_lambda_ <- cv_model_$lambda.min
best_model_ <- glmnet(x, y, alpha = 1, lambda = best_lambda_)
MSE_lasso_mean_ <- cv_model_$cvm[cv_model_$lambda == cv_model_$lambda.min]

## Adjusting data

table_mean_ = as.data.frame(as.matrix(coef(best_model_)))
table_mean <- cbind(table_mean, "lambda = 0.1" = table_mean_$s0)

table_mean[nrow(table_mean) + 1,] = c(MSE_lasso_mean, MSE_lasso_mean_)
rownames(table_mean)[15] <- c("MSE")
View(table_mean)

# MSE and RMSE of each lag ------------------------------------------------

MSE_RMSE_AMEM <- data.frame(row.names = c("Lag_0", "Lag_1", "Lag_2", "Lag_3", "mean"),
                            MSE_best = c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean),
                            MSE_0.1 = c(MSE_lasso_0_, MSE_lasso_1_, MSE_lasso_2_, MSE_lasso_3_, MSE_lasso_mean_),
                            RMSE_best = sqrt(c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean)),
                            RMSE_0.1 = sqrt(c(MSE_lasso_0_, MSE_lasso_1_, MSE_lasso_2_, MSE_lasso_3_, MSE_lasso_mean_)))

### Interpretability x predictive power

View(MSE_RMSE_AMEM)

#--------------------------------------------------------------------------------------------

# Só avançados

setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")

rm(list=ls())

# Libraries
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(stargazer)
library(plm)
library(lmtest)
library(glmnet)

# Calling our dataset

dataset_total_jan_2021 <- read.csv("Bases/merged.csv")

# Setting panel dataset
panel_dataset <- pdata.frame(dataset_total_jan_2021, index = c("country", "year"))

#---------------

# Panel dataset for AM 
panel_dataset <- panel_dataset %>% 
  filter(develop == "AM",
         country != "United States")

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur  
  )

#---------------

# Creating LAGS + MEAN

# Panel dataset - 1 lag added

panel_dataset_lags <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_1 = dplyr::lag(ESGIp,1),
    lag_Ep_1 = dplyr::lag(Ep,1),
    lag_Sp_1 = dplyr::lag(Sp,1),
    lag_Gp_1 = dplyr::lag(Gp,1),
    lag_foreign_debt_to_gdp_1 = dplyr::lag(foreign_debt_to_gdp, 1),
    lag_vix_EUA_1 = dplyr::lag(vix_EUA, 1),
    lag_vix_EUR_1 = dplyr::lag(vix_EUR, 1))

# Panel dataset - 2 lag added

panel_dataset_lags_2 <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_2 = dplyr::lag(ESGIp,2),
    lag_Ep_2 = dplyr::lag(Ep,2),
    lag_Sp_2 = dplyr::lag(Sp,2),
    lag_Gp_2 = dplyr::lag(Gp,2),
    lag_foreign_debt_to_gdp_2 = dplyr::lag(foreign_debt_to_gdp, 2),
    lag_vix_EUA_2 = dplyr::lag(vix_EUA, 2),
    lag_vix_EUR_2 = dplyr::lag(vix_EUR, 2))

# Panel dataset - 3 lag added

panel_dataset_lags_3 <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_3 = dplyr::lag(ESGIp,3),
    lag_Ep_3 = dplyr::lag(Ep,3),
    lag_Sp_3 = dplyr::lag(Sp,3),
    lag_Gp_3 = dplyr::lag(Gp,3),
    lag_foreign_debt_to_gdp_3 = dplyr::lag(foreign_debt_to_gdp, 3),
    lag_vix_EUA_3 = dplyr::lag(vix_EUA, 3),
    lag_vix_EUR_3 = dplyr::lag(vix_EUR, 3))

# Panel dataset - mean lags added

panel_dataset_mean_lags <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_mean = (dplyr::lag(ESGIp,1)+dplyr::lag(ESGIp,2)+dplyr::lag(ESGIp,3))/3,
    lag_Ep_mean = (dplyr::lag(Ep,1)+dplyr::lag(Ep,2)+dplyr::lag(Ep,3))/3,
    lag_Sp_mean = (dplyr::lag(Sp,1)+dplyr::lag(Sp,2)+dplyr::lag(Sp,3))/3,
    lag_Gp_mean = (dplyr::lag(Gp,1)+dplyr::lag(Gp,2)+dplyr::lag(Gp,3))/3,
    lag_foreign_debt_to_gdp_mean = (dplyr::lag(foreign_debt_to_gdp, 1) +dplyr::lag(foreign_debt_to_gdp, 2) + dplyr::lag(foreign_debt_to_gdp, 3))/3,
    lag_vix_EUA_mean = (dplyr::lag(vix_EUA, 1) + dplyr::lag(vix_EUA, 2) + dplyr::lag(vix_EUA, 3))/ 3,
    lag_vix_EUR_mean = (dplyr::lag(vix_EUR, 1) + dplyr::lag(vix_EUR, 2) + dplyr::lag(vix_EUR, 3))/ 3)

#---------------

# LASSO

## Excluding real_interest_rate and taxes due to NA values

lambda <- 0.01

lambda.array <- seq(from = 0.01, to = 5, by = 0.01)

#---------------

# Lag 0

## Identifying the variables

lag_0 <- subset(panel_dataset, select = c(GDP_growth, fx_volatility, GDP_per_cap_cur_USD, nominal_rate, account_balance, lending_borroeing_rate, unemployment, inflation_mean, debt_to_GDP, ESGIp, Ep, Sp, Gp, foreign_debt_to_gdp, vix_EUA, vix_EUR, spreads))
lag_0 <- na.omit(lag_0)

x <- subset(lag_0, select = -c(spreads))
x <- scale(x)
y <- lag_0[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df1.comp <- data.frame(Lasso   = la.eq$beta[,1])
df1.comp

## Calculating MSE using min(lambda)

### Training x test

set.seed(12345)

size <- floor(0.8 * nrow(lag_0))

train_ind <- sample(seq_len(nrow(lag_0)), size = size)

train <- lag_0[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- lag_0[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

### LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

## Goodness of fit

plot(lassofit, xvar = "dev", label = T)

## Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_0 <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_0)

#---------------

# Lag 1

## Identifying the variables

lag_1 <- subset(panel_dataset_lags, select = c(lag_GDP_growth_1, lag_fx_volatility_1, lag_nominal_rate_1, lag_account_balance_1, lag_lending_borroeing_rate_1, lag_unemployment_1, lag_inflation_mean_1, lag_debt_to_GDP_1, lag_ESGIp_1, lag_Ep_1, lag_Sp_1, lag_Gp_1, lag_foreign_debt_to_gdp_1, lag_vix_EUA_1, lag_vix_EUR_1, spreads))
lag_1 <- na.omit(lag_1)

x <- subset(lag_1, select = -c(spreads))
x <- scale(x)
y <- lag_1[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df1.comp <- data.frame(Lasso   = la.eq$beta[,1])
df1.comp

## Calculating MSE using min(lambda)

### Training x test

set.seed(12345)

size <- floor(0.8 * nrow(lag_1))

train_ind <- sample(seq_len(nrow(lag_1)), size = size)

train <- lag_1[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- lag_1[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

## LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

# Goodness of fit

plot(lassofit, xvar = "dev", label = T)

# Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_1 <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_1)

#---------------

# Lag 2

## Identifying the variables

lag_2 <- subset(panel_dataset_lags_2, select = c(lag_GDP_growth_2, lag_fx_volatility_2, lag_nominal_rate_2, lag_account_balance_2, lag_lending_borroeing_rate_2, lag_unemployment_2, lag_inflation_mean_2, lag_debt_to_GDP_2, lag_ESGIp_2, lag_Ep_2, lag_Sp_2, lag_Gp_2, lag_foreign_debt_to_gdp_2, lag_vix_EUA_2, lag_vix_EUR_2, spreads))
lag_2 <- na.omit(lag_2)

x <- subset(lag_2, select = -c(spreads))
x <- scale(x)
y <- lag_2[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df2.comp <- data.frame(Lasso   = la.eq$beta[,1])
df2.comp

## Calculating MSE using min(lambda)

### Training x test

set.seed(12345)

size <- floor(0.8 * nrow(lag_2))

train_ind <- sample(seq_len(nrow(lag_2)), size = size)

train <- lag_2[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- lag_2[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

## LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

# Goodness of fit

plot(lassofit, xvar = "dev", label = T)

# Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_2 <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_2)

#---------------

# Lag 3

## Identifying the variables

lag_3 <- subset(panel_dataset_lags_3, select = c(lag_GDP_growth_3, lag_fx_volatility_3, lag_nominal_rate_3, lag_account_balance_3, lag_lending_borroeing_rate_3, lag_unemployment_3, lag_inflation_mean_3, lag_debt_to_GDP_3, lag_ESGIp_3, lag_Ep_3, lag_Sp_3, lag_Gp_3, lag_foreign_debt_to_gdp_3, lag_vix_EUA_3, lag_vix_EUR_3, spreads))
lag_3 <- na.omit(lag_3)

x <- subset(lag_3, select = -c(spreads))
x <- scale(x)
y <- lag_3[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df3.comp <- data.frame(Lasso   = la.eq$beta[,1])
df3.comp

## Calculating MSE using min(lambda)

### Training x test

set.seed(12345)

size <- floor(0.8 * nrow(lag_3))

train_ind <- sample(seq_len(nrow(lag_3)), size = size)

train <- lag_3[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- lag_3[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

## LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

# Goodness of fit

plot(lassofit, xvar = "dev", label = T)

# Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_3 <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_3)

#---------------

# mean

## Identifying the variables

mean <- subset(panel_dataset_mean_lags, select = c(lag_GDP_growth_mean, lag_fx_volatility_mean, lag_nominal_rate_mean, lag_account_balance_mean, lag_lending_borroeing_rate_mean, lag_unemployment_mean, lag_inflation_mean_mean, lag_debt_to_GDP_mean, lag_ESGIp_mean, lag_Ep_mean, lag_Sp_mean, lag_Gp_mean, lag_foreign_debt_to_gdp_mean, lag_vix_EUA_mean, lag_vix_EUR_mean, spreads))
mean <- na.omit(mean)

x <- subset(mean, select = -c(spreads))
x <- scale(x)
y <- mean[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df_mean.comp <- data.frame(Lasso = la.eq$beta[,1])
df_mean.comp

## Calculating MSE using min(lambda)

### Training x test

size <- floor(0.8 * nrow(mean))

train_ind <- sample(seq_len(nrow(mean)), size = size)

train <- mean[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- mean[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

## LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

# Goodness of fit

plot(lassofit, xvar = "dev", label = T)

# Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_mean <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_mean)

#---------------

# MSE and RMSE

MSE_RMSE_AM <- data.frame(row.names = c("Lag_0", "Lag_1", "Lag_2", "Lag_3", "mean"),
                          MSE = c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean),
                          RMSE = sqrt(c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean)))

View(MSE_RMSE_AM)

#---------------

# Rascunho

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#--------------------------------------------------------------------------------------------

# Só emergentes

setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")

rm(list=ls())

# Libraries
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(stargazer)
library(plm)
library(lmtest)
library(glmnet)

# Calling our dataset

dataset_total_jan_2021 <- read.csv("Bases/merged.csv")

# Setting panel dataset
panel_dataset <- pdata.frame(dataset_total_jan_2021, index = c("country", "year"))

#---------------

# Panel dataset for EM
panel_dataset <- panel_dataset %>% 
  filter(develop == "EM",
         country != "United States")

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur  
  )

#---------------

# Creating LAGS + MEAN

# Panel dataset - 1 lag added

panel_dataset_lags <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_1 = dplyr::lag(ESGIp,1),
    lag_Ep_1 = dplyr::lag(Ep,1),
    lag_Sp_1 = dplyr::lag(Sp,1),
    lag_Gp_1 = dplyr::lag(Gp,1),
    lag_foreign_debt_to_gdp_1 = dplyr::lag(foreign_debt_to_gdp, 1),
    lag_vix_EUA_1 = dplyr::lag(vix_EUA, 1),
    lag_vix_EUR_1 = dplyr::lag(vix_EUR, 1))

# Panel dataset - 2 lag added

panel_dataset_lags_2 <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_2 = dplyr::lag(ESGIp,2),
    lag_Ep_2 = dplyr::lag(Ep,2),
    lag_Sp_2 = dplyr::lag(Sp,2),
    lag_Gp_2 = dplyr::lag(Gp,2),
    lag_foreign_debt_to_gdp_2 = dplyr::lag(foreign_debt_to_gdp, 2),
    lag_vix_EUA_2 = dplyr::lag(vix_EUA, 2),
    lag_vix_EUR_2 = dplyr::lag(vix_EUR, 2))

# Panel dataset - 3 lag added

panel_dataset_lags_3 <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_3 = dplyr::lag(ESGIp,3),
    lag_Ep_3 = dplyr::lag(Ep,3),
    lag_Sp_3 = dplyr::lag(Sp,3),
    lag_Gp_3 = dplyr::lag(Gp,3),
    lag_foreign_debt_to_gdp_3 = dplyr::lag(foreign_debt_to_gdp, 3),
    lag_vix_EUA_3 = dplyr::lag(vix_EUA, 3),
    lag_vix_EUR_3 = dplyr::lag(vix_EUR, 3))

# Panel dataset - mean lags added

panel_dataset_mean_lags <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_mean = (dplyr::lag(ESGIp,1)+dplyr::lag(ESGIp,2)+dplyr::lag(ESGIp,3))/3,
    lag_Ep_mean = (dplyr::lag(Ep,1)+dplyr::lag(Ep,2)+dplyr::lag(Ep,3))/3,
    lag_Sp_mean = (dplyr::lag(Sp,1)+dplyr::lag(Sp,2)+dplyr::lag(Sp,3))/3,
    lag_Gp_mean = (dplyr::lag(Gp,1)+dplyr::lag(Gp,2)+dplyr::lag(Gp,3))/3,
    lag_foreign_debt_to_gdp_mean = (dplyr::lag(foreign_debt_to_gdp, 1) +dplyr::lag(foreign_debt_to_gdp, 2) + dplyr::lag(foreign_debt_to_gdp, 3))/3,
    lag_vix_EUA_mean = (dplyr::lag(vix_EUA, 1) + dplyr::lag(vix_EUA, 2) + dplyr::lag(vix_EUA, 3))/ 3,
    lag_vix_EUR_mean = (dplyr::lag(vix_EUR, 1) + dplyr::lag(vix_EUR, 2) + dplyr::lag(vix_EUR, 3))/ 3)

#---------------

# LASSO

## Excluding real_interest_rate and taxes due to NA values

lambda <- 0.01

lambda.array <- seq(from = 0.01, to = 5, by = 0.01)

#---------------

# Lag 0

## Identifying the variables

lag_0 <- subset(panel_dataset, select = c(GDP_growth, fx_volatility, GDP_per_cap_cur_USD, nominal_rate, account_balance, lending_borroeing_rate, unemployment, inflation_mean, debt_to_GDP, ESGIp, Ep, Sp, Gp, foreign_debt_to_gdp, vix_EUA, vix_EUR, spreads))
lag_0 <- na.omit(lag_0)

x <- subset(lag_0, select = -c(spreads))
x <- scale(x)
y <- lag_0[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df1.comp <- data.frame(Lasso   = la.eq$beta[,1])
df1.comp

## Calculating MSE using min(lambda)

### Training x test

set.seed(12345)

size <- floor(0.8 * nrow(lag_0))

train_ind <- sample(seq_len(nrow(lag_0)), size = size)

train <- lag_0[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- lag_0[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

### LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

## Goodness of fit

plot(lassofit, xvar = "dev", label = T)

## Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_0 <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_0)

#---------------

# Lag 1

## Identifying the variables

lag_1 <- subset(panel_dataset_lags, select = c(lag_GDP_growth_1, lag_fx_volatility_1, lag_nominal_rate_1, lag_account_balance_1, lag_lending_borroeing_rate_1, lag_unemployment_1, lag_inflation_mean_1, lag_debt_to_GDP_1, lag_ESGIp_1, lag_Ep_1, lag_Sp_1, lag_Gp_1, lag_foreign_debt_to_gdp_1, lag_vix_EUA_1, lag_vix_EUR_1, spreads))
lag_1 <- na.omit(lag_1)

x <- subset(lag_1, select = -c(spreads))
x <- scale(x)
y <- lag_1[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df1.comp <- data.frame(Lasso   = la.eq$beta[,1])
df1.comp

## Calculating MSE using min(lambda)

### Training x test

set.seed(12345)

size <- floor(0.8 * nrow(lag_1))

train_ind <- sample(seq_len(nrow(lag_1)), size = size)

train <- lag_1[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- lag_1[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

## LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

# Goodness of fit

plot(lassofit, xvar = "dev", label = T)

# Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_1 <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_1)

#---------------

# Lag 2

## Identifying the variables

lag_2 <- subset(panel_dataset_lags_2, select = c(lag_GDP_growth_2, lag_fx_volatility_2, lag_nominal_rate_2, lag_account_balance_2, lag_lending_borroeing_rate_2, lag_unemployment_2, lag_inflation_mean_2, lag_debt_to_GDP_2, lag_ESGIp_2, lag_Ep_2, lag_Sp_2, lag_Gp_2, lag_foreign_debt_to_gdp_2, lag_vix_EUA_2, lag_vix_EUR_2, spreads))
lag_2 <- na.omit(lag_2)

x <- subset(lag_2, select = -c(spreads))
x <- scale(x)
y <- lag_2[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df2.comp <- data.frame(Lasso   = la.eq$beta[,1])
df2.comp

## Calculating MSE using min(lambda)

### Training x test

set.seed(12345)

size <- floor(0.8 * nrow(lag_2))

train_ind <- sample(seq_len(nrow(lag_2)), size = size)

train <- lag_2[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- lag_2[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

## LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

# Goodness of fit

plot(lassofit, xvar = "dev", label = T)

# Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_2 <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_2)

#---------------

# Lag 3

## Identifying the variables

lag_3 <- subset(panel_dataset_lags_3, select = c(lag_GDP_growth_3, lag_fx_volatility_3, lag_nominal_rate_3, lag_account_balance_3, lag_lending_borroeing_rate_3, lag_unemployment_3, lag_inflation_mean_3, lag_debt_to_GDP_3, lag_ESGIp_3, lag_Ep_3, lag_Sp_3, lag_Gp_3, lag_foreign_debt_to_gdp_3, lag_vix_EUA_3, lag_vix_EUR_3, spreads))
lag_3 <- na.omit(lag_3)

x <- subset(lag_3, select = -c(spreads))
x <- scale(x)
y <- lag_3[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df3.comp <- data.frame(Lasso   = la.eq$beta[,1])
df3.comp

## Calculating MSE using min(lambda)

### Training x test

set.seed(12345)

size <- floor(0.8 * nrow(lag_3))

train_ind <- sample(seq_len(nrow(lag_3)), size = size)

train <- lag_3[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- lag_3[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

## LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

# Goodness of fit

plot(lassofit, xvar = "dev", label = T)

# Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_3 <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_3)

#---------------

# mean

## Identifying the variables

mean <- subset(panel_dataset_mean_lags, select = c(lag_GDP_growth_mean, lag_fx_volatility_mean, lag_nominal_rate_mean, lag_account_balance_mean, lag_lending_borroeing_rate_mean, lag_unemployment_mean, lag_inflation_mean_mean, lag_debt_to_GDP_mean, lag_ESGIp_mean, lag_Ep_mean, lag_Sp_mean, lag_Gp_mean, lag_foreign_debt_to_gdp_mean, lag_vix_EUA_mean, lag_vix_EUR_mean, spreads))
mean <- na.omit(mean)

x <- subset(mean, select = -c(spreads))
x <- scale(x)
y <- mean[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df_mean.comp <- data.frame(Lasso = la.eq$beta[,1])
df_mean.comp

## Calculating MSE using min(lambda)

### Training x test

size <- floor(0.8 * nrow(mean))

train_ind <- sample(seq_len(nrow(mean)), size = size)

train <- mean[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- mean[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

## LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

# Goodness of fit

plot(lassofit, xvar = "dev", label = T)

# Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso_mean <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(MSE_lasso_mean)

#---------------

# MSE and RMSE

MSE_RMSE_EM <- data.frame(row.names = c("Lag_0", "Lag_1", "Lag_2", "Lag_3", "mean"),
                          MSE = c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean),
                          RMSE = sqrt(c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean)))

View(MSE_RMSE_EM)

#---------------

# Rascunho

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#--------------------------------------------------------------------------------------------

# Todos os lags juntos

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

#---------------

dataset_total_jan_2021 <- read.csv("Bases/merged.csv")

panel_dataset <- pdata.frame(dataset_total_jan_2021, index = c("country", "year"))

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur  
  )

#---------------

# Creating LAGS + MEAN

# Panel dataset - 1 lag added

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_1 = dplyr::lag(ESGIp,1),
    lag_Ep_1 = dplyr::lag(Ep,1),
    lag_Sp_1 = dplyr::lag(Sp,1),
    lag_Gp_1 = dplyr::lag(Gp,1),
    lag_foreign_debt_to_gdp_1 = dplyr::lag(foreign_debt_to_gdp, 1),
    lag_vix_EUA_1 = dplyr::lag(vix_EUA, 1),
    lag_vix_EUR_1 = dplyr::lag(vix_EUR, 1))

# Panel dataset - 2 lag added

panel_dataset <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_2 = dplyr::lag(ESGIp,2),
    lag_Ep_2 = dplyr::lag(Ep,2),
    lag_Sp_2 = dplyr::lag(Sp,2),
    lag_Gp_2 = dplyr::lag(Gp,2),
    lag_foreign_debt_to_gdp_2 = dplyr::lag(foreign_debt_to_gdp, 2),
    lag_vix_EUA_2 = dplyr::lag(vix_EUA, 2),
    lag_vix_EUR_2 = dplyr::lag(vix_EUR, 2))

# Panel dataset - 3 lag added

panel_dataset <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_3 = dplyr::lag(ESGIp,3),
    lag_Ep_3 = dplyr::lag(Ep,3),
    lag_Sp_3 = dplyr::lag(Sp,3),
    lag_Gp_3 = dplyr::lag(Gp,3),
    lag_foreign_debt_to_gdp_3 = dplyr::lag(foreign_debt_to_gdp, 3),
    lag_vix_EUA_3 = dplyr::lag(vix_EUA, 3),
    lag_vix_EUR_3 = dplyr::lag(vix_EUR, 3))

# Panel dataset - mean lags added

panel_dataset <- panel_dataset %>%
  group_by(country) %>% 
  mutate(
    
    # Adding GDP Growth
    GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
    # Adding foreign debt to GDP
    foreign_debt_to_gdp = foreign_debt / GDP_cur,
    # Lags
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
    lag_ESGIp_mean = (dplyr::lag(ESGIp,1)+dplyr::lag(ESGIp,2)+dplyr::lag(ESGIp,3))/3,
    lag_Ep_mean = (dplyr::lag(Ep,1)+dplyr::lag(Ep,2)+dplyr::lag(Ep,3))/3,
    lag_Sp_mean = (dplyr::lag(Sp,1)+dplyr::lag(Sp,2)+dplyr::lag(Sp,3))/3,
    lag_Gp_mean = (dplyr::lag(Gp,1)+dplyr::lag(Gp,2)+dplyr::lag(Gp,3))/3,
    lag_foreign_debt_to_gdp_mean = (dplyr::lag(foreign_debt_to_gdp, 1) +dplyr::lag(foreign_debt_to_gdp, 2) + dplyr::lag(foreign_debt_to_gdp, 3))/3,
    lag_vix_EUA_mean = (dplyr::lag(vix_EUA, 1) + dplyr::lag(vix_EUA, 2) + dplyr::lag(vix_EUA, 3))/ 3,
    lag_vix_EUR_mean = (dplyr::lag(vix_EUR, 1) + dplyr::lag(vix_EUR, 2) + dplyr::lag(vix_EUR, 3))/ 3)

#---------------

# LASSO

## Excluding real_interest_rate and taxes due to NA values

lambda <- 0.01

lambda.array <- seq(from = 0.01, to = 5, by = 0.01)

#---------------

# Lag 0

## Identifying the variables

lag_0 <- subset(panel_dataset, select = c(GDP_growth, fx_volatility, GDP_per_cap_cur_USD, nominal_rate, account_balance, lending_borroeing_rate, unemployment, inflation_mean, debt_to_GDP, ESGIp, Ep, Sp, Gp, foreign_debt_to_gdp, vix_EUA, vix_EUR, spreads, lag_GDP_growth_1, lag_fx_volatility_1, lag_nominal_rate_1, lag_account_balance_1, lag_lending_borroeing_rate_1, lag_unemployment_1, lag_inflation_mean_1, lag_debt_to_GDP_1, lag_ESGIp_1, lag_Ep_1, lag_Sp_1, lag_Gp_1, lag_foreign_debt_to_gdp_1, lag_vix_EUA_1, lag_vix_EUR_1, lag_GDP_growth_2, lag_fx_volatility_2, lag_nominal_rate_2, lag_account_balance_2, lag_lending_borroeing_rate_2, lag_unemployment_2, lag_inflation_mean_2, lag_debt_to_GDP_2, lag_ESGIp_2, lag_Ep_2, lag_Sp_2, lag_Gp_2, lag_foreign_debt_to_gdp_2, lag_vix_EUA_2, lag_vix_EUR_2, lag_GDP_growth_3, lag_fx_volatility_3, lag_nominal_rate_3, lag_account_balance_3, lag_lending_borroeing_rate_3, lag_unemployment_3, lag_inflation_mean_3, lag_debt_to_GDP_3, lag_ESGIp_3, lag_Ep_3, lag_Sp_3, lag_Gp_3, lag_foreign_debt_to_gdp_3, lag_vix_EUA_3, lag_vix_EUR_3, lag_GDP_growth_mean, lag_fx_volatility_mean, lag_nominal_rate_mean, lag_account_balance_mean, lag_lending_borroeing_rate_mean, lag_unemployment_mean, lag_inflation_mean_mean, lag_debt_to_GDP_mean, lag_ESGIp_mean, lag_Ep_mean, lag_Sp_mean, lag_Gp_mean, lag_foreign_debt_to_gdp_mean, lag_vix_EUA_mean, lag_vix_EUR_mean))
lag_0 <- na.omit(lag_0)

x <- subset(lag_0, select = -c(spreads))
x <- scale(x)
y <- lag_0[, c("spreads")]
y <- scale(y)

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df1.comp <- data.frame(Lasso   = la.eq$beta[,1])
df1.comp

## Calculating MSE using min(lambda)

### Training x test

set.seed(12345)

size <- floor(0.8 * nrow(lag_0))

train_ind <- sample(seq_len(nrow(lag_0)), size = size)

train <- lag_0[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
xtrain <- scale(xtrain)
ytrain <- subset(train, select = c(spreads))
ytrain <- scale(ytrain)

test <- lag_0[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
xtest <- scale(xtest)
ytest <- subset(test, select = c(spreads))
ytest <- scale(ytest)

### LASSO model

lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array)

plot(lassofit, xvar = "lambda", label = T)

## Goodness of fit

plot(lassofit, xvar = "dev", label = T)

## Predicted values

y_predicted_lasso <- predict(lassofit, s = min(lambda.array), newx = xtest)

# MSE

MSE_lasso <- sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso)
print(sqrt(MSE_lasso))

#---------------

# Rascunho

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

