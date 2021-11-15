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
lag_0 <- scale(lag_0)

x <- subset(lag_0, select = -c(spreads))
y <- lag_0[, c("spreads")]

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
ytrain <- subset(train, select = c(spreads))

test <- lag_0[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
ytest <- subset(test, select = c(spreads))

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
lag_1 <- scale(lag_1)

x <- subset(lag_1, select = -c(spreads))
y <- lag_1[, c("spreads")]

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
ytrain <- subset(train, select = c(spreads))

test <- lag_1[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
ytest <- subset(test, select = c(spreads))

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
lag_2 <- scale(lag_2)

x <- subset(lag_2, select = -c(spreads))
y <- lag_2[, c("spreads")]

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
ytrain <- subset(train, select = c(spreads))

test <- lag_2[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
ytest <- subset(test, select = c(spreads))

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
lag_3 <- scale(lag_3)

x <- subset(lag_3, select = -c(spreads))
y <- lag_3[, c("spreads")]

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
ytrain <- subset(train, select = c(spreads))

test <- lag_3[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
ytest <- subset(test, select = c(spreads))

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
mean <- scale(mean)

x <- subset(mean, select = -c(spreads))
y <- mean[, c("spreads")]

la.eq <- glmnet(x, y, lambda=lambda, family="gaussian", intercept = F, alpha=1) 

df_mean.comp <- data.frame(Lasso = la.eq$beta[,1])
df_mean.comp

## Calculating MSE using min(lambda)

### Training x test

size <- floor(0.8 * nrow(mean))

train_ind <- sample(seq_len(nrow(mean)), size = size)

train <- mean[train_ind, ]
xtrain <- subset(train, select = -c(spreads))
ytrain <- subset(train, select = c(spreads))

test <- mean[-train_ind, ]
xtest <- subset(test, select = -c(spreads))
ytest <- subset(test, select = c(spreads))

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

MSE_RMSE <- data.frame(row.names = c("Lag_0", "Lag_1", "Lag_2", "Lag_3", "mean"),
                       MSE = c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean),
                       RMSE = sqrt(c(MSE_lasso_0, MSE_lasso_1, MSE_lasso_2, MSE_lasso_3, MSE_lasso_mean)))

View(MSE_RMSE)

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
