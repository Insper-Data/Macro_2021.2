# Fixed Effects -----------------------------------------------------------

# AM + EM -----------------------------------------------------------------

rm(list=ls())

setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")

library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(stargazer)
library(plm)
library(lmtest)

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



# Regressions -------------------------------------------------------------

# Using variables and lag selected by the LASSO regression

# Only E, S, G ------------------------------------------------------------

formula <- spreads ~ Ep + Sp + Gp

reg1.esg <- plm(formula_0, data = panel_dataset, model = "within", effect = "individual")
reg2.esg <- plm(formula_0, data = panel_dataset, model = "within", effect = "time")
reg3.esg <- plm(formula_0, data = panel_dataset, model = "within", effect = "twoways")
reg4.esg <- plm(formula_0, data = panel_dataset, model = "pooling")

# Clusterized errors:
reg1.esg_c <- coeftest(reg1.esg, vcovHC(reg1.esg, type="sss", cluster = "group", method = "white2"))[,2]
reg2.esg_c <- coeftest(reg2.esg, vcovHC(reg2.esg, type="sss", cluster="group", method = "white2"))[,2]
reg3.esg_c <- coeftest(reg3.esg, vcovHC(reg3.esg, type="sss", cluster="group", method = "white2"))[,2]
reg4.esg_c <- coeftest(reg4.esg, vcovHC(reg4.esg, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.esg, 
          reg2.esg, 
          reg3.esg, 
          reg4.esg,
          title = "Apenas E, S, G", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.esg_c, reg2.esg_c, reg3.esg_c, reg4.esg_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_esg.txt")


# Mean (best lambda) ------------------------------------------------------

formula_mean <- spreads ~ lag_GDP_growth_mean + lag_fx_volatility_mean + lag_nominal_rate_mean + lag_account_balance_mean + lag_lending_borroeing_rate_mean + lag_unemployment_mean + lag_inflation_mean_mean + lag_debt_to_GDP_mean + lag_Ep_mean + lag_Sp_mean + lag_Gp_mean + lag_foreign_debt_to_gdp_mean + lag_vix_EUA_mean 

reg1.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "individual")
reg2.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "time")
reg3.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "twoways")
reg4.lag_mean<- plm(formula_mean, data = panel_dataset_mean_lags, model = "pooling")

# Clusterized errors:
reg1.lag_mean_c <- coeftest(reg1.lag_mean, vcovHC(reg1.lag_mean, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_mean_c <- coeftest(reg2.lag_mean, vcovHC(reg2.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_mean_c <- coeftest(reg3.lag_mean, vcovHC(reg3.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_mean_c <- coeftest(reg4.lag_mean, vcovHC(reg4.lag_mean, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_mean, 
          reg2.lag_mean, 
          reg3.lag_mean, 
          reg4.lag_mean,
          title = "Painel Variáveis com a média dos Lags (best lambda)", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_mean_c, reg2.lag_mean_c, reg3.lag_mean_c, reg4.lag_mean_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_mean_AMEM.txt")


# Mean (best lambda + 1se) ------------------------------------------------

formula_mean <- spreads ~ lag_nominal_rate_mean + lag_taxes_mean + lag_account_balance_mean + lag_Gp_mean 

reg1.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "individual")
reg2.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "time")
reg3.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "twoways")
reg4.lag_mean<- plm(formula_mean, data = panel_dataset_mean_lags, model = "pooling")

# Clusterized errors:
reg1.lag_mean_c <- coeftest(reg1.lag_mean, vcovHC(reg1.lag_mean, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_mean_c <- coeftest(reg2.lag_mean, vcovHC(reg2.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_mean_c <- coeftest(reg3.lag_mean, vcovHC(reg3.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_mean_c <- coeftest(reg4.lag_mean, vcovHC(reg4.lag_mean, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_mean, 
          reg2.lag_mean, 
          reg3.lag_mean, 
          reg4.lag_mean,
          title = "Painel Variáveis com a média dos Lags (best lambda + 1se)", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_mean_c, reg2.lag_mean_c, reg3.lag_mean_c, reg4.lag_mean_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_mean_AMEM_se.txt")


# AM ----------------------------------------------------------------------

rm(list=ls())

# Database ----------------------------------------------------------------

data <- read.csv("Bases/merged2.csv")

panel_dataset <- pdata.frame(data, index = c("country", "year"))

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



# Regressions -------------------------------------------------------------


# Lag 0 (no lags) ---------------------------------------------------------

formula_0 <- spreads ~ fx_volatility + nominal_rate + taxes + account_balance + lending_borroeing_rate + unemployment + inflation_mean + debt_to_GDP + real_interest_rate + Ep + Sp + Gp + vix_EUA

reg1.lag_0 <- plm(formula_0, data = panel_dataset, model = "within", effect = "individual")
reg2.lag_0 <- plm(formula_0, data = panel_dataset, model = "within", effect = "time")
reg3.lag_0 <- plm(formula_0, data = panel_dataset, model = "within", effect = "twoways")
reg4.lag_0<- plm(formula_0, data = panel_dataset, model = "pooling")

# Clusterized errors:
reg1.lag_0_c <- coeftest(reg1.lag_0, vcovHC(reg1.lag_0, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_0_c <- coeftest(reg2.lag_0, vcovHC(reg2.lag_0, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_0_c <- coeftest(reg3.lag_0, vcovHC(reg3.lag_0, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_0_c <- coeftest(reg4.lag_0, vcovHC(reg4.lag_0, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_0, 
          reg2.lag_0, 
          reg3.lag_0, 
          reg4.lag_0,
          title = "Painel Variáveis sem Lag", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_0_c, reg2.lag_0_c, reg3.lag_0_c, reg4.lag_0_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_0_AM.txt")

# Lag 1 -------------------------------------------------------------------

formula_1 <- spreads ~ lag_fx_volatility_1 + lag_nominal_rate_1 + lag_taxes_1 + lag_account_balance_1 + lag_lending_borroeing_rate_1 + lag_unemployment_1 + lag_inflation_mean_1 + lag_debt_to_GDP_1 + lag_real_interest_rate_1 + lag_Ep_1 + lag_Sp_1 + lag_Gp_1 + lag_vix_EUA_1 
reg1.lag_1 <- plm(formula_1, data = panel_dataset_lags, model = "within", effect = "individual")
reg2.lag_1 <- plm(formula_1, data = panel_dataset_lags, model = "within", effect = "time")
reg3.lag_1 <- plm(formula_1, data = panel_dataset_lags, model = "within", effect = "twoways")
reg4.lag_1<- plm(formula_1, data = panel_dataset_lags, model = "pooling")

# Clusterized errors:
reg1.lag_1_c <- coeftest(reg1.lag_1, vcovHC(reg1.lag_1, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_1_c <- coeftest(reg2.lag_1, vcovHC(reg2.lag_1, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_1_c <- coeftest(reg3.lag_1, vcovHC(reg3.lag_1, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_1_c <- coeftest(reg4.lag_1, vcovHC(reg4.lag_1, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_1, 
          reg2.lag_1, 
          reg3.lag_1, 
          reg4.lag_1,
          title = "Painel Variáveis com 1 Lag", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_1_c, reg2.lag_1_c, reg3.lag_1_c, reg4.lag_1_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_1_AM.txt")


# Lag 2 -------------------------------------------------------------------

formula_2 <- spreads ~ lag_fx_volatility_2 + lag_nominal_rate_2 + lag_taxes_2 + lag_account_balance_2 + lag_lending_borroeing_rate_2 + lag_unemployment_2 + lag_inflation_mean_2 + lag_debt_to_GDP_2 + lag_real_interest_rate_2 + lag_Ep_2 + lag_Sp_2 + lag_Gp_2 + lag_vix_EUA_2 
reg1.lag_2 <- plm(formula_2, data = panel_dataset_lags_2, model = "within", effect = "individual")
reg2.lag_2 <- plm(formula_2, data = panel_dataset_lags_2, model = "within", effect = "time")
reg3.lag_2 <- plm(formula_2, data = panel_dataset_lags_2, model = "within", effect = "twoways")
reg4.lag_2<- plm(formula_2, data = panel_dataset_lags_2, model = "pooling")

# Clusterized errors:
reg1.lag_2_c <- coeftest(reg1.lag_2, vcovHC(reg1.lag_2, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_2_c <- coeftest(reg2.lag_2, vcovHC(reg2.lag_2, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_2_c <- coeftest(reg3.lag_2, vcovHC(reg3.lag_2, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_2_c <- coeftest(reg4.lag_2, vcovHC(reg4.lag_2, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_2, 
          reg2.lag_2, 
          reg3.lag_2, 
          reg4.lag_2,
          title = "Painel Variáveis com 2 Lags", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_2_c, reg2.lag_2_c, reg3.lag_2_c, reg4.lag_2_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_2_AM.txt")


# Lag 3 -------------------------------------------------------------------

formula_3 <- spreads ~ lag_fx_volatility_3 + lag_nominal_rate_3 + lag_taxes_3 + lag_account_balance_3 + lag_lending_borroeing_rate_3 + lag_unemployment_3 + lag_inflation_mean_3 + lag_debt_to_GDP_3 + lag_real_interest_rate_3 + lag_Ep_3 + lag_Sp_3 + lag_Gp_3 + lag_vix_EUA_3 
reg1.lag_3 <- plm(formula_3, data = panel_dataset_lags_3, model = "within", effect = "individual")
reg2.lag_3 <- plm(formula_3, data = panel_dataset_lags_3, model = "within", effect = "time")
reg3.lag_3 <- plm(formula_3, data = panel_dataset_lags_3, model = "within", effect = "twoways")
reg4.lag_3<- plm(formula_3, data = panel_dataset_lags_3, model = "pooling")

# Clusterized errors:
reg1.lag_3_c <- coeftest(reg1.lag_3, vcovHC(reg1.lag_3, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_3_c <- coeftest(reg2.lag_3, vcovHC(reg1.lag_3, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_3_c <- coeftest(reg3.lag_3, vcovHC(reg3.lag_3, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_3_c <- coeftest(reg4.lag_3, vcovHC(reg4.lag_3, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_3, 
          reg2.lag_3, 
          reg3.lag_3, 
          reg4.lag_3,
          title = "Painel Variáveis com 3 Lags", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_3_c, reg2.lag_3_c, reg3.lag_3_c, reg4.lag_3_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_3_AM.txt")


# Mean --------------------------------------------------------------------

formula_mean <- spreads ~ lag_fx_volatility_mean + lag_nominal_rate_mean + lag_taxes_mean + lag_account_balance_mean + lag_lending_borroeing_rate_mean + lag_unemployment_mean + lag_inflation_mean_mean + lag_debt_to_GDP_mean + lag_real_interest_rate_mean + lag_Ep_mean + lag_Sp_mean + lag_Gp_mean + lag_vix_EUA_mean 

reg1.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "individual")
reg2.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "time")
reg3.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "twoways")
reg4.lag_mean<- plm(formula_mean, data = panel_dataset_mean_lags, model = "pooling")

# Clusterized errors:
reg1.lag_mean_c <- coeftest(reg1.lag_mean, vcovHC(reg1.lag_mean, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_mean_c <- coeftest(reg2.lag_mean, vcovHC(reg2.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_mean_c <- coeftest(reg3.lag_mean, vcovHC(reg3.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_mean_c <- coeftest(reg4.lag_mean, vcovHC(reg4.lag_mean, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_mean, 
          reg2.lag_mean, 
          reg3.lag_mean, 
          reg4.lag_mean,
          title = "Painel Variáveis com a média dos Lags", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_mean_c, reg2.lag_mean_c, reg3.lag_mean_c, reg4.lag_mean_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_mean_AM.txt")


# EM ----------------------------------------------------------------------

rm(list=ls())

# Database ----------------------------------------------------------------

data <- read.csv("Bases/merged2.csv")

panel_dataset <- pdata.frame(data, index = c("country", "year"))

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



# Regressions -------------------------------------------------------------

# Lag 0 (no lags) ---------------------------------------------------------

formula_0 <- spreads ~ fx_volatility + nominal_rate + taxes + account_balance + lending_borroeing_rate + unemployment + inflation_mean + debt_to_GDP + real_interest_rate + Ep + Sp + Gp + vix_EUA

reg1.lag_0 <- plm(formula_0, data = panel_dataset, model = "within", effect = "individual")
reg2.lag_0 <- plm(formula_0, data = panel_dataset, model = "within", effect = "time")
reg3.lag_0 <- plm(formula_0, data = panel_dataset, model = "within", effect = "twoways")
reg4.lag_0<- plm(formula_0, data = panel_dataset, model = "pooling")

# Clusterized errors:
reg1.lag_0_c <- coeftest(reg1.lag_0, vcovHC(reg1.lag_0, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_0_c <- coeftest(reg2.lag_0, vcovHC(reg2.lag_0, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_0_c <- coeftest(reg3.lag_0, vcovHC(reg3.lag_0, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_0_c <- coeftest(reg4.lag_0, vcovHC(reg4.lag_0, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_0, 
          reg2.lag_0, 
          reg3.lag_0, 
          reg4.lag_0,
          title = "Painel Variáveis sem Lag", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_0_c, reg2.lag_0_c, reg3.lag_0_c, reg4.lag_0_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_0_EM.txt")

# Lag 1 -------------------------------------------------------------------

formula_1 <- spreads ~ lag_fx_volatility_1 + lag_nominal_rate_1 + lag_taxes_1 + lag_account_balance_1 + lag_lending_borroeing_rate_1 + lag_unemployment_1 + lag_inflation_mean_1 + lag_debt_to_GDP_1 + lag_real_interest_rate_1 + lag_Ep_1 + lag_Sp_1 + lag_Gp_1 + lag_vix_EUA_1 
reg1.lag_1 <- plm(formula_1, data = panel_dataset_lags, model = "within", effect = "individual")
reg2.lag_1 <- plm(formula_1, data = panel_dataset_lags, model = "within", effect = "time")
reg3.lag_1 <- plm(formula_1, data = panel_dataset_lags, model = "within", effect = "twoways")
reg4.lag_1<- plm(formula_1, data = panel_dataset_lags, model = "pooling")

# Clusterized errors:
reg1.lag_1_c <- coeftest(reg1.lag_1, vcovHC(reg1.lag_1, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_1_c <- coeftest(reg2.lag_1, vcovHC(reg2.lag_1, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_1_c <- coeftest(reg3.lag_1, vcovHC(reg3.lag_1, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_1_c <- coeftest(reg4.lag_1, vcovHC(reg4.lag_1, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_1, 
          reg2.lag_1, 
          reg3.lag_1, 
          reg4.lag_1,
          title = "Painel Variáveis com 1 Lag", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_1_c, reg2.lag_1_c, reg3.lag_1_c, reg4.lag_1_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_1_EM.txt")


# Lag 2 -------------------------------------------------------------------

formula_2 <- spreads ~ lag_fx_volatility_2 + lag_nominal_rate_2 + lag_taxes_2 + lag_account_balance_2 + lag_lending_borroeing_rate_2 + lag_unemployment_2 + lag_inflation_mean_2 + lag_debt_to_GDP_2 + lag_real_interest_rate_2 + lag_Ep_2 + lag_Sp_2 + lag_Gp_2 + lag_vix_EUA_2 
reg1.lag_2 <- plm(formula_2, data = panel_dataset_lags_2, model = "within", effect = "individual")
reg2.lag_2 <- plm(formula_2, data = panel_dataset_lags_2, model = "within", effect = "time")
reg3.lag_2 <- plm(formula_2, data = panel_dataset_lags_2, model = "within", effect = "twoways")
reg4.lag_2<- plm(formula_2, data = panel_dataset_lags_2, model = "pooling")

# Clusterized errors:
reg1.lag_2_c <- coeftest(reg1.lag_2, vcovHC(reg1.lag_2, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_2_c <- coeftest(reg2.lag_2, vcovHC(reg2.lag_2, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_2_c <- coeftest(reg3.lag_2, vcovHC(reg3.lag_2, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_2_c <- coeftest(reg4.lag_2, vcovHC(reg4.lag_2, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_2, 
          reg2.lag_2, 
          reg3.lag_2, 
          reg4.lag_2,
          title = "Painel Variáveis com 2 Lags", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_2_c, reg2.lag_2_c, reg3.lag_2_c, reg4.lag_2_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_2_EM.txt")


# Lag 3 -------------------------------------------------------------------

formula_3 <- spreads ~ lag_fx_volatility_3 + lag_nominal_rate_3 + lag_taxes_3 + lag_account_balance_3 + lag_lending_borroeing_rate_3 + lag_unemployment_3 + lag_inflation_mean_3 + lag_debt_to_GDP_3 + lag_real_interest_rate_3 + lag_Ep_3 + lag_Sp_3 + lag_Gp_3 + lag_vix_EUA_3 
reg1.lag_3 <- plm(formula_3, data = panel_dataset_lags_3, model = "within", effect = "individual")
reg2.lag_3 <- plm(formula_3, data = panel_dataset_lags_3, model = "within", effect = "time")
reg3.lag_3 <- plm(formula_3, data = panel_dataset_lags_3, model = "within", effect = "twoways")
reg4.lag_3<- plm(formula_3, data = panel_dataset_lags_3, model = "pooling")

# Clusterized errors:
reg1.lag_3_c <- coeftest(reg1.lag_3, vcovHC(reg1.lag_3, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_3_c <- coeftest(reg2.lag_3, vcovHC(reg1.lag_3, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_3_c <- coeftest(reg3.lag_3, vcovHC(reg3.lag_3, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_3_c <- coeftest(reg4.lag_3, vcovHC(reg4.lag_3, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_3, 
          reg2.lag_3, 
          reg3.lag_3, 
          reg4.lag_3,
          title = "Painel Variáveis com 3 Lags", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_3_c, reg2.lag_3_c, reg3.lag_3_c, reg4.lag_3_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_3_EM.txt")


# Mean --------------------------------------------------------------------

formula_mean <- spreads ~ lag_fx_volatility_mean + lag_nominal_rate_mean + lag_taxes_mean + lag_account_balance_mean + lag_lending_borroeing_rate_mean + lag_unemployment_mean + lag_inflation_mean_mean + lag_debt_to_GDP_mean + lag_real_interest_rate_mean + lag_Ep_mean + lag_Sp_mean + lag_Gp_mean + lag_vix_EUA_mean 

reg1.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "individual")
reg2.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "time")
reg3.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "twoways")
reg4.lag_mean<- plm(formula_mean, data = panel_dataset_mean_lags, model = "pooling")

# Clusterized errors:
reg1.lag_mean_c <- coeftest(reg1.lag_mean, vcovHC(reg1.lag_mean, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_mean_c <- coeftest(reg2.lag_mean, vcovHC(reg2.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_mean_c <- coeftest(reg3.lag_mean, vcovHC(reg3.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_mean_c <- coeftest(reg4.lag_mean, vcovHC(reg4.lag_mean, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.lag_mean, 
          reg2.lag_mean, 
          reg3.lag_mean, 
          reg4.lag_mean,
          title = "Painel Variáveis com a média dos Lags", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.lag_mean_c, reg2.lag_mean_c, reg3.lag_mean_c, reg4.lag_mean_c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_mean_EM.txt")


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

# Regression -------------------------------------------------------------

formula <- spreads ~ fx_volatility + nominal_rate + taxes + account_balance + lending_borroeing_rate + unemployment + inflation_mean + debt_to_GDP + real_interest_rate + Ep + Sp + Gp + vix_EUA + lag_fx_volatility_1 + lag_nominal_rate_1 + lag_taxes_1 + lag_account_balance_1 + lag_lending_borroeing_rate_1 + lag_unemployment_1 + lag_inflation_mean_1 + lag_debt_to_GDP_1 + lag_real_interest_rate_1 + lag_Ep_1 + lag_Sp_1 + lag_Gp_1 + lag_vix_EUA_1 + lag_fx_volatility_2 + lag_nominal_rate_2 + lag_taxes_2 + lag_account_balance_2 + lag_lending_borroeing_rate_2 + lag_unemployment_2 + lag_inflation_mean_2 + lag_debt_to_GDP_2 + lag_real_interest_rate_2 + lag_Ep_2 + lag_Sp_2 + lag_Gp_2 + lag_vix_EUA_2 + lag_fx_volatility_3 + lag_nominal_rate_3 + lag_taxes_3 + lag_account_balance_3 + lag_lending_borroeing_rate_3 + lag_unemployment_3 + lag_inflation_mean_3 + lag_debt_to_GDP_3 + lag_real_interest_rate_3 + lag_Ep_3 + lag_Sp_3 + lag_Gp_3 + lag_vix_EUA_3 + lag_fx_volatility_mean + lag_nominal_rate_mean + lag_taxes_mean + lag_account_balance_mean + lag_lending_borroeing_rate_mean + lag_unemployment_mean + lag_inflation_mean_mean + lag_debt_to_GDP_mean + lag_real_interest_rate_mean + lag_Ep_mean + lag_Sp_mean + lag_Gp_mean + lag_vix_EUA_mean

reg1 <- plm(formula, data = panel_dataset, model = "within", effect = "individual")
reg2 <- plm(formula, data = panel_dataset, model = "within", effect = "time")
reg3 <- plm(formula, data = panel_dataset, model = "within", effect = "twoways")
reg4 <- plm(formula, data = panel_dataset, model = "pooling")

# Clusterized errors:
reg1._c <- coeftest(reg1, vcovHC(reg1, type="sss", cluster = "group", method = "white2"))[,2]
reg2._c <- coeftest(reg2, vcovHC(reg2, type="sss", cluster="group", method = "white2"))[,2]
reg3._c <- coeftest(reg3, vcovHC(reg3, type="sss", cluster="group", method = "white2"))[,2]
reg4._c <- coeftest(reg4, vcovHC(reg4, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1, 
          reg2, 
          reg3, 
          reg4,
          title = "Painel Variáveis sem Lag", type = "text", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1._c, reg2._c, reg3._c, reg4._c),
          omit.stat = "f",
          dep.var.labels = c("Spreads de títulos de 10 anos"),
          out = "Regressão/EF_dynamic.txt")