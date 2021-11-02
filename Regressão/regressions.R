library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(stargazer)
library(plm)
library(lmtest)

dataset_total_jan_2021 <- read.csv("C:/Users/mathe/OneDrive/Documentos/GitHub/Macro_2021.2/Bases/merged.csv")

panel_dataset <- pdata.frame(dataset_total_jan_2021, index = c("country", "year"))

panel_dataset_lags_1 <- panel_dataset %>% 
mutate(

lag_fx_volatility_1= dplyr::lag(fx_volatility,1),
lag_GDP_per_cap_cte_1 = dplyr::lag(GDP_per_cap_cte,1),
lag_nominal_rate_1 = dplyr::lag(nominal_rate,1),
lag_taxes_1 = dplyr::lag(taxes,1),
lag_ln_GDP_per_cap_cte_1 = dplyr::lag(ln_GDP_per_cap_cte,1),
lag_account_balance_1 = dplyr::lag(account_balance,1),
lag_lending_borroeing_rate_1 = dplyr::lag(lending_borroeing_rate,1),
lag_unemployment_1 = dplyr::lag(unemployment,1),
lag_inflation_mean_1 = dplyr::lag(inflation_mean,1),
lag_fx_1 = dplyr::lag(fx,1),
lag_debt_to_GDP_1 = dplyr::lag(debt_to_GDP,1),
lag_real_interest_rate_1 = dplyr::lag(real_interest_rate,1),
lag_spreads_1 = dplyr::lag(spreads,1),
lag_ESGIp_1 = dplyr::lag(ESGIp,1),
lag_Sp_1 = dplyr::lag(Sp,1),
lag_Ep_1 = dplyr::lag(Ep,1),
lag_Gp_1 = dplyr::lag(Gp,1))
