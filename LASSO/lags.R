library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(stargazer)
library(plm)
library(lmtest)



dataset_total_jan_2021 <- read.csv("C:/Users/mathe/OneDrive/Documentos/GitHub/Macro_2021.2/Bases/merged.csv")

panel_dataset <- pdata.frame(dataset_total_jan_2021, index = c("country", "year"))


# Panel dataset 1 lag added

panel_dataset_lags <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(

GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
lag_GDP_growth_1 = dplyr::lag(GDP_growth,1),
lag_fx_volatility_1 = dplyr::lag(fx_volatility,1),
lag_GDP_per_cap_cur_USD_1 = dplyr::lag(GDP_per_cap_cur_USD,1),
lag_nominal_rate_1 = dplyr::lag(nominal_rate,1),
lag_taxes_1 = dplyr::lag(taxes,1),
lag_ln_GDP_per_cap_cte_1 = dplyr::lag(ln_GDP_per_cap_cte,1),
lag_account_balance_1 = dplyr::lag(account_balance,1),
lag_lending_borroeing_rate_1 = dplyr::lag(lending_borroeing_rate,1),
lag_unemployment_1 = dplyr::lag(unemployment,1),
lag_inflation_mean_1 = dplyr::lag(inflation_mean,1),
lag_debt_to_GDP_1 = dplyr::lag(debt_to_GDP,1),
lag_real_interest_rate_1 = dplyr::lag(real_interest_rate,1),
lag_ESGIp_1 = dplyr::lag(ESGIp,1),
lag_Ep_1 = dplyr::lag(Ep,1),
lag_Sp_1 = dplyr::lag(Sp,1),
lag_vix_EUA_1 = dplyr::lag(vix_EUA, 1),
lag_Gp_1 = dplyr::lag(Gp,1))


# Panel dataset 2 lag added

panel_dataset_lags_2 <- panel_dataset %>%
  group_by(country) %>% 
mutate(

GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
lag_GDP_growth_2 = dplyr::lag(GDP_growth,2),    
GDP_growth_2 = 100*(GDP_cur - dplyr::lag(GDP_cur,2))/dplyr::lag(GDP_cur,2),
lag_fx_volatility_2 = dplyr::lag(fx_volatility,2),
lag_GDP_per_cap_cur_USD_2 = dplyr::lag(GDP_per_cap_cur_USD,2),
lag_nominal_rate_2 = dplyr::lag(nominal_rate,2),
lag_taxes_2 = dplyr::lag(taxes,2),
lag_ln_GDP_per_cap_cte_2 = dplyr::lag(ln_GDP_per_cap_cte,2),
lag_account_balance_2 = dplyr::lag(account_balance,2),
lag_lending_borroeing_rate_2 = dplyr::lag(lending_borroeing_rate,2),
lag_unemployment_2 = dplyr::lag(unemployment,2),
lag_inflation_mean_2 = dplyr::lag(inflation_mean,2),
lag_debt_to_GDP_2 = dplyr::lag(debt_to_GDP,2),
lag_real_interest_rate_2 = dplyr::lag(real_interest_rate,2),
lag_ESGIp_2 = dplyr::lag(ESGIp,2),
lag_Ep_2 = dplyr::lag(Ep,2),
lag_Sp_2 = dplyr::lag(Sp,2),
lag_vix_EUA_2 = dplyr::lag(vix_EUA, 2),
lag_Gp_2 = dplyr::lag(Gp,2))



# Panel dataset 3 lag added

panel_dataset_lags_3 <- panel_dataset %>% 
  group_by(country) %>% 
  mutate(

GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
lag_GDP_growth_3 = dplyr::lag(GDP_growth,3),    
GDP_growth_3 = 100*(GDP_cur - dplyr::lag(GDP_cur,3))/dplyr::lag(GDP_cur,3),
lag_fx_volatility_3 = dplyr::lag(fx_volatility,3),
lag_GDP_per_cap_cur_USD_3 = dplyr::lag(GDP_per_cap_cur_USD,3),
lag_nominal_rate_3 = dplyr::lag(nominal_rate,3),
lag_taxes_3 = dplyr::lag(taxes,3),
lag_ln_GDP_per_cap_cte_3 = dplyr::lag(ln_GDP_per_cap_cte,3),
lag_account_balance_3 = dplyr::lag(account_balance,3),
lag_lending_borroeing_rate_3 = dplyr::lag(lending_borroeing_rate,3),
lag_unemployment_3 = dplyr::lag(unemployment,3),
lag_inflation_mean_3 = dplyr::lag(inflation_mean,3),
lag_debt_to_GDP_3 = dplyr::lag(debt_to_GDP,3),
lag_real_interest_rate_3 = dplyr::lag(real_interest_rate,3),
lag_ESGIp_3 = dplyr::lag(ESGIp,3),
lag_Ep_3 = dplyr::lag(Ep,3),
lag_Sp_3 = dplyr::lag(Sp,3),
lag_vix_EUA_3 = dplyr::lag(vix_EUA, 3),
lag_Gp_3 = dplyr::lag(Gp,3))



# Panel dataset mean lags added

panel_dataset_mean_lags <- panel_dataset %>%
  group_by(country) %>% 
  mutate(

GDP_growth = 100*(GDP_cur - dplyr::lag(GDP_cur,1))/dplyr::lag(GDP_cur,1),
lag_GDP_growth_mean = (dplyr::lag(GDP_growth,1) + dplyr::lag(GDP_growth,2) + dplyr::lag(GDP_growth,3))/3, 
lag_fx_volatility_mean = (dplyr::lag(fx_volatility,1) +dplyr::lag(fx_volatility,2) +dplyr::lag(fx_volatility,3))/3,
lag_GDP_per_cap_cur_USD_mean = (dplyr::lag(GDP_per_cap_cur_USD,1) +dplyr::lag(GDP_per_cap_cur_USD,2)+dplyr::lag(GDP_per_cap_cur_USD,3))/3,
lag_nominal_rate_mean = (dplyr::lag(nominal_rate,1)+dplyr::lag(nominal_rate,2)+dplyr::lag(nominal_rate,3))/3,
lag_taxes_mean = (dplyr::lag(taxes,1)+dplyr::lag(taxes,2)+dplyr::lag(taxes,3))/3,
lag_ln_GDP_per_cap_cte_mean = (dplyr::lag(ln_GDP_per_cap_cte,1)+dplyr::lag(ln_GDP_per_cap_cte,2)+dplyr::lag(ln_GDP_per_cap_cte,3))/3,
lag_account_balance_mean = (dplyr::lag(account_balance,1)+dplyr::lag(account_balance,2)+dplyr::lag(account_balance,3))/3,
lag_lending_borroeing_rate_mean = (dplyr::lag(lending_borroeing_rate,1)+dplyr::lag(lending_borroeing_rate,2)+dplyr::lag(lending_borroeing_rate,3))/3,
lag_unemployment_mean = (dplyr::lag(unemployment,1)+dplyr::lag(unemployment,2)+dplyr::lag(unemployment,3))/3,
lag_inflation_mean_mean = (dplyr::lag(inflation_mean,1)+dplyr::lag(inflation_mean,2)+dplyr::lag(inflation_mean,3))/3,
lag_debt_to_GDP_mean = (dplyr::lag(debt_to_GDP,1)+dplyr::lag(debt_to_GDP,2)+dplyr::lag(debt_to_GDP,3))/3,
lag_real_interest_rate_mean = (dplyr::lag(real_interest_rate,1)+dplyr::lag(real_interest_rate,2)+dplyr::lag(real_interest_rate,3))/3,
lag_ESGIp_mean = (dplyr::lag(ESGIp,1)+dplyr::lag(ESGIp,2)+dplyr::lag(ESGIp,3))/3,
lag_Ep_mean = (dplyr::lag(Ep,1)+dplyr::lag(Ep,2)+dplyr::lag(Ep,3))/3,
lag_Sp_mean = (dplyr::lag(Sp,1)+dplyr::lag(Sp,2)+dplyr::lag(Sp,3))/3,
lag_vix_EUA_mean = (dplyr::lag(vix_EUA, 1)+dplyr::lag(vix_EUA, 2)+dplyr::lag(vix_EUA, 3))/3,
lag_Gp_mean = (dplyr::lag(Gp,1)+dplyr::lag(Gp,2)+dplyr::lag(Gp,3))/3)


#--------------------------------------------------------------------------------------------
# TABLE 0 -> T-0 LAGS
#--------------------------------------------------------------------------------------------

# Formula for dynamic model:
formula_0 <- spreads ~ fx_volatility + nominal_rate + taxes + account_balance + lending_borroeing_rate + unemployment + inflation_mean + debt_to_GDP + real_interest_rate + Ep + Sp + Gp + vix_EUA

# Dynamic Panel regressions:
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
#stargazer(reg1.lag_0, 
      #    reg2.lag_0, 
       #   reg3.lag_0, 
        #  reg4.lag_0,
         # title = "Painel Dinâmico variáveis sem Lag", type = "html", 
          #covariate.labels = c("Volatilidade do Câmbio", "Taxa de juros Nominal", "Impostos", "Conta Corrente", "Taxa de juros de empréstimos bancários", "Desemprego", "Inflação média", "Dívida/PIB", "Taxa de juros real", "Ep", "Sp", "Gp", "vix_EUA"),
          #dep.var.labels = c("Spreads de títulos de 10 anos"))
  

#--------------------------------------------------------------------------------------------
# TABLE 1 -> T-1 LAGS
#--------------------------------------------------------------------------------------------

# Formula for dynamic model:
formula_1 <- spreads ~ lag_fx_volatility_1 + lag_nominal_rate_1 + lag_taxes_1 + lag_account_balance_1 + lag_lending_borroeing_rate_1 + lag_unemployment_1 + lag_inflation_mean_1 + lag_debt_to_GDP_1 + lag_real_interest_rate_1 + lag_Ep_1 + lag_Sp_1 + lag_Gp_1 + lag_vix_EUA_1 

# Dynamic Panel regressions:
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
#stargazer(reg1.lag_0, 
#    reg2.lag_0, 
#   reg3.lag_0, 
#  reg4.lag_0,
# title = "Painel Dinâmico variáveis sem Lag", type = "html", 
#covariate.labels = c("Volatilidade do Câmbio", "Taxa de juros Nominal", "Impostos", "Conta Corrente", "Taxa de juros de empréstimos bancários", "Desemprego", "Inflação média", "Dívida/PIB", "Taxa de juros real", "Ep", "Sp", "Gp", "vix_EUA"),
#dep.var.labels = c("Spreads de títulos de 10 anos"))

#--------------------------------------------------------------------------------------------
# TABLE 4 -> T-mean
#--------------------------------------------------------------------------------------------

# Formula for dynamic model:
formula_mean <- spreads ~ lag_fx_volatility_mean + lag_nominal_rate_mean + lag_taxes_mean + lag_account_balance_mean + lag_lending_borroeing_rate_mean + lag_unemployment_mean + lag_inflation_mean_mean + lag_debt_to_GDP_mean + lag_real_interest_rate_mean + lag_Ep_mean + lag_Sp_mean + lag_Gp_mean + lag_vix_EUA_mean 

# Dynamic Panel regressions:
reg1.lag_mean <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "individual")
reg2.lag_1 <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "time")
reg3.lag_1 <- plm(formula_mean, data = panel_dataset_mean_lags, model = "within", effect = "twoways")
reg4.lag_1<- plm(formula_mean, data = panel_dataset_mean_lags, model = "pooling")


# Clusterized errors:
reg1.lag_mean_c <- coeftest(reg1.lag_mean, vcovHC(reg1.lag_mean, type="sss", cluster = "group", method = "white2"))[,2]
reg2.lag_mean_c <- coeftest(reg2.lag_mean, vcovHC(reg2.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg3.lag_mean_c <- coeftest(reg3.lag_mean, vcovHC(reg3.lag_mean, type="sss", cluster="group", method = "white2"))[,2]
reg4.lag_mean_c <- coeftest(reg4.lag_mean, vcovHC(reg4.lag_mean, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
#stargazer(reg1.lag_0, 
#    reg2.lag_0, 
#   reg3.lag_0, 
#  reg4.lag_0,
# title = "Painel Dinâmico variáveis sem Lag", type = "html", 
#covariate.labels = c("Volatilidade do Câmbio", "Taxa de juros Nominal", "Impostos", "Conta Corrente", "Taxa de juros de empréstimos bancários", "Desemprego", "Inflação média", "Dívida/PIB", "Taxa de juros real", "Ep", "Sp", "Gp", "vix_EUA"),
#dep.var.labels = c("Spreads de títulos de 10 anos"))


                   