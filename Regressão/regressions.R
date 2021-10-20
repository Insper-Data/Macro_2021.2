setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")

rm(list = ls())

library(plm)

data <- read.csv("Bases/merged.csv")

data <- pdata.frame(data, index = c("country", "year"))

ft1 <- spreads ~ Ep + Sp + Gp + debt_to_GDP + inflation_mean + unemployment + GDP_per_cap_cur_USD

reg3.1.1 <- plm(ft1, data = data, model = "within", effect = "individual")

reg3.1.2 <- plm(ft1, data = data, model = "within", effect = "time")

reg3.1.3 <- plm(ft1, data = data, model = "within", effect = "twoways")

reg3.1.4 <- plm(ft1, data = data, model = "pooling")
