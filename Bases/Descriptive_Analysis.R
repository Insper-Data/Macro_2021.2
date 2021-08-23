
# MAY 2021  

# Script to generate figures used in our paper

# Authors: Victor H. Alexandrino and Diogo Guillen


#--------------------------------------------------------------------------------------------

setwd("~/Google Drive/PhD Insper/Thesis/Paper 3/Empirics/Data/Datasets/Paper/Datasets/")

# Libraries
library(scales)
library(dplyr)
library(ggthemes)
library(spData)
library(readr)
library(readxl)
library(plotly)
library(sf)
library(viridis)
library(ggcharts)
library(ggrepel)
library(cowplot)
library(tidyr)
library(grid)
library(forecast)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(mFilter)
library(devtools)
library(methods)
library(stats)
library(datasets)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(Hmisc)

# Calling our dataset -> From paper1_join_tidy_datasets_jan_2021

dataset_total <- read.csv("dataset_total_ago_2021.csv")

## delta*(1 - alpha) = Domestic Nominal Debt
## delta*(alpha) = Domestic Real Debt

dataset_total <- dataset_total %>% 
  mutate(delta_nom = delta*(1 - alpha),
         delta_real = delta*alpha,
         one_delta_nom = (1 - delta)*(1 - kappa),
         one_delta_real = kappa*(1 - delta))

# Creating growth

summary(dataset_total$ln_GDP_cur_billions)

dataset_total <- dataset_total %>% 
  group_by(country) %>% 
  mutate(lag_ln_GDP_cur_billions = Lag(ln_GDP_cur_billions)) 

dataset_total_gdp <- dataset_total %>% 
  select(c("year","country","ln_GDP_cur_billions","lag_ln_GDP_cur_billions"))
  

summary(dataset_total$lag_ln_GDP_cur_billions)

dataset_total <- dataset_total %>% 
  group_by(country) %>% 
  mutate(gdp_growth = ln_GDP_cur_billions - lag_ln_GDP_cur_billions)

summary(dataset_total$gdp_growth)

head(dataset_total)

list(dataset_total$country)

ls(dataset_total)

summary(dataset_total$bcom_price)

### Data for Brazil and Argentina

## Argentina

dataset_argentina <- dataset_total %>% 
  filter(country == "Argentina")

# Filtering Log(GDP)

hp_gdp_argentina <- hpfilter(dataset_argentina$ln_GDP_usd_cur_billions, freq = 6.25)

# Adding the cyclical and trend component in the dataset

dataset_argentina <- dataset_argentina %>% 
                      mutate(hp_cycle = hp_gdp_argentina$cycle,
                             hp_trend = hp_gdp_argentina$trend)

write.xlsx(dataset_argentina, file = "dataset_argentina.xlsx", append = F)

## Brazil

dataset_brazil <- dataset_total %>% 
  filter(country == "Brazil")

# Filtering Log(GDP)

hp_gdp_brazil <- hpfilter(dataset_brazil$ln_GDP_usd_cur_billions, freq = 6.25)

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(dataset_brazil$ln_GDP_usd_cur_billions, ylab = "")  # plot time series
lines(hp_gdp_brazil$trend, col = "red")  # include HP trend
legend("topleft", legend = c("data", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(hp_gdp_brazil$cycle, ylab = "")  # plot cycle
legend("topleft", legend = c("HPcycle"), lty = 1, col = c("black"), 
       bty = "n")

# Adding the cyclical and trend component in the dataset

dataset_brazil <- dataset_brazil %>% 
  mutate(hp_cycle = hp_gdp_brazil$cycle,
         hp_trend = hp_gdp_brazil$trend)

# Fitting AR(1)

AR_gdp_brazil <- lm(ln_GDP_usd_cur_billions ~ lag(ln_GDP_usd_cur_billions) + 0,data = dataset_brazil)
summary(AR_gdp_brazil)

AR_trend_brazil <- lm(hp_trend ~ lag(hp_trend) + 0, data=dataset_brazil)
summary(AR_trend_brazil)

AR_cycle_brazil <- lm(hp_cycle ~ lag(hp_cycle) + 0, data = dataset_brazil)
summary(AR_cycle_brazil)

acf(dataset_brazil$hp_cycle, lag.max = 10)

write.xlsx(dataset_brazil, file = "dataset_brazil.xlsx", append = F)

#write_csv(dataset_total, "paper1_dataset_total_may_2021.csv", append = F)

#--------------------------------------------------------------------------------------------
#     STYLIZED FACTS ABOUT THE DATABASE
#--------------------------------------------------------------------------------------------

# 1. The importance of debt
debt_to_gdp_graph <- dataset_total %>% 
  rename(Development = develop) %>% 
  group_by(Development, year) %>% 
  summarise(debt_to_GDP = mean(debt_to_GDP)) %>% 
  ggplot(aes(x = year, y = debt_to_GDP, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Debt-to-GDP Ratio (%)", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004,2019,by=2), name = "Year") +
  ylim(30,90)+
  theme_bw()

debt_to_gdp_graph

# 2. 

dataset_total %>%
  filter(year %in% c(2004, 2010, 2014, 2019), !is.na(develop)) %>%
  rename(Development = develop) %>% 
  bar_chart(x = country, y = debt_to_GDP, facet = year, top_n = 10, fill = Development) +
  labs( x = "", y = "Debt-to-GDP Ratio (%)",
        title = "", fill = "Development") +
  theme_classic() +
  scale_fill_manual("Development", values = c("EM" = "red4", "AM" = "navyblue"))+
  theme(legend.title = element_text(face = "bold", size = 10))

# 3.

graph_debt_foreign_pp <- dataset_total %>% 
  filter(!is.na(develop)) %>% 
  rename(Development = develop) %>% 
  group_by(year, Development) %>% 
  summarise(foreign_participation_percent_GDP = mean(foreign_participation_percent_GDP)) %>% 
  ggplot(aes(x = year, y = foreign_participation_percent_GDP, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004,2019,by=2), name = "Year") +
  #ylim(50,160)+
  theme_light()

graph_debt_foreign_pp



# 4.

dataset_total %>%
  rename(Development = develop) %>% 
  filter(year %in% c(2004, 2010, 2014, 2019), !is.na(Development)) %>%
  bar_chart(x = country, y = (foreign_participation_percent_GDP), facet = year, top_n = 10, fill = Development) +  
  labs( x = "", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)",
        title = "", fill = "Development") +
  theme_classic() +
  theme(legend.title = element_text(face = "bold", size = 10)) +
  scale_fill_manual("Development", values = c("EM" = "red4", "AM" = "navyblue"))



#################################################
########### Composition of debt #################
#################################################


# (GGall) General government debt: Table 1, 1:25
# • (ExtGGall) External general government debt: Table 2, 1:25
# • (GG) General government debt securities: Table 1, 27:52
# • (ExtGG) External general government debt securities: Table 2, 53:78
# • (LCCG) Local currency central government debt securities: Table 1, 54:79
# • (ExtLCCG) External local currency central government debt securities: Table 2, 107:132
# • (GGy) General government debt to GDP: FX, 27:51


### Delta: Domestic share of debt

# Delta = 1 - ExtGGall/GGall = 1 - External general gov debt/General government debt

### x: Nominal share of debt

# x = LCCG/GG = LC central government debt securities/general government debt = LC debt

# 1 - x = FC debt

### External share of nominal debt = y = External share of LC debt

# y = (1 - delta)*(1 - kappa)/(x) = ExtLCCG/LCCG = External LC central government debt securities/LC central government debt securities

### Kappa = real share of external debt = FC share of external debt = External debt in FC

# Kappa = 1 - xy/(1-delta) = 1 - (nominal share of debt * external share of nominal debt) / external share of debt 

### Alpha = Real share of domestic debt = Domestic debt in FC

# Alpha = 1 - x*(1 - y)/delta = 1 - (nominal share of debt * domestic share of nominal debt) / doemstic share of debt



# 5.1 Delta vs Alpha: Only for EM

dataset_total %>%
  group_by(country) %>% 
  mutate(mean_delta = mean(delta, na.rm = T),
         mean_alpha = mean(alpha, na.rm = T)) %>%
  ggplot() +
  geom_point(aes(x = mean_delta, y = mean_alpha,
                 label = country)) +
  geom_label(aes(x = mean_delta, y = mean_alpha,
                 label = country), 
            alpha = .3) +
  scale_color_manual(values = "navyblue") +
  theme_gray() +
  labs(x = "Average Domestic Share of Debt Holdings (δ)", y = "FC Share of Domestic Debt (α)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))

# 5.2. 1 - Delta vs Kappa

dataset_total %>%
  group_by(country) %>% 
  mutate(mean_one_delta = mean(1 - delta, na.rm = T),
         mean_kappa = mean(kappa, na.rm = T)) %>%
  ggplot() +
  geom_point(aes(x = mean_one_delta, y = mean_kappa,
                 label = country)) +
  geom_label(aes(x = mean_one_delta, y = mean_kappa,
                 label = country), 
             alpha = .3) +
  scale_color_manual(values = "navyblue") +
  theme_gray() +
  labs(x = "Average External Share of Debt Holdings (1 - δ)", y = "FC Share of External Debt (κ)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))

# 5.2. 1 - Delta vs LC share (x)

dataset_total %>%
  group_by(country) %>% 
  mutate(mean_delta = mean(delta, na.rm = T),
         mean_x = mean(x, na.rm = T)) %>%
  ggplot() +
  # geom_point(aes(x = mean_delta, y = mean_x,
  #                label = country)) +
  geom_label(aes(x = mean_delta, y = mean_x,
                 label = country), 
             alpha = .3,
             check_overlap = TRUE) +
  geom_smooth(aes(x = mean_delta, y = mean_x, method = 'lm')) +
  stat_cor(aes(x = mean_delta, y = mean_x, p.accuracy = 0.001,r.accuracy = 0.01)) +
  xlim(0.3,1.0) +
  scale_color_manual(values = "navyblue") +
  theme_gray() +
 labs(x = "Average Domestic Share of Debt Holdings", y = "Average Nominal Share of Debt") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

# Only for Brazil


dataset_brazil %>%
  ggplot() +
  # geom_point(aes(x = mean_delta, y = mean_x,
  #                label = country)) +
  geom_point(aes(x = delta, y = x), 
             alpha = .3,
             check_overlap = TRUE) +
  geom_smooth(aes(x = delta, y = x, method = 'lm')) +
  stat_cor(aes(x = delta, y = x, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = "navyblue") +
  theme_gray() +
  labs(x = "Average Domestic Share of Debt Holdings", y = "Average Nominal Share of Debt") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")


# 6. Evolution Delta 

graph_delta_evol <- dataset_total %>% 
  filter(!is.na(develop)) %>% 
  rename(Development = develop) %>% 
  group_by(year, Development) %>% 
  summarise(delta = mean(delta)) %>% 
  ggplot(aes(x = year, y = delta, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Average Domestic Share of Debt Holdings (δ)", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004,2019,by=2), name = "Year") +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)) +
  theme_gray()) 

graph_delta_evol

# Evolution Domestic Share by Debt Type (Nominal and Real)

# Domestic Share of Nominal Debt

dataset_total %>%
  group_by(year) %>% 
  mutate(mean_delta_nom = mean(delta_nom, na.rm = T)) %>%
  ggplot() +
  geom_point(aes(x = year, y = mean_delta_nom)) +
  geom_line(aes(x = year, y = mean_delta_nom), 
            delta_nom = .3) +
  scale_color_manual(values = "navyblue") +
  theme_gray() +
  labs(x = "Year", y = "Domestic Share of Nominal Debt") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))

# Domestic Share of Real Debt

dataset_total %>%
  group_by(year) %>% 
  mutate(mean_delta_real = mean(delta_real, na.rm = T)) %>%
  ggplot() +
  geom_point(aes(x = year, y = mean_delta_real)) +
  geom_line(aes(x = year, y = mean_delta_real), 
            delta_real = .3) +
  scale_color_manual(values = "navyblue") +
  theme_gray() +
  labs(x = "Year", y = "Domestic Share of Real Debt") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))

# Domestic Share of Real and Nominal Debt Together

fig_domestic_denomination <- dataset_total %>% 
  group_by(year) %>% 
  mutate(mean_delta_real = mean(delta_real, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = mean_delta_real, color = "Real"), group = 1) +
  geom_point(aes(x = year, y = mean_delta_nom, color = "Nominal")) +
  geom_line(aes(x = year, y = mean_delta_real, color = "Real"), show.legend = TRUE) +
  geom_line(aes(x = year, y = mean_delta_nom, color = "Nominal"), show.legend = TRUE) +
  scale_x_continuous(limits = c(2004, 2019), seq(2004,2019,by=3), name = "Year") +
  theme_gray() + 
  labs(x = "Year", y = "Average Domestic Share of Debt") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom") + 
  scale_colour_manual(name = "Debt Denomination",
                      values = c(Real="navyblue",
                                 Nominal="red4"))

fig_domestic_denomination

# Evolution growth and BCOM

fig_growth_bcom <- dataset_total %>% 
  group_by(year) %>% 
  mutate(mean_growth = mean(gdp_growth, na.rm = T),
         mean_bcom_price = mean(bcom_price, na.rm = T),
         mean_bcom_growth = mean(bcom_growth, na.rm = T)) %>% 
  ggplot() + 
  geom_point(aes(x = year, y = mean_growth, color = "GDP Growth")) + 
  geom_point(aes(x = year, y = mean_bcom_growth, color = "BCOM Index")) + 
  geom_line(aes(x = year, y = mean_growth, color = "GDP Growth")) + 
  geom_point(aes(x = year, y = mean_bcom_growth, color = "BCOM Index")) +
  scale_x_continuous(limits = c(2004, 2019), seq(2004,2019,by=3), name = "Year") +
  theme_gray() + 
  labs(x = "Year", y = "Growth") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom") + 
  scale_colour_manual(name = "Variable",
                      values = c("GDP Growth" = "navyblue",
                                 "BCOM Index" = "red4"))


fig_growth_bcom

# Regression BCOM and Growth

fig_growth_bcom_reg <- dataset_total %>%
  group_by(year) %>% 
  mutate(mean_growth = mean(gdp_growth, na.rm = T),
         mean_bcom_price = mean(bcom_price, na.rm = T),
         mean_bcom_growth = mean(bcom_growth, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(x = mean_bcom_price, y = mean_growth), alpha = .2) +
  stat_cor(aes(x = mean_bcom_price, y = mean_growth, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = mean_bcom_price, y = mean_growth, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Bloomberg Commodities Index", y = "Average GDP growth (%)")  +
  theme(text = element_text(size = 15),
  axis.text.y = element_text(margin = margin(l = 8)),
  axis.text.x = element_text(margin = margin(b = 8)),
  legend.position = "bottom")

fig_growth_bcom_reg

# 7. Evolution Alpha -> only for EM

dataset_total %>%
  group_by(year) %>% 
  mutate(mean_alpha = mean(alpha, na.rm = T)) %>%
  ggplot() +
  geom_point(aes(x = year, y = mean_alpha)) +
  geom_line(aes(x = year, y = mean_alpha), 
             alpha = .3) +
  scale_color_manual(values = "navyblue") +
  theme_gray() +
  labs(x = "Year", y = "FC Share of Domestic Debt (α)") +
  theme(text = element_text(size = 15),
      axis.text.y = element_text(margin = margin(l = 8)),
      axis.text.x = element_text(margin = margin(b = 8)))

# 8. Evolution Kappa -> only for EM

dataset_total %>%
  group_by(year) %>% 
  mutate(mean_kappa = mean(kappa, na.rm = T)) %>%
  ggplot() +
  geom_point(aes(x = year, y = mean_kappa)) +
  geom_line(aes(x = year, y = mean_kappa), 
            alpha = .3) +
  scale_color_manual(values = "navyblue") +
  theme_gray() +
  labs(x = "Year", y = "Average Real Share of External Debt (α)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))

# 8. Inflation vs Delta

# All obs

dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = delta, y = mean_inflation, colour = develop), alpha = .5) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
#  labs(x = "Mean Inflation Between 2004-2019 (%)", y = "Foreign Participation in Sovereign\n Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  guides(col=guide_legend("")) +
  theme_light() +
  theme(axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

#  Only EM

# Total Domestic Debt

fig_dom_inflation_total <- dataset_total %>%
  group_by(country) %>% 
  filter(develop == "EM") %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T),
         mean_delta_real = mean(delta_real, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_delta, y = mean_inflation), alpha = .2) +
  stat_cor(aes(x = mean_delta, y = mean_inflation, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = mean_delta, y = mean_inflation, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Domestic Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_inflation_total

 ## With only Brazil

fig_dom_inflation_total_brazil <- dataset_brazil %>%
  ggplot() +
  geom_point(aes(x = delta, y = inflation_end), alpha = .2) +
  stat_cor(aes(x = delta, y = inflation_end, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = delta, y = inflation_end, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Domestic Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_inflation_total_brazil

 ## With only Argentina

fig_dom_inflation_total_argentina <- dataset_argentina %>%
  ggplot() +
  geom_point(aes(x = delta, y = inflation_end), alpha = .2) +
  stat_cor(aes(x = delta, y = inflation_end, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = delta, y = inflation_end, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Domestic Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_inflation_total_argentina

# Total External Debt x Inflation

fig_ext_inflation_total <- dataset_total %>%
  group_by(country) %>% 
  filter(develop == "EM") %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T),
         mean_delta_real = mean(delta_real, na.rm = T),
         mean_one_delta_nom = mean(one_delta_nom, na.rm = T),
         mean_one_delta_real = mean(one_delta_real, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = 1 - mean_delta, y = mean_inflation), alpha = .2) +
  stat_cor(aes(x = 1 - mean_delta, y = mean_inflation, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = 1 - mean_delta, y = mean_inflation, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average External Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_ext_inflation_total

# Nominal Debt

fig_dom_inflation_nominal <- dataset_total %>%
  group_by(country) %>% 
  filter(develop == "EM") %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T),
         mean_delta_real = mean(delta_real, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_delta_nom, y = mean_inflation), alpha = .2) +
  stat_cor(aes(x = mean_delta_nom, y = mean_inflation, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = mean_delta_nom, y = mean_inflation, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Domestic Share of Nominal Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_inflation_nominal

 ## Only Brazil

fig_dom_inflation_nominal_brazil <- dataset_brazil %>%
  ggplot() +
  geom_point(aes(x = delta_nom, y = inflation_end), alpha = .2) +
  stat_cor(aes(x = delta_nom, y = inflation_end, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = delta_nom, y = inflation_end, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Nominal Domestic Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_inflation_nominal_brazil

 ## Only Argentina

fig_dom_inflation_nominal_argentina <- dataset_argentina %>%
  ggplot() +
  geom_point(aes(x = delta_nom, y = inflation_end), alpha = .2) +
  stat_cor(aes(x = delta_nom, y = inflation_end, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = delta_nom, y = inflation_end, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Nominal Domestic Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_inflation_nominal_argentina

# External Debt vs Inflation - Only nominal debt

fig_ext_inflation_nominal <- dataset_total %>%
  group_by(country) %>% 
  filter(develop == "EM") %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T),
         mean_delta_real = mean(delta_real, na.rm = T),
         mean_one_delta_nom = mean(one_delta_nom, na.rm = T),
         mean_one_delta_real = mean(one_delta_real, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_one_delta_nom, y = mean_inflation), alpha = .2) +
  stat_cor(aes(x = mean_one_delta_nom, y = mean_inflation, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = mean_one_delta_nom, y = mean_inflation, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average External Share of Nominal Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_ext_inflation_nominal

# Real Debt

fig_dom_inflation_real <- dataset_total %>%
  group_by(country) %>% 
  filter(develop == "EM") %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T),
         mean_delta_real = mean(delta_real, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_delta_real, y = mean_inflation), alpha = .2) +
  stat_cor(aes(x = mean_delta_real, y = mean_inflation, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = mean_delta_real, y = mean_inflation, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Domestic Share of Real Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_inflation_real

 ## Only Brazil

fig_dom_inflation_real_brazil <- dataset_brazil %>%
  ggplot() +
  geom_point(aes(x = delta_real, y = inflation_end), alpha = .2) +
  stat_cor(aes(x = delta_real, y = inflation_end, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = delta_real, y = inflation_end, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Nominal Domestic Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_inflation_real_brazil

 ## Only Argentina

fig_dom_inflation_real_argentina <- dataset_argentina %>%
  ggplot() +
  geom_point(aes(x = delta_real, y = inflation_end), alpha = .2) +
  stat_cor(aes(x = delta_real, y = inflation_end, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = delta_real, y = inflation_end, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Nominal Domestic Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_inflation_real_argentina

# External Debt x Inflation - Only Real Debt


fig_ext_inflation_real <- dataset_total %>%
  group_by(country) %>% 
  filter(develop == "EM") %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T),
         mean_delta_real = mean(delta_real, na.rm = T),
         mean_one_delta_nom = mean(one_delta_nom, na.rm = T),
         mean_one_delta_real = mean(one_delta_real, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_one_delta_real, y = mean_inflation), alpha = .2) +
  stat_cor(aes(x = mean_one_delta_real, y = mean_inflation, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = mean_one_delta_real, y = mean_inflation, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average External Share of Real Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_ext_inflation_real

# Inflation vs LC Debt (x)

fig_nominal_inflation_total <- dataset_total %>%
  group_by(country) %>% 
  filter(develop == "EM") %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T),
         mean_delta_real = mean(delta_real, na.rm = T),
         mean_x = mean(x, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_x, y = mean_inflation), alpha = .2) +
  stat_cor(aes(x = mean_x, y = mean_inflation, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = mean_x, y = mean_inflation, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Nominal Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_nominal_inflation_total

## Only Brazil

fig_nominal_inflation_total_brazil <- dataset_brazil %>%
  ggplot() +
  geom_point(aes(x = x, y = inflation_end), alpha = .2) +
  stat_cor(aes(x = x, y = inflation_end, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = x, y = inflation_end, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Nominal Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_nominal_inflation_total_brazil

## Only Argentina

fig_nominal_inflation_total_argentina <- dataset_argentina %>%
  ggplot() +
  geom_point(aes(x = x, y = inflation_end), alpha = .2) +
  stat_cor(aes(x = x, y = inflation_end, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = x, y = inflation_end, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Nominal Share of Debt Holdings", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_nominal_inflation_total_argentina


# Debt to GDP vs Ownership 

fig_dom_debt_total <- dataset_total %>%
  group_by(country) %>% 
  filter(develop == "EM") %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T),
         mean_delta_real = mean(delta_real, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_delta, y = mean_indebt), alpha = .2) +
  stat_cor(aes(x = mean_delta, y = mean_indebt, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = mean_delta, y = mean_indebt, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Domestic Share of Debt Holdings", y = "Mean Debt to GDP Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_dom_debt_total


# Debt to GDP vs Denomination

fig_nominal_debt_total <- dataset_total %>%
  group_by(country) %>% 
  filter(develop == "EM") %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_delta_nom = mean(delta_nom, na.rm = T),
         mean_delta_real = mean(delta_real, na.rm = T),
         mean_x = mean(x, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_x, y = mean_indebt), alpha = .2) +
  stat_cor(aes(x = mean_x, y = mean_indebt, p.accuracy = 0.001,r.accuracy = 0.01)) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  geom_smooth(aes(x = mean_x, y = mean_indebt, method = 'lm')) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Average Nominal Share of Debt Holdings", y = "Mean Debt to GDP Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

fig_nominal_debt_total

# GDP and LC debt

dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_x = mean(x, na.rm = T),
         mean_alpha = mean(alpha, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_x, y = mean_inflation, colour = develop), alpha = .2) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "LC Share of Debt Holdings (x)", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")

# Inflation vs Domestic LC Debt (x)

dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         mean_delta = mean(delta, na.rm = T),
         mean_x = mean(x, na.rm = T),
         mean_alpha = mean(alpha, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = 1 - mean_alpha, y = mean_inflation, colour = develop), alpha = .2) +
  scale_color_manual(values = c("navyblue", "red4")) +
  #scale_shape_manual(values=c(1,2)) +
  guides(col=guide_legend("")) +
  theme_gray() +
  labs(x = "Domestic LC Share of Debt Holdings (1 - alpha)", y = "Mean Inflation Between 2004-2019 (%)") +
  theme(text = element_text(size = 15),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)),
        legend.position = "bottom")




################################################ FUNDAMENTALS ################################################################################################################

# 5.1
dataset_total %>%
  filter(develop == "AM") %>%
  ggplot(aes(x = fx_volatility, y = foreign_participation_percent_GDP)) +
  geom_point(color="navyblue")+
  labs(x = "Exchange Rate Volatility", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  xlim(0,1)+
  theme_bw()

# 5.2
dataset_total %>%
  filter(develop == "EM" ) %>%
  ggplot(aes(x = fx_volatility, y = foreign_participation_percent_GDP)) +
  geom_point(color="red4")+
  labs(x = "Exchange Rate Volatility", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  xlim(0,1)+
  theme_bw()

# 5.3 Mostra a relação da inflação com a participação na dívida e o tamanho da bolinha é o PIB percapita (note que ele vai diminuindo)
dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_inflation, y = foreign_participation_percent_GDP, colour = develop,
                 size = GDP_percapita_cur_USD), alpha = .2) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  labs(x = "Mean Inflation Between 2004-2019 (%)", y = "Foreign Participation in Sovereign\n Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  guides(col=guide_legend(""),
         size=guide_legend("GDP per capita \n(thousand USD)")) +
  theme_light() +
  theme(axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))

# 5.4 Mesmo que no de cima, mas colorindo por país
dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(x = mean_inflation, y = foreign_participation_percent_GDP, colour = country,
                 size = political_stability_rank), alpha = .5)+
  #labs(x = "ln(GDP per capita USD)", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  #scale_color_viridis_d("magma") +
  theme_light() +
  theme(legend.position = "none")

# 5.5 Relação entre volatilidade do crescimento real e a participação
dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         GDP_growth = (GDP_cte_billions - lag(GDP_cte_billions, k = 1))/lag(GDP_cte_billions, k = 1),
         sd_GDP_growth = sd(GDP_growth, na.rm = T),
         mean_share = mean(foreign_participation_percent_GDP)) %>%
  ggplot() +
  geom_label(aes(x = sd_GDP_growth, y = mean_share, colour = develop,
                 size = mean_indebt, label = country), alpha = .3) +
  #labs(x = "ln(GDP per capita USD)", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  theme_light() +
  facet_wrap(~develop) +
  theme(legend.position = "none")

# 5.6 Relação entre média do crescimento real e a participação - sensibilizando por Rule of Law
dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         GDP_growth = (GDP_cte_billions - lag(GDP_cte_billions, k = 1))/lag(GDP_cte_billions, k = 1),
         mean_GDP_growth = mean(GDP_growth, na.rm = T),
         sd_GDP_growth = sd(GDP_growth, na.rm = T),
         mean_rule = mean(rule_of_law_rank, na.rm = T),
         mean_share_ex_off = mean(foreign_ex_officials_participation_percent_GDP)) %>%
  ggplot() +
  geom_label(aes(x = mean_GDP_growth, y = mean_share_ex_off, colour = develop,
                       size = mean_rule, label = country), alpha = .3) +
  guides(size = guide_legend("Rule of\nLaw"),
         colour = FALSE) +
  labs(x = "Mean GDP Growth Between 2004-2019 (%)", y = "Mean Foreign Participation in\nSovereign Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  theme_light() +
  theme(axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8))) +
  facet_wrap(~develop)

# 5.7 Relação entre volatilidade do cambio e a participação - sensibilizando por debt-to-gdp
dataset_5.7_int <- dataset_total %>%
  filter(country == "United States") %>%
  group_by() %>% 
  select(year, inflation_end) %>% 
  rename(US_inflation_rate = inflation_end)

dataset_5.7 <- dataset_total %>% 
  left_join(dataset_5.7_int, by = "year")


dataset_5.7 %>%
  group_by(country) %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         GDP_growth = (GDP_cte_billions - lag(GDP_cte_billions, k = 1))/lag(GDP_cte_billions, k = 1),
         mean_GDP_growth = mean(GDP_growth, na.rm = T),
         sd_GDP_growth = sd(GDP_growth, na.rm = T),
         x = US_inflation_rate/inflation_end,
         fx_vol_real = mean(x, na.rm = T),
         mean_share_ex_off = mean(foreign_ex_officials_participation_percent_GDP)) %>%
  ggplot() +
  geom_point(aes(x = log(fx_volatility*10000), y = foreign_participation_percent_GDP, colour = develop,
                 size = debt_to_GDP), alpha = .3) +
  #labs(x = "ln(GDP per capita USD)", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  theme_light() #+
  facet_wrap(~develop) +
  theme(legend.position = "none")

# 5.8 Níveis de inflação dividindo por desenvolvimento
x_order <- c("From -1 to 2.5", "From 2.5 to 5", "From 5 to 7.5", "From 7.5 to 10", "From 10 to 12.5",
             "From 12.5 to 15", "15 +")  

dataset_total %>%
  mutate(inflation_level = ifelse(inflation_end > - 1 & inflation_end <= 2.5, "From -1 to 2.5",
                                  ifelse(inflation_end < 5, "From 2.5 to 5",
                                         ifelse(inflation_end < 7.5, "From 5 to 7.5",
                                                ifelse(inflation_end < 10, "From 7.5 to 10",
                                                       ifelse(inflation_end < 12.5, "From 10 to 12.5",
                                                              ifelse(inflation_end < 15, "From 12.5 to 15", "15 +"))))))) %>%
  filter(!is.na(inflation_level)) %>% 
  ggplot() +
  geom_violin(aes(factor(inflation_level, levels = x_order), foreign_participation_percent_GDP, fill = develop,
                  colour = develop), trim = T) +
  geom_vline(xintercept = 1.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 2.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 3.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 4.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 5.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 6.5, 
             color = "black", size = .6) +
  scale_color_manual(values = c("navyblue", "red4")) +
  scale_fill_manual(values = c("navyblue", "red4")) +
  guides(fill = guide_legend(""),
         color = guide_legend("")) +
  theme_light() +
  labs(x = "Inflation Level (%)", y = "Foreign Participation in Sovereign\n Debt in Terms of GDP (%)") + 
  theme(axis.line.x = element_line(colour = "black", size = .6),
        axis.line.y = element_line(colour = "black", size = .6),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))

# 5.8 Níveis de inflação
x_order <- c("From -1 to 2.5", "From 2.5 to 5", "From 5 to 7.5", "From 7.5 to 10", "From 10 to 12.5",
             "From 12.5 to 15", "15 +")  

dataset_total %>%
  mutate(inflation_level = ifelse(inflation_end > - 1 & inflation_end <= 2.5, "From -1 to 2.5",
                                  ifelse(inflation_end < 5, "From 2.5 to 5",
                                         ifelse(inflation_end < 7.5, "From 5 to 7.5",
                                                ifelse(inflation_end < 10, "From 7.5 to 10",
                                                       ifelse(inflation_end < 12.5, "From 10 to 12.5",
                                                              ifelse(inflation_end < 15, "From 12.5 to 15", "15 +"))))))) %>%
  filter(!is.na(inflation_level)) %>% 
  ggplot() +
  geom_violin(aes(factor(inflation_level, levels = x_order), foreign_participation_percent_GDP), fill = "black") +
  scale_color_manual(values = c("navyblue", "red4")) +
  scale_fill_manual(values = c("navyblue", "red4")) +
  theme_light() +
  xlab("Inflation Levels (%)")



# 5.9 Relação entre volatilidade do cambio e a participação - sensibilizando por rule of law
dataset_5.9_int <- dataset_total %>%
  filter(country == "United States") %>%
  group_by() %>% 
  select(year, inflation_end) %>% 
  rename(US_inflation_rate = inflation_end)

dataset_5.9 <- dataset_total %>% 
  left_join(dataset_5.9_int, by = "year")


dataset_5.9 %>%
  group_by(country) %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         GDP_growth = (GDP_cte_billions - lag(GDP_cte_billions, k = 1))/lag(GDP_cte_billions, k = 1),
         mean_GDP_growth = mean(GDP_growth, na.rm = T),
         sd_GDP_growth = sd(GDP_growth, na.rm = T),
         x = US_inflation_rate/inflation_end,
         fx_vol_real = mean(x, na.rm = T),
         mean_share_ex_off = mean(foreign_ex_officials_participation_percent_GDP)) %>%
  ggplot() +
  geom_point(aes(x = log(fx_volatility*10000), y = foreign_participation_percent_GDP, colour = develop,
                 size = rule_of_law_rank), alpha = .4) +
  labs(x = "FX Volatility*", y = "Foreign Participation in Sovereign\n Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  guides(col=guide_legend(""),
         size=guide_legend("Rule of\nLaw Rank")) +
  theme_light() +
  theme(axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))
  

################################## EXTERNAL FACTORS ##############################################################
  
  
dataset_total2 <- dataset_total %>% 
select(vix_EUA, foreign_participation_percent_GDP, year, develop, country) 

dataset_total2 <- dataset_total2 %>% 
group_by(year) %>% 
mutate(VIX = mean(vix_EUA))

dataset_total2 <- dataset_total2 %>% 
group_by(year, develop) %>% 
mutate(foreign_GDP = mean(foreign_participation_percent_GDP))

dataset_total2 <- dataset_total2 %>% 
  select(3,4,6,7)

dataset_total2 <- dataset_total2 %>% 
  distinct()


dataset3 <- dataset_total2 

dataset3 <- dataset3 %>% 
  mutate(foreign_AM = ifelse(develop == "AM", foreign_GDP, 0))

dataset3 <- dataset3 %>% 
  filter(foreign_AM != 0)


dataset4 <- dataset_total2

dataset4 <- dataset4 %>% 
  mutate(foreign_EM = ifelse(develop =="EM", foreign_GDP, 0))

dataset4 <- dataset4 %>% 
  filter(foreign_EM != 0)

dataset3 <- dataset3 %>% 
  select(1,3,5)

dataset4 <- dataset4 %>% 
  select(1,3,5)

dataset4 <- dataset4 %>% 
  left_join(dataset3, by="year")

dataset4 <- dataset4 %>%
  select(2,3,4,7)

dataset4 <- dataset4 %>% 
  rename(VIX = 2, 
          AM = 4,
          EM = 3 ) %>% 
          mutate(VIX = (VIX/100))

dataset4 <- dataset4 %>%
  select(year, VIX, AM, EM) %>%
  gather(key = "variable", value = "value", -year)


ggplot(dataset4, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("navyblue", "red4" , "black")) +
  labs(x = "Year", y = "", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004, 2019, by=2), name = "Year") +
  scale_y_continuous(breaks=NULL) +
  theme_bw()

# Another way to display the same graph:
dataset5 <- dataset_total %>% 
  select(year, vix_EUA) %>% 
  filter(row_number() <= 16) %>% 
  rename(value = vix_EUA) %>% 
  mutate(variable = "US VIX")

dataset6 <- dataset_total %>%
  filter(!is.na(foreign_participation_percent_GDP)) %>% 
  group_by(year, develop) %>%
  summarise(foreign_GDP = mean(foreign_participation_percent_GDP), year = year) %>%
  select(c(year, foreign_GDP)) %>% 
  distinct() %>% 
  rename(variable = develop, value = foreign_GDP)

p1 <- dataset5 %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, colour = variable), size = 1) +
  scale_color_manual(values = c("black")) +
  labs(x = "", y = "US VIX", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004, 2019, by=2), name = "Year") +
  #scale_y_continuous(breaks=NULL) +
  theme_light() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.border = element_blank(),
        plot.caption = element_blank(),
        axis.text.y = element_text(margin = margin(l = 8)))
  
(p2 <- dataset6 %>% 
  ggplot(aes(x = year, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("navyblue", "red4")) +
  labs(x = "Year", y = "Foreign Participation in \nSovereign Debt (% of GDP)", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2003, 2020), seq(2004, 2019, by = 2), name = "Year") +
  #scale_y_continuous(breaks=NULL) +
  theme_light() +
  theme(legend.title = element_blank(),
        plot.title = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.border = element_blank(),
        plot.subtitle = element_blank(),
        axis.text.y = element_text(margin = margin(l = 8))))

grid.newpage()
grid.draw(rbind(ggplotGrob(p1),
                ggplotGrob(p2),
                size = "last"))



#DXY

# 3.
dataset7 <- dataset_total %>% 
  select(year, dxy) %>% 
  filter(row_number() <= 16) %>% 
  rename(value = dxy) %>% 
  mutate(variable = "DXY")

dataset8 <- dataset_total %>%
  filter(!is.na(foreign_participation_percent_GDP)) %>% 
  group_by(year, develop) %>%
  summarise(foreign_GDP = mean(foreign_participation_percent_GDP), year = year) %>%
  select(c(year, foreign_GDP)) %>% 
  distinct() %>% 
  rename(variable = develop, value = foreign_GDP)

p3 <- dataset7 %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, colour = variable), size = 1) +
  scale_color_manual(values = c("black")) +
  labs(x = "", y = "DXY", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004, 2019, by=2), name = "Year") +
  #scale_y_continuous(breaks=NULL) +
  theme_light() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.border = element_blank(),
        plot.caption = element_blank(),
        axis.text.y = element_text(margin = margin(l = 8)))

(p4 <- dataset8 %>% 
    ggplot(aes(x = year, y = value, fill = variable)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("navyblue", "red4")) +
    labs(x = "Year", y = "Foreign Participation in \nSovereign Debt (% of GDP)", title = "", subtitle = "") +
    scale_x_continuous(limits = c(2003, 2020), seq(2004, 2019, by = 2), name = "Year") +
    #scale_y_continuous(breaks=NULL) +
    theme_light() +
    theme(legend.title = element_blank(),
          plot.title = element_blank(),
          axis.line.x = element_line(color = "black"),
          panel.border = element_blank(),
          plot.subtitle = element_blank(),
          axis.text.y = element_text(margin = margin(l = 8))))

grid.newpage()
grid.draw(rbind(ggplotGrob(p3),
                ggplotGrob(p4),
                size = "last"))

